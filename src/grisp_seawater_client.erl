-module(grisp_seawater_client).

-export([start_link/0]).
-export([connect/0]).
-export([connect/2]).
-export([link_device/1]).
-export([ping/0]).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
    http_conn,
    ws_stream,
    requests = #{}
}).

-define(request_timeout, 5_000).

-include_lib("kernel/include/logger.hrl").

%--- API Functions -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    connect("seawater.stritzinger.com", 443).

connect(Server, Port) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Server, Port}, 60_000).

link_device(Token) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Token}).

ping() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

% gen_server callbacks -----------------------------

init([]) ->
    {ok, #state{}}.

handle_call({connect, Server, Port}, _, #state{http_conn = undefined} = S) ->
    case grisp_seawater_http:open(Server, Port) of
        {ok, Conn} ->
            case upgrade(Conn) of
                {ok, WsStream} ->
                    {reply, ok, #state{http_conn = Conn, ws_stream = WsStream}};
                Error ->
                    ok = grisp_seawater_http:close(Conn),
                    {reply, Error, S}
            end;
        Error ->
            {reply, Error, S}
    end;
handle_call({connect, _Server, _Port}, _,  S) ->
    {reply, already_connected, S};
handle_call(_, _,  #state{http_conn = C, ws_stream = Stream} = S)
when  C == undefined orelse Stream == undefined ->
    {reply, {error, disconnected}, S};
handle_call(ping, From, S) ->
    {ok, NewS} = make_request(From, post, ping, #{}, S),
    {noreply, NewS};
handle_call({link_device, Token}, From, S) ->
    {ok, NewS} = make_request(From, post, device_linking_token, #{token => Token}, S),
    {noreply, NewS}.

handle_cast(connect, S) ->
    {noreply, S}.

handle_info({gun_ws, Conn, Stream, {text, JSON}},
            #state{http_conn = Conn, ws_stream = Stream}= S) ->
    JSON_RPC = grisp_seawater_jsonrpc:decode(JSON),
    case handle_jsonrpc(JSON_RPC, S) of
        {[], NewS} -> {noreply, NewS};
        {Msgs, NewS} ->
            Text = grisp_seawater_jsonrpc:encode(Msgs),
            gun:ws_send(S#state.http_conn, S#state.ws_stream, {text, Text}),
            {noreply, NewS}
    end;
handle_info({timeout, TRef, ID}, #state{requests = Reqs} = State) ->
    case maps:get(ID, Reqs, undefined) of
        {Caller, TRef} ->
            NewS = State#state{requests = maps:remove(ID, Reqs)},
            gen_server:reply(Caller, {error, timeout}),
            {noreply, NewS};
        _ -> error(unexpected_timeout)
    end;
handle_info({gun_down, C, ws,closed, [Stream]},
            #state{http_conn = C, ws_stream = Stream}) ->
    grisp_seawater_http:close(C),
    {noreply, #state{}};
handle_info(M, S) ->
    ?LOG_WARNING("Unandled WS message: ~p", [M]),
    {noreply, S}.

% internal functions -----------------------------

upgrade(Conn) ->
    StreamRef = gun:ws_upgrade(Conn, "/grisp-connect/ws"),
    receive
    {gun_upgrade, Conn, StreamRef, [<<"websocket">>], _Headers} ->
        {ok, StreamRef};
    {gun_response, Conn, _, _, Status, Headers} ->
        {ws_upgrade_failed, Status, Headers};
    {gun_error, Conn, StreamRef, Reason} ->
        {ws_upgrade_failed, Reason}
    after 1000 ->
        timeout
    end.

handle_jsonrpc({batch, Batch}, S) ->
    handle_rpc_messages(Batch, [], S);
handle_jsonrpc({single, Rpc}, S) ->
    handle_rpc_messages([Rpc], [], S).

handle_rpc_messages([], Replies , S) -> {lists:reverse(Replies), S};
handle_rpc_messages([{request, M, Params, ID} | Batch], Replies , S)
        when M == <<"post">> ->
    handle_rpc_messages(Batch, [handle_request(M, Params, ID) | Replies], S);
handle_rpc_messages([{result, _, _} = Res| Batch], Replies, S) ->
    handle_rpc_messages(Batch, Replies, handle_result(Res, S));
handle_rpc_messages([{error, _Code, _Msg, _Data, _ID} = E | Batch], Replies, S) ->
    ?LOG_DEBUG("Received JsonRPC error: ~p",[E]),
    handle_rpc_messages(Batch, Replies, S);
handle_rpc_messages([{internal_error, _, _} = E | Batch], Replies, S) ->
    ?LOG_ERROR("JsonRPC: ~p",[E]),
    handle_rpc_messages(Batch, [grisp_seawater_jsonrpc:format_error(E)| Replies], S).

handle_request(<<"post">>, #{type := <<"flash">>} = Params, ID) ->
    Led = maps:get(led, Params, 1),
    Color = maps:get(color, Params, red),
    {result, flash(Led, Color), ID};
handle_request(_, _, ID) ->
    grisp_seawater_jsonrpc:format_error({internal_error, method_not_found, ID}).

make_request(Caller, Method, Type, Params, #state{requests = Reqs} = State) ->
    ID = list_to_binary(integer_to_list(erlang:unique_integer())),
    Rpc = {request, Method, maps:put(type, Type, Params), ID},
    Msg = grisp_seawater_jsonrpc:encode(Rpc),
    gun:ws_send(State#state.http_conn, State#state.ws_stream, {text, Msg}),
    TRef = erlang:start_timer(?request_timeout, self(), ID),
    Request = {Caller, TRef},
    {ok, State#state{requests = Reqs#{ID => Request}}}.

handle_result({result, R, ID} = Res, #state{requests = Requests} = S) ->
    case maps:get(ID, Requests) of
        {Caller, Tref} ->
            erlang:cancel_timer(Tref),
            gen_server:reply(Caller, R);
        _ ->
            ?LOG_ERROR("Unexpected jsonrpc ~p",[Res])
    end,
    S#state{requests = maps:remove(ID, Requests)}.

flash(Led, Color) ->
    spawn(fun() ->
        io:format("Flash from Seawater!~n"),
        grisp_led:color(Led, Color),
        timer:sleep(100),
        grisp_led:off(Led)
    end),
    ok.

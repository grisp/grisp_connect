%% @doc Websocket client to connect to grisp.io
-module(grisp_seawater_ws).

-export([start_link/0]).
-export([connect/0]).
-export([connect/2]).
-export([is_connected/0]).
-export([link_device/0]).
-export([link_device/1]).
-export([ping/0]).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
    gun_pid,
    gun_ref,
    ws_stream,
    ws_up = false,
    requests = #{}
}).

-define(request_timeout, 5_000).
-define(call_timeout, ?request_timeout + 1000).
-define(disconnected_state,
        #state{gun_pid = undefined, gun_ref = undefine, ws_up = false}).

-include_lib("kernel/include/logger.hrl").

%--- API Functions -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    {ok, Domain} = application:get_env(grisp_seawater, seawater_domain),
    {ok, Port} = application:get_env(grisp_seawater, seawater_port),
    connect(Domain, Port).

connect(Server, Port) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Server, Port}).

is_connected() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

link_device() ->
    case application:get_env(grisp_seawater, device_linking_token) of
        undefined -> {error, token_undefined};
        {ok, Token} -> link_device(Token)
    end.

link_device(Token) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Token}, ?call_timeout).

ping() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME, ?call_timeout).

% gen_server callbacks ---------------------------------------------------------

init([]) -> {ok, #state{}}.

handle_call(is_connected, _, #state{ws_up = Up} = S) ->
    {reply, Up, S};
handle_call(_, _, #state{ws_up = false} = S) ->
    {reply, {error, disconnected}, S};
handle_call(ping, From, S) ->
    {ok, NewS} = make_request(From, post, ping, #{}, S),
    {noreply, NewS};
handle_call({link_device, Token}, From, S) ->
    {ok, NewS} = make_request(From, post, device_linking_token,
                              #{token => Token}, S),
    {noreply, NewS}.

handle_cast({connect, Server, Port}, #state{gun_pid = undefined} = S) ->
    case grisp_seawater_tls:connect(Server, Port) of
        {ok, GunPid} ->
            {noreply, #state{gun_pid = GunPid}};
        Error ->
            grisp_io_client ! {connection_fail, Error},
            {noreply, S}
    end;
handle_cast({connect, _Server, _Port}, S) ->
    {noreply, S}.

handle_info({gun_up, GunPid, _}, #state{gun_pid = GunPid} = S) ->
    ?LOG_INFO("HTTP connection enstablished, upgrading to WS..."),
    GunRef = monitor(process, GunPid),
    WsStream = gun:ws_upgrade(GunPid, "/grisp-connect/ws"),
    NewState = S#state{gun_pid = GunPid, gun_ref = GunRef, ws_stream = WsStream},
    {noreply, NewState};
handle_info({gun_upgrade, Pid, Stream, [<<"websocket">>], _}, 
            #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_INFO("WS Upgraded"),
    {noreply, S#state{ws_up = true}};
handle_info({gun_response, Pid, Stream, _, Status, Headers},
            #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_ERROR("WS Upgrade fail with status ~p", [Status]),
    grisp_io_client ! {connection_fail, upgrade},
    {noreply, shutdown_gun(S)};
handle_info({gun_ws, Conn, Stream, {text, JSON}},
            #state{gun_pid = Conn, ws_stream = Stream}= S) ->
    JSON_RPC = grisp_seawater_jsonrpc:decode(JSON),
    case handle_jsonrpc(JSON_RPC, S) of
        {[], NewS} -> {noreply, NewS};
        {Msgs, NewS} ->
            Text = grisp_seawater_jsonrpc:encode(Msgs),
            gun:ws_send(S#state.gun_pid, S#state.ws_stream, {text, Text}),
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
handle_info({gun_down, Pid, ws, closed, [Stream]},
            #state{gun_pid = Pid, ws_stream = Stream, requests = Requests} = S) ->
    ?LOG_WARNING("Websocket down!"),
    [begin
        erlang:cancel_timer(Tref),
        gen_server:reply(Caller, {error, ws_closed})
     end || {Caller, Tref} <- maps:values(Requests)],
    {noreply, shutdown_gun(S#state{requests = #{}})};
handle_info(M, S) ->
    ?LOG_WARNING("Unandled WS message: ~p", [M]),
    {noreply, S}.

% internal functions -----------------------------

handle_jsonrpc({batch, Batch}, S) ->
    handle_rpc_messages(Batch, [], S);
handle_jsonrpc({single, Rpc}, S) ->
    handle_rpc_messages([Rpc], [], S).

handle_rpc_messages([], Replies , S) -> {lists:reverse(Replies), S};
handle_rpc_messages([{request, M, Params, ID} | Batch], Replies , S)
when M == <<"post">> ->
    handle_rpc_messages(Batch, [handle_request(M, Params, ID) | Replies], S);
handle_rpc_messages([{result, _, _} = Res| Batch], Replies, S) ->
    handle_rpc_messages(Batch, Replies, handle_response(Res, S));
handle_rpc_messages([{error, _Code, _Msg, _Data, _ID} = E | Batch],
                    Replies, S) ->
    ?LOG_INFO("Received JsonRPC error: ~p",[E]),
    handle_rpc_messages(Batch, Replies, handle_response(E, S));
handle_rpc_messages([{internal_error, _, _} = E | Batch], Replies, S) ->
    ?LOG_ERROR("JsonRPC: ~p",[E]),
    handle_rpc_messages(Batch,
                        [grisp_seawater_jsonrpc:format_error(E)| Replies], S).

handle_request(<<"post">>, #{type := <<"flash">>} = Params, ID) ->
    Led = maps:get(led, Params, 1),
    Color = maps:get(color, Params, red),
    {result, flash(Led, Color), ID};
handle_request(_, _, ID) ->
    grisp_seawater_jsonrpc:format_error({internal_error, method_not_found, ID}).

make_request(Caller, Method, Type, Params, #state{requests = Reqs} = State) ->
    ID = id(),
    Rpc = {request, Method, maps:put(type, Type, Params), ID},
    Msg = grisp_seawater_jsonrpc:encode(Rpc),
    gun:ws_send(State#state.gun_pid, State#state.ws_stream, {text, Msg}),
    TRef = erlang:start_timer(?request_timeout, self(), ID),
    Request = {Caller, TRef},
    {ok, State#state{requests = Reqs#{ID => Request}}}.

handle_response(Response, #state{requests = Requests} = S) ->
    {Reply, ID} = case Response of
        {result, Result, ID0} -> {{ok, Result}, ID0};
        {error, Code, Message, Data, ID0} ->
            {{error, error_atom(Code), Message, Data}, ID0}
    end,
    case maps:get(ID, Requests, undefined) of
        {Caller, Tref} ->
            erlang:cancel_timer(Tref),
            gen_server:reply(Caller, Reply);
        undefined ->
            ?LOG_ERROR("Unexpected jsonrpc ~p",[Response])
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

error_atom(-1) -> device_not_linked;
error_atom(-2) -> token_expired;
error_atom(-3) -> device_already_linked;
error_atom(-4) -> invalid_token;
error_atom(_)  -> jsonrpc_error.

id() -> 
    list_to_binary(integer_to_list(erlang:unique_integer())).

shutdown_gun(#state{gun_pid = Pid} = State) ->
    gun:shutdown(Pid),
    grisp_io_client ! disconnected,
    State?disconnected_state.

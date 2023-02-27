-module(grisp_seawater_client).

-export([start_link/0]).
-export([connect/0]).
-export([connect/2]).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
    http_conn,
    ws_stream
}).


-include_lib("kernel/include/logger.hrl").

%--- API Functions -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    connect("seawater.stritzinger.com", 443).

connect(Server, Port) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Server, Port}).


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
    {reply, already_connected, S}.

handle_cast(connect, S) ->
    {noreply, S}.

handle_info({gun_ws, Conn, Stream, {text, JSON}},
            #state{http_conn = Conn, ws_stream = Stream}= S) ->
    JSON_RPC = grisp_seawater_jsonrpc:decode(JSON),
    case handle_jsonrpc(JSON_RPC) of
        [] -> ok;
        Msgs ->
            Text = grisp_seawater_jsonrpc:encode(Msgs),
            gun:ws_send(S#state.http_conn, S#state.ws_stream, {text, Text})
    end,
    {noreply, S};
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

handle_jsonrpc({batch, Batch}) ->
    handle_rpc_messages(Batch, []);
handle_jsonrpc({single, Rpc}) ->
    handle_rpc_messages([Rpc], []).

handle_rpc_messages([], Replies) -> lists:reverse(Replies);
handle_rpc_messages([{request, M, Params, ID} | Batch], Replies)
        when M == <<"post">> ->
    handle_rpc_messages(Batch, [handle_request(M, Params, ID) | Replies]);
handle_rpc_messages([{error, _Code, _Msg, _Data, _ID} = E | Batch], Replies) ->
    ?LOG_DEBUG("Received JsonRPC error: ~p",[E]),
    handle_rpc_messages(Batch, Replies);
handle_rpc_messages([{internal_error, _, _} = E | Batch], Replies) ->
    ?LOG_ERROR("JsonRPC: ~p",[E]),
    handle_rpc_messages(Batch, [grisp_seawater_jsonrpc:format_error(E)| Replies]).

handle_request(<<"post">>, #{type := <<"flash">>}, ID) ->
    {result, flash(), ID};
handle_request(_, _, ID) ->
    grisp_seawater_jsonrpc:format_error({internal_error, method_not_found, ID}).

flash() ->
    io:format("Flash from Seawater!~n"),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    ok.

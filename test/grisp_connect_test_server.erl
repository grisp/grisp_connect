-module(grisp_connect_test_server).

% API
-export([start/1]).
-export([start_cowboy/1]).
-export([stop_cowboy/0]).
-export([close_websocket/0]).
-export([listen/0]).
-export([flush/0]).
-export([receive_text/0]).
-export([receive_jsonrpc/0]).
-export([receive_jsonrpc_request/0]).
-export([send_text/1]).
-export([send_jsonrpc_result/2]).
-export([send_jsonrpc_error/3]).

% Websocket Callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-behaviour(cowboy_websocket).

%--- API -----------------------------------------------------------------------

% Start the cowboy application and server,
% call this in `init_per_suite'.
% Returns the started apps
start(CertDir) ->
    {ok, Apps} = application:ensure_all_started(cowboy),
    % TODO: Disable ssl for testing
    start_cowboy(CertDir),
    Apps.

% Start the cowboy listener.
% You have to make sure cowboy is running before.
start_cowboy(CertDir) ->
    SslOpts = [
        {verify, verify_peer},
        {keyfile, filename:join(CertDir, "server.key")},
        {certfile, filename:join(CertDir, "server.crt")},
        {cacertfile, filename:join(CertDir, "CA.crt")}
    ],
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{"/grisp-connect/ws", grisp_connect_test_server, []}]}]),
    {ok, _} = cowboy:start_tls(server_listener,
        [{port, 3030} | SslOpts],
        #{env => #{dispatch => Dispatch}}
    ).

% Stop the cowboy listener.
stop_cowboy() ->
    cowboy:stop_listener(server_listener).

% Close the websocket.
close_websocket() ->
    ?MODULE ! ?FUNCTION_NAME.

% Listen to websocket messages.
% Call this in `init_per_testcase' after connecting to the websocket
listen() ->
    ?MODULE ! {?FUNCTION_NAME, self()},
    receive ok -> ok
    after 5000 -> {error, timeout}
    end.

% Flush all messages.
% Call this in `end_per_testcase` to see what messages where missed.
% This is especially useful when test cases fail.
flush() -> flush([]).

flush(Acc) ->
    receive Any -> ct:pal("Flushed: ~p", [Any]), flush([Any | Acc])
    after 0 -> lists:reverse(Acc)
    end.

receive_text() ->
    receive {received_text, Msg} -> Msg
    after 5000 -> {error, timeout}
    end.

receive_jsonrpc() ->
    case receive_text() of
        {error, _} = Error -> Error;
        Msg -> check_jsonrpc(Msg)
    end.

receive_jsonrpc_request() ->
    case receive_jsonrpc() of
        {error, _} = Error -> Error;
        {error, _, _} = Error -> Error;
        #{id := _} = Decoded -> Decoded;
        Decoded -> {error, invalid_jsonrpc_request, Decoded}
    end.

check_jsonrpc(Msg) ->
    case jsx:decode(Msg, [{labels, attempt_atom}, return_maps]) of
        #{jsonrpc := <<"2.0">>} = Decoded -> Decoded;
        _ -> {error, invalid_jsonrpc, Msg}
    end.

send_text(Msg) ->
    ?MODULE ! {?FUNCTION_NAME, Msg}.

send_jsonrpc_result(Result, Id) ->
    Map = #{jsonrpc => <<"2.0">>,
            result => Result,
            id => Id},
    send_text(jsx:encode(Map)).

send_jsonrpc_error(Code, Msg, Id) ->
    Map = #{jsonrpc => <<"2.0">>,
            error => #{code => Code, message => Msg},
            id => Id},
    send_text(jsx:encode(Map)).

%--- Websocket Callbacks -------------------------------------------------------

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(_) ->
    register(?MODULE, self()),
    {[], []}.

websocket_handle({text, Msg}, State) ->
    ct:pal("Received websocket message:~n~s", [Msg]),
    [Pid ! {received_text, Msg} || Pid <- State],
    {[], State};
websocket_handle(Frame, State) ->
    ct:pal("Ignore websocket frame:~n~p", [Frame]),
    {[], State}.

websocket_info({listen, Pid}, State) ->
    Pid ! ok,
    {[], [Pid | State]};
websocket_info({send_text, Msg}, State) ->
    {[{text, Msg}], State};
websocket_info(close, State) ->
    {[close], State};
websocket_info(Info, State) ->
    ct:pal("Ignore websocket info:~n~p", [Info]),
    {[], State}.

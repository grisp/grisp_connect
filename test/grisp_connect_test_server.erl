-module(grisp_connect_test_server).

-include_lib("stdlib/include/assert.hrl").

% API
-export([start/0, start/1]).
-export([stop/1]).
-export([start_cowboy/0, start_cowboy/1]).
-export([stop_cowboy/0]).
-export([close_websocket/0]).
-export([listen/0]).
-export([flush/0]).
-export([receive_text/0, receive_text/1]).
-export([receive_jsonrpc/0, receive_jsonrpc/1]).
-export([receive_jsonrpc_request/0, receive_jsonrpc_request/1]).
-export([receive_jsonrpc_notification/0]).
-export([receive_jsonrpc_result/0]).
-export([receive_jsonrpc_error/0]).
-export([send_text/1]).
-export([send_jsonrpc_notification/2]).
-export([send_jsonrpc_request/3]).
-export([send_jsonrpc_result/2]).
-export([send_jsonrpc_error/3]).
-export([wait_disconnection/0]).

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
start() ->
    {ok, Apps} = application:ensure_all_started(cowboy),
    start_cowboy(#{}),
    Apps.

start(Opts) ->
    {ok, Apps} = application:ensure_all_started(cowboy),
    start_cowboy(Opts),
    Apps.

stop(Apps) ->
    ?assertEqual(ok, stop_cowboy()),
    [?assertEqual(ok, application:stop(App)) || App <- Apps],
    % Ensure the process is unregistered...
    wait_disconnection().


% Start the cowboy listener.
% You have to make sure cowboy is running before.
start_cowboy() ->
    start_cowboy(#{}).

start_cowboy(#{cert_dir := CertDir} = Opts) ->
    SslOpts = [
        {verify, verify_peer},
        {keyfile, filename:join(CertDir, "server.key")},
        {certfile, filename:join(CertDir, "server.crt")},
        {cacertfile, filename:join(CertDir, "CA.crt")}
    ],
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{"/grisp-connect/ws", grisp_connect_test_server, Opts}]}]),
    {ok, _} = cowboy:start_tls(server_listener,
        [{port, 3030} | SslOpts],
        #{env => #{dispatch => Dispatch}}
    );
start_cowboy(Opts) ->
    Dispatch = cowboy_router:compile(
        [{'_', [{"/grisp-connect/ws", grisp_connect_test_server, Opts}]}]),
    {ok, _} = cowboy:start_clear(server_listener, [{port, 3030}],
                                 #{env => #{dispatch => Dispatch}}).

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
    receive_text(5000).

receive_text(Timeout) ->
    receive {received_text, Msg} -> Msg
    after Timeout -> error(timeout)
    end.

receive_jsonrpc() ->
    check_jsonrpc(receive_text()).

receive_jsonrpc(Timeout) ->
    check_jsonrpc(receive_text(Timeout)).

receive_jsonrpc_request() ->
    case receive_jsonrpc() of
        #{id := _} = Decoded -> Decoded;
        Decoded -> error({invalid_jsonrpc_request, Decoded})
    end.

receive_jsonrpc_request(Timeout) ->
    case receive_jsonrpc(Timeout) of
        #{id := _} = Decoded -> Decoded;
        Decoded -> error({invalid_jsonrpc_request, Decoded})
    end.

receive_jsonrpc_notification() ->
    case receive_jsonrpc() of
        #{method := _} = Decoded -> Decoded;
        Decoded -> error({invalid_jsonrpc_notification, Decoded})
    end.

receive_jsonrpc_result() ->
    case receive_jsonrpc() of
        #{result := _} = Decoded -> Decoded;
        Decoded -> error({invalid_jsonrpc_result, Decoded})
    end.

receive_jsonrpc_error() ->
    case receive_jsonrpc() of
        #{error := _} = Decoded -> Decoded;
        Decoded -> error({invalid_jsonrpc_error, Decoded})
    end.

check_jsonrpc(Msg) ->
    case jsx:decode(Msg, [{labels, attempt_atom}, return_maps]) of
        #{jsonrpc := <<"2.0">>} = Decoded -> Decoded;
        Batch when is_list(Batch) ->
            lists:foreach(fun
                (#{jsonrpc := <<"2.0">>}) -> ok;
                (_) -> error({invalid_jsonrpc, Msg})
            end, Batch),
            Batch;
        _ -> error({invalid_jsonrpc, Msg})
    end.

send_text(Msg) ->
    ?MODULE ! {?FUNCTION_NAME, Msg}.

send_jsonrpc_notification(Method, Params) ->
    Map = #{jsonrpc => <<"2.0">>,
            method => Method,
            params => Params},
    send_text(jsx:encode(Map)).

send_jsonrpc_request(Method, Params, Id) ->
    Map = #{jsonrpc => <<"2.0">>,
            method => Method,
            params => Params,
            id => Id},
    send_text(jsx:encode(Map)).

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

wait_disconnection() ->
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid ->
            timer:sleep(200),
            wait_disconnection()
    end.


%--- Websocket Callbacks -------------------------------------------------------

init(Req, Opts = #{init_callback := Fun}) when is_function(Fun, 2) ->
    Fun(Req, Opts);
init(Req, Opts) ->
    ExpVer = maps:get(expected_protocol, Opts, <<"grisp-io-v1">>),
    SelVer = maps:get(selected_protocol, Opts, <<"grisp-io-v1">>),
    case cowboy_req:header(<<"sec-websocket-protocol">>, Req) of
        undefined ->
            Req2 = cowboy_req:reply(400, #{}, <<"No protocol specified">>, Req),
            {ok, Req2, Opts};
        ExpVer ->
            Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, SelVer, Req),
            {cowboy_websocket, Req2, Opts};
        _ ->
            Req2 = cowboy_req:reply(400, #{}, <<"Unsupported Protocol">>, Req),
            {ok, Req2, Opts}
    end.

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
websocket_info(close_websocket, State) ->
    {[close], State};
websocket_info(Info, State) ->
    ct:pal("Ignore websocket info:~n~p", [Info]),
    {[], State}.

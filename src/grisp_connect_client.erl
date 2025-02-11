%% @doc Client to interact with grisp.io
%%
%% This module contains a state machine to ensure connectivity with grisp.io.
%% JsonRPC traffic is managed here.
%% @end
-module(grisp_connect_client).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

% External API
-export([start_link/0]).
-export([connect/0]).
-export([is_connected/0]).
-export([request/3]).
-export([notify/3]).

% Internal API
-export([reboot/0]).

% Behaviour gen_statem callback functions
-export([init/1, terminate/3, code_change/4, callback_mode/0]).

% State Functions
-export([idle/3]).
-export([waiting_ip/3]).
-export([connecting/3]).
-export([connected/3]).


%--- Types ---------------------------------------------------------------------

-record(data, {
    domain :: binary(),
    port :: inet:port_number(),
    ws_path :: binary(),
    ws_transport :: tcp | tls,
    conn :: undefined | pid(),
    retry_count = 0 :: non_neg_integer()
}).

-type data() :: #data{}.
-type on_result_fun() :: fun((data(), Result :: term()) -> data()).
-type on_error_fun() :: fun((data(), local | remote,
                             Code :: atom() | integer(),
                             Message :: undefined | binary(),
                             Data :: term()) -> data()).


%--- Macros --------------------------------------------------------------------

-define(FORMAT(FMT, ARGS), iolist_to_binary(io_lib:format(FMT, ARGS))).
-define(STD_TIMEOUT, 1000).
-define(CONNECT_TIMEOUT, 5000).
-define(ENV(KEY, GUARDS), fun() ->
    case application:get_env(grisp_connect, KEY) of
        {ok, V} when GUARDS -> V;
        {ok, V} -> erlang:exit({invalid_env, KEY, V});
        undefined -> erlang:exit({missing_env, KEY})
    end
end()).
-define(ENV(KEY, GUARDS, CONV), fun() ->
    case application:get_env(grisp_connect, KEY) of
        {ok, V} when GUARDS -> CONV;
        {ok, V} -> erlang:exit({invalid_env, KEY, V});
        undefined -> erlang:exit({missing_env, KEY})
    end
end()).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(EventType, EventContent, Data) ->
        handle_common(EventType, EventContent, ?FUNCTION_NAME, Data)).


%--- External API Functions ----------------------------------------------------

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

is_connected() ->
    try gen_statem:call(?MODULE, ?FUNCTION_NAME)
    catch exit:noproc -> false
    end.

request(Method, Type, Params) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Method, Type, Params}).

notify(Method, Type, Params) ->
    gen_statem:cast(?MODULE, {?FUNCTION_NAME, Method, Type, Params}).


%--- Internal API Functions ----------------------------------------------------

reboot() ->
    erlang:send_after(1000, ?MODULE, reboot).


%--- Behaviour gen_statem Callback Functions -----------------------------------

init([]) ->
    process_flag(trap_exit, true),
    AutoConnect = ?ENV(connect, is_boolean(V)),
    Domain = ?ENV(domain, is_binary(V) orelse is_list(V) orelse is_atom(V), as_bin(V)),
    Port = ?ENV(port, is_integer(V) andalso V >= 0 andalso V < 65536),
    WsTransport = ?ENV(ws_transport, V =:= tls orelse V =:= tcp),
    WsPath = ?ENV(ws_path, is_binary(V) orelse is_list(V), as_bin(V)),
    Data = #data{
        domain = Domain,
        port = Port,
        ws_transport = WsTransport,
        ws_path = WsPath
    },
    % The error list is put in a persistent term to not add noise to the state.
    persistent_term:put({?MODULE, self()}, generic_errors()),
    NextState = case AutoConnect of
        true -> waiting_ip;
        false -> idle
    end,
    {ok, NextState, Data}.

terminate(Reason, _State, Data) ->
    conn_close(Data, Reason),
    persistent_term:erase({?MODULE, self()}),
    ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

callback_mode() -> [state_functions, state_enter].


%--- Behaviour gen_statem State Callback Functions -----------------------------

idle(enter, _OldState, _Data) ->
    keep_state_and_data;
idle(cast, connect, Data) ->
    {next_state, waiting_ip, Data};
?HANDLE_COMMON.

waiting_ip(enter, _OldState, Data) ->
    Delay = case Data#data.retry_count > 0 of
        true -> ?STD_TIMEOUT;
        false -> 0
    end,
    {keep_state_and_data, [{state_timeout, Delay, retry}]};
waiting_ip(state_timeout, retry, Data = #data{retry_count = RetryCount}) ->
    case check_inet_ipv4() of
        {ok, IP} ->
            ?LOG_INFO(#{event => checked_ip, ip => IP}),
            {next_state, connecting, Data};
        invalid ->
            ?LOG_DEBUG(#{event => waiting_ip}),
            {repeat_state, Data#data{retry_count = RetryCount + 1}}
    end;
?HANDLE_COMMON.

connecting(enter, _OldState, Data) ->
    {keep_state, Data, [{state_timeout, 0, connect}]};
connecting(state_timeout, connect,
           Data = #data{conn = undefined, retry_count = RetryCount}) ->
    ?LOG_INFO(#{description => <<"Connecting to grisp.io">>,
                event => connecting}),
    case conn_start(Data) of
        {ok, Data2} ->
            {keep_state, Data2, [{state_timeout, ?CONNECT_TIMEOUT, timeout}]};
        {error, Reason} ->
            ?LOG_WARNING("Failed to connect to grisp.io: ~p", [Reason],
                         #{event => connection_failed, reason => Reason}),
            {next_state, waiting_ip, Data#data{retry_count = RetryCount + 1}}
    end;
connecting(state_timeout, timeout, Data = #data{retry_count = RetryCount}) ->
    Reason = connect_timeout,
    ?LOG_WARNING(#{description => <<"Timeout while connecting to grisp.io">>,
                   event => connection_failed, reason => Reason}),
    Data2 = conn_close(Data, Reason),
    {next_state, waiting_ip, Data2#data{retry_count = RetryCount + 1}};
connecting(info, {jarl, Conn, connected}, Data = #data{conn = Conn}) ->
    % Received from the connection process
    ?LOG_NOTICE(#{description => <<"Connected to grisp.io">>,
                  event => connected}),
    {next_state, connected, Data#data{retry_count = 0}};
?HANDLE_COMMON.

connected(enter, _OldState, _Data) ->
    grisp_connect_log_server:start(),
    keep_state_and_data;
connected({call, From}, is_connected, _) ->
    {keep_state_and_data, [{reply, From, true}]};
connected(info, {jarl, Conn, Msg}, Data = #data{conn = Conn}) ->
    handle_connection_message(Data, Msg);
connected({call, From}, {request, Method, Type, Params}, Data) ->
    Data2 = conn_request(Data, Method, Type, Params,
                fun(D, R) -> gen_statem:reply(From, {ok, R}), D end,
                fun(D, _, C, _, _) -> gen_statem:reply(From, {error, C}), D end),
    {keep_state, Data2};
connected(cast, {notify, Method, Type, Params}, Data) ->
    conn_notify(Data, Method, Type, Params),
    keep_state_and_data;
?HANDLE_COMMON.

% Common event handling appended as last match case to each state_function
handle_common(cast, connect, State, _Data) when State =/= idle ->
    keep_state_and_data;
handle_common({call, From}, is_connected, State, _) when State =/= connected ->
    {keep_state_and_data, [{reply, From, false}]};
handle_common({call, From}, {request, _, _, _}, State, _Data)
when State =/= connected ->
    {keep_state_and_data, [{reply, From, {error, disconnected}}]};
handle_common(cast, {notify, _Method, _Type, _Params}, _State, _Data) ->
    % We ignore notifications sent while disconnected
    keep_state_and_data;
handle_common(info, reboot, _, _) ->
    init:stop(),
    keep_state_and_data;
handle_common(info, {'EXIT', Conn, Reason}, _State,
              Data = #data{conn = Conn, retry_count = RetryCount}) ->
    % The connection process died
    ?LOG_WARNING(#{description =>
                    ?FORMAT("The connection to grisp.io died: ~p", [Reason]),
                   event => connection_failed, reason => Reason}),
    {next_state, waiting_ip, conn_died(Data#data{retry_count = RetryCount + 1})};
handle_common(info, {'EXIT', _Conn, _Reason}, _State, _Data) ->
    % Ignore any EXIT from past jarl connections
    keep_state_and_data;
handle_common(info, {jarl, Conn, Msg}, State, _Data) ->
    ?LOG_DEBUG("Received message from unknown connection ~p in state ~w: ~p",
               [Conn, State, Msg]),
    keep_state_and_data;
handle_common(cast, Cast, _, _) ->
    error({unexpected_cast, Cast});
handle_common({call, _}, Call, _, _) ->
    error({unexpected_call, Call});
handle_common(info, Info, State, Data) ->
    ?LOG_ERROR(#{event => unexpected_info,
                 info => Info,
                 state => State,
                 data => Data}),
    keep_state_and_data.


%--- Internal Functions --------------------------------------------------------

generic_errors() -> [
    {device_not_linked,          -1, <<"Device not linked">>},
    {token_expired,              -2, <<"Token expired">>},
    {device_already_linked,      -3, <<"Device already linked">>},
    {invalid_token,              -4, <<"Invalid token">>},
    {grisp_updater_unavailable, -10, <<"Software update unavailable">>},
    {already_updating,          -11, <<"Already updating">>},
    {boot_system_not_validated, -12, <<"Boot system not validated">>},
    {validate_from_unbooted,    -13, <<"Validate from unbooted">>}
].

as_bin(Binary) when is_binary(Binary) -> Binary;
as_bin(List) when is_list(List) -> list_to_binary(List);
as_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom).

handle_connection_message(_Data, {response, _Result, #{on_result := undefined}}) ->
    keep_state_and_data;
handle_connection_message(Data, {response, Result, #{on_result := OnResult}}) ->
    {keep_state, OnResult(Data, Result)};
handle_connection_message(_Data, {error, Code, Msg, _ErrorData,
                                 #{on_error := undefined}}) ->
    ?LOG_WARNING("Unhandled remote request error ~w: ~s", [Code, Msg]),
    keep_state_and_data;
handle_connection_message(Data, {error, Code, Msg, ErrorData,
                                 #{on_error := OnError}}) ->
    {keep_state, OnError(Data, remote, Code, Msg, ErrorData)};

handle_connection_message(_Data, {jarl_error, Reason,
                                 #{on_error := undefined}}) ->
    ?LOG_WARNING("Unhandled local request error ~w", [Reason]),
    keep_state_and_data;
handle_connection_message(Data, {jarl_error, Reason,
                                 #{on_error := OnError}}) ->
    {keep_state, OnError(Data, local, Reason, undefined, undefined)};
handle_connection_message(Data, Msg) ->
    case grisp_connect_api:handle_msg(Msg) of
        ok -> keep_state_and_data;
        {error, Code, Message, ErData, ReqRef} ->
            conn_error(Data, Code, Message, ErData, ReqRef),
            keep_state_and_data;
        {reply, Result, ReqRef} ->
            conn_result(Data, Result, ReqRef),
            keep_state_and_data
    end.

% Connection Functions

conn_start(Data = #data{conn = undefined,
                        domain = Domain,
                        port = Port,
                        ws_path = WsPath,
                        ws_transport = WsTransport}) ->
    WsPingTimeout = ?ENV(ws_ping_timeout, V =:= infinity orelse is_integer(V)),
    WsReqTimeout = ?ENV(ws_request_timeout, V =:= infinity orelse is_integer(V)),
    ConnTransport = case WsTransport of
        tcp -> tcp;
        tls -> {tls, grisp_cryptoauth_tls:options(Domain)}
    end,
    ErrorList = persistent_term:get({?MODULE, self()}),
    ConnOpts = #{
        domain => Domain,
        port => Port,
        transport => ConnTransport,
        path => WsPath,
        errors => ErrorList,
        ping_timeout => WsPingTimeout,
        request_timeout => WsReqTimeout
    },
    case jarl:start_link(self(), ConnOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Conn} -> {ok, Data#data{conn = Conn}}
    end.

% Safe to call in any state
conn_close(Data = #data{conn = undefined}, _Reason) ->
    Data;
conn_close(Data = #data{conn = Conn}, _Reason) ->
    grisp_connect_log_server:stop(),
    jarl:disconnect(Conn),
    Data#data{conn = undefined}.

% Safe to call in any state
conn_died(Data) ->
    grisp_connect_log_server:stop(),
    Data#data{conn = undefined}.

-spec conn_request(data(), jarl:method(), atom(), map(),
                undefined | on_result_fun(), undefined | on_error_fun())
    -> data().
conn_request(Data = #data{conn = Conn}, Method, Type, Params, OnResult, OnError)
  when Conn =/= undefined ->
    ReqCtx = #{on_result => OnResult, on_error => OnError},
    Params2 = maps:put(type, Type, Params),
    case jarl:request(Conn, Method, Params2, ReqCtx) of
        ok -> Data;
        {jarl_error, Reason} ->
            OnError(Data, local, Reason, undefined, undefined)
    end.

conn_notify(#data{conn = Conn}, Method, Type, Params)
  when Conn =/= undefined ->
    Params2 = maps:put(type, Type, Params),
    jarl:notify(Conn, Method, Params2).

conn_result(#data{conn = Conn}, Result, ReqRef)
  when Conn =/= undefined ->
    jarl:reply(Conn, Result, ReqRef).

conn_error(#data{conn = Conn}, Code, Message, ErData, ReqRef)
  when Conn =/= undefined ->
    jarl:reply(Conn, Code, Message, ErData, ReqRef).

% IP check functions

check_inet_ipv4() ->
    case get_ip_of_valid_interfaces() of
        {IP1,_,_,_} = IP when IP1 =/= 127 -> {ok, IP};
        _ -> invalid
    end.

get_ipv4_from_opts([]) ->
    undefined;
get_ipv4_from_opts([{addr, {_1, _2, _3, _4}} | _]) ->
    {_1, _2, _3, _4};
get_ipv4_from_opts([_ | TL]) ->
    get_ipv4_from_opts(TL).

has_ipv4(Opts) ->
    get_ipv4_from_opts(Opts) =/= undefined.

flags_are_ok(Flags) ->
    lists:member(up, Flags) and
        lists:member(running, Flags) and
        not lists:member(loopback, Flags).

get_valid_interfaces() ->
    {ok, Interfaces} = inet:getifaddrs(),
    [
        Opts
     || {_Name, [{flags, Flags} | Opts]} <- Interfaces,
        flags_are_ok(Flags),
        has_ipv4(Opts)
    ].

get_ip_of_valid_interfaces() ->
    case get_valid_interfaces() of
        [Opts | _] -> get_ipv4_from_opts(Opts);
        _ -> undefined
    end.

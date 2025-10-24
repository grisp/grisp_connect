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
-export([wait_connected/1]).
-export([request/3]).
-export([notify/3]).

% Internal API
-export([reboot/0]).

% Behaviour gen_statem callback functions
-export([init/1, terminate/3, code_change/4, callback_mode/0]).

% State Functions
-export([idle/3]).
-export([waiting_network/3]).
-export([connecting/3]).
-export([connected/3]).


%--- Types ---------------------------------------------------------------------

-record(data, {
    domain :: binary(),
    port :: inet:port_number(),
    ws_path :: binary(),
    ws_transport :: tcp | tls,
    conn :: undefined | pid(),
    retry_count = 0 :: non_neg_integer(),
    last_error :: term(),
    max_retries = infinity :: non_neg_integer() | infinity,
    wait_calls = [] :: [gen_statem:from()]
}).

-type data() :: #data{}.
-type on_result_fun() :: fun((data(), Result :: term()) -> data()).
-type on_error_fun() :: fun((data(), local | remote,
                             Code :: atom() | integer(),
                             Message :: undefined | binary(),
                             Data :: term()) -> data()).


%--- Macros --------------------------------------------------------------------

-define(GRISP_IO_PROTOCOL, <<"grisp-io-v1">>).
-define(FORMAT(FMT, ARGS), iolist_to_binary(io_lib:format(FMT, ARGS))).
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

wait_connected(Timeout) ->
    try gen_statem:call(?MODULE, ?FUNCTION_NAME, Timeout)
    catch exit:noproc -> {error, noproc}
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
    MaxRetries = ?ENV(ws_max_retries, is_integer(V) orelse V =:= infinity),
    Data = #data{
        domain = Domain,
        port = Port,
        ws_transport = WsTransport,
        ws_path = WsPath,
        max_retries = MaxRetries
    },
    % The error list is put in a persistent term to not add noise to the state.
    persistent_term:put({?MODULE, self()}, generic_errors()),
    NextState = case AutoConnect of
        true -> waiting_network;
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

idle(enter, _OldState,
     Data = #data{wait_calls = WaitCalls, last_error = LastError}) ->
    % When entering idle, we reply to all wait_connected calls with the last error
    gen_statem:reply([{reply, F, {error, LastError}} || F <- WaitCalls]),
    {keep_state, Data#data{wait_calls = [], last_error = undefined}};
idle({call, From}, wait_connected, _) ->
    {keep_state_and_data, [{reply, From, {error, not_connecting}}]};
idle(cast, connect, Data) ->
    {next_state, waiting_network, Data};
?HANDLE_COMMON.

% @doc State waiting_network is used to check the device has an IP address.
% The first time entering this state, the check will be performed right away.
% If the device do not have an IP address, it will wait a fixed amount of time
% and check again, without incrementing the retry counter.
waiting_network(enter, _OldState, _Data) ->
    % First IP check do not have any delay
    {keep_state_and_data, [{state_timeout, 0, check_ip}]};
waiting_network(state_timeout, check_ip, Data) ->
    case grisp_connect_utils:check_inet_ipv4() of
        {ok, IP} ->
            ?LOG_DEBUG(#{description => ?FORMAT("IP Address available: ~s",
                                                [format_ipv4(IP)]),
                         event => checked_ip, ip => format_ipv4(IP)}),
            {next_state, connecting, Data};
        invalid ->
            ?LOG_DEBUG(#{description => <<"Waiting for an IP address do connect to grisp.io">>,
                         event => waiting_network}),
            {keep_state_and_data, [{state_timeout, 1000, check_ip}]}
    end;
?HANDLE_COMMON.

% @doc State connecting is used to establish a connection to grisp.io.
connecting(enter, _OldState, #data{retry_count = RetryCount}) ->
    Delay = grisp_connect_utils:retry_delay(RetryCount),
    ?LOG_DEBUG("Scheduling connection attempt in ~w ms", [Delay]),
    {keep_state_and_data, [{state_timeout, Delay, connect}]};
connecting(state_timeout, connect, Data = #data{conn = undefined}) ->
    ?LOG_INFO(#{description => <<"Connecting to grisp.io ...">>,
                event => connecting}),
    case conn_start(Data) of
        {ok, Data2} ->
            {keep_state, Data2, [{state_timeout, ?CONNECT_TIMEOUT, timeout}]};
        {error, Reason} ->
            ?LOG_WARNING(#{description => ?FORMAT("Failed to connect to grisp.io: ~p", [Reason]),
                           event => connection_failed, reason => Reason}),
            reconnect(Data, Reason)
    end;
connecting(state_timeout, timeout, Data) ->
    Reason = connect_timeout,
    ?LOG_WARNING(#{description => <<"Timeout while connecting to grisp.io">>,
                   event => connection_failed, reason => Reason}),
    reconnect(conn_close(Data, Reason), Reason);
connecting(info, {jarl, Conn, {connected, _}}, Data = #data{conn = Conn}) ->
    % Received from the connection process
    ?LOG_NOTICE(#{description => <<"Connected to grisp.io">>,
                  event => connected}),
    {next_state, connected, Data#data{retry_count = 0}};
?HANDLE_COMMON.

connected(enter, _OldState, Data = #data{wait_calls = WaitCalls}) ->
    % When entering connected, we reply to all wait_connected calls with ok
    gen_statem:reply([{reply, F, ok} || F <- WaitCalls]),
    {keep_state, Data#data{wait_calls = [], last_error = undefined}};
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
handle_common({call, From}, wait_connected, _State,
              Data = #data{wait_calls = WaitCalls}) ->
    {keep_state, Data#data{wait_calls = [From | WaitCalls]}};
handle_common({call, From}, {request, _, _, _}, State, _Data)
when State =/= connected ->
    {keep_state_and_data, [{reply, From, {error, disconnected}}]};
handle_common(cast, {notify, _Method, _Type, _Params}, _State, _Data) ->
    % We ignore notifications sent while disconnected
    keep_state_and_data;
handle_common(info, reboot, _, _) ->
    init:stop(),
    keep_state_and_data;
handle_common(info, {'EXIT', Conn, Reason}, _State, Data = #data{conn = Conn}) ->
    RealReason = case Reason of
        {shutdown, R} -> R;
        R -> R
    end,
    ?LOG_WARNING(#{description => ?FORMAT("Connection to grisp.io terminated: ~p", [RealReason]),
                   event => connection_failed, reason => RealReason}),
    reconnect(conn_died(Data), RealReason);
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
handle_common(info, Info, State, _Data) ->
    ?LOG_WARNING(#{description => <<"Unexpected message">>,
                   event => unexpected_info, info => Info, state => State}),
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

format_ipv4({A, B, C, D}) ->
    ?FORMAT("~w.~w.~w.~w", [A, B, C, D]).

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

% @doc Setup the state machine to rety connecting to grisp.io if the maximum
% number of allowed atempts has not been reached.
% Otherwise, the state machine will give up and go back to idle.
reconnect(Data = #data{retry_count = RetryCount,
                       max_retries = MaxRetries,
                       last_error = LastError}, Reason)
  when MaxRetries =/= infinity, RetryCount >= MaxRetries ->
    Error = case Reason of undefined -> LastError; E -> E end,
    ?LOG_ERROR(#{description => <<"Max retries reached, giving up connecting to grisp.io">>,
                 event => max_retries_reached, last_error => LastError}),
    {next_state, idle, Data#data{retry_count = 0, last_error = Error}};
reconnect(Data = #data{retry_count = RetryCount, last_error = LastError},
          Reason) ->
    Error = case Reason of undefined -> LastError; E -> E end,
    % When reconnecting we always increment the retry counter, even if we
    % where connected and it was reset to 0, the next step will always be
    % retry number 1. It should never reconnect right away.
    {next_state, waiting_network,
     Data#data{retry_count = RetryCount + 1, last_error = Error}}.

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
        tls -> {tls, tls_options(Domain)}
    end,
    ErrorList = persistent_term:get({?MODULE, self()}),
    ConnOpts = #{
        domain => Domain,
        port => Port,
        transport => ConnTransport,
        path => WsPath,
        errors => ErrorList,
        ping_timeout => WsPingTimeout,
        request_timeout => WsReqTimeout,
        protocols => [?GRISP_IO_PROTOCOL]
    },
    case jarl:start_link(self(), ConnOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Conn} -> {ok, Data#data{conn = Conn}}
    end.

% Safe to call in any state
conn_close(Data = #data{conn = undefined}, _Reason) ->
    Data;
conn_close(Data = #data{conn = Conn}, _Reason) ->
    jarl:disconnect(Conn),
    Data#data{conn = undefined}.

% Safe to call in any state
conn_died(Data) ->
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
  when Conn =/= undefined, is_binary(ErData) orelse ErData =:= undefined ->
    jarl:reply(Conn, Code, Message, ErData, ReqRef);
conn_error(Data, Code, Message, ErData, ReqRef) ->
    BinErData = iolist_to_binary(io_lib:format("~p", [ErData])),
    conn_error(Data, Code, Message, BinErData, ReqRef).

tls_options(Domain) ->
    ExtraOpts = case application:get_env(grisp_connect, allow_expired_certs) of
        {ok, false} -> [];
        {ok, true} ->
            [{verify_fun,
                {fun grisp_connect_crypto:skip_cert_expired/3, []}}]
    end,
    grisp_keychain:tls_options(Domain) ++ ExtraOpts.

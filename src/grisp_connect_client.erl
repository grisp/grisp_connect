%% @doc Client to interact with grisp.io
%%
%% This module contains a state machine to ensure connectivity with grisp.io.
%% @end
-module(grisp_connect_client).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

-import([grisp_connect_utils, as_bin/1]).

% API Functions
-export([start_link/0]).
-export([connect/0]).
-export([is_connected/0]).
-export([disconnect/0]).
-export([reboot/0]).
-export([link_device/1]).

% Internal API Functions
-export([feature_is_reboot_allowed/1]).
-export([feature_reboot/1]).
-export([feature_request/3]).
-export([feature_notify/3]).
-export([feature_post/5]).
-export([feature_error/5]).
-export([feature_reply/3]).

% Behaviour gen_statm Generic Callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).

% Behaviour gen_statm State Callbacks
-export([waiting_ip/3]).
-export([disconnected/3]).
-export([connecting/3]).
-export([handshaking/3]).
-export([unlinked/3]).
-export([linked/3]).
-export([rebooting/3]).



%--- Types ---------------------------------------------------------------------

-record(data, {
    domain :: binary(),
    port :: inet:port_number(),
    ws_path :: binary(),
    ws_transport :: tcp | tls,
    ws_ping_timeout :: infinity | pos_integer(),
    ws_request_timeout :: infinity | pos_integer(),
    error_list = [] :: [grisp_connect_connection:error_mapping()],
    features = #{} :: #{atom() => {Mod :: module(), Sub :: term()}},
    reboot_allowed :: boolean(),
    % undefined for the first connection
    reconn_delay = undefined :: undefined | pos_integer(),
    % Next retry count, 0 means the initial connection so it should
    % be reset to 1 after a successful connection.
    reconn_retries = 0 :: non_neg_integer(),
    % The last connection error reason
    reconn_error :: term(),
    reconn_max_retries :: infinity | non_neg_integer(),
    reconn_base_delay :: pos_integer(),
    reconn_max_delay :: pos_integer(),
    reconn_multiplier :: pos_integer(),
    reconn_jitter :: pos_integer(),
    conn :: pid(),
    pending_calls = #{} :: #{Tag :: atom() => [gen_statem:from()]}
}).


%--- Macros --------------------------------------------------------------------

-define(PROTOCOL_VERSION, <<"1.0.0">>).

-define(SERVER, ?MODULE).
-define(CHECK_IP_INTERVAL, 1000).
-define(CONNECT_TIMEOUT, 2000).
-define(HANDSHAKE_TIMEOUT, 2000).
-define(REBOOT_DELAY, 1000).
-define(ENV(KEY), fun() ->
    case application:get_env(grisp_connect, KEY) of
        {ok, V} -> V;
        undefined -> erlang:exit({missing_env, KEY})
    end
end()).
-define(ENV(KEY, GUARDS), fun() ->
    case application:get_env(grisp_connect, KEY) of
        {ok, V} when GUARDS -> V;
        {ok, _} -> erlang:exit({invalid_env, KEY});
        undefined -> erlang:exit({missing_env, KEY})
    end
end()).
-define(ENV(KEY, GUARDS, CONV), fun() ->
    case application:get_env(grisp_connect, KEY) of
        {ok, V} when GUARDS -> CONV;
        {ok, _} -> erlang:exit({invalid_env, KEY});
        undefined -> erlang:exit({missing_env, KEY})
    end
end()).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(EventType, EventContent, Data) ->
        handle_common(EventType, EventContent, ?FUNCTION_NAME, Data)).


%--- API Functions -------------------------------------------------------------

-spec start_link() -> gen_statem:start_ret().
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec connect() ->
    {ok, Linked :: boolean()} | {error, retries_exausted} | {error, cancelled}
connect() ->
    gen_statem:call(?SERVER, connect).

-spec is_connected() -> boolean().
is_connected() ->
    gen_statem:call(?SERVER, is_connected).

-spec disconnect() -> ok
disconnect() ->
    gen_statem:call(?SERVER, disconnect).

-spec reboot() -> ok | manual_reboot_required.
reboot() ->
    gen_statem:call(?SERVER, reboot).

-spec link_device(Token :: binary()) ->
    ok | {error, not_connected} | {error, already_linked} |
    {error, token_undefined} | {error, invalid_token}.
link_device(Token) when is_binary(Token) ->
    gen_statem:call(?SERVER, {link_device, Token}).


%--- Internal API Functions ----------------------------------------------------

feature_is_reboot_allowed(#data{reboot_allowed = RebootAllowed}) ->
    RebootAllowed.

feature_reboot(#data{reboot_allowed = true}) ->
    self() ! feature_reboot,
    ok;
feature_reboot(#data{reboot_allowed = false}) ->
    self() ! feature_reboot,
    manual_reboot_required.

feature_request(#data{conn = undefined}, _Method, _Params) ->
    {error, not_connected};
feature_request(#data{conn = Conn}, Method, Params) ->
    grisp_connect_connection:request(Conn, Method, Params).

feature_notify(#data{conn = undefined}, _Method, _Params) ->
    {error, not_connected};
feature_notify(#data{conn = Conn}, Method, Params) ->
    grisp_connect_connection:notify(Conn, Method, Params).

feature_post(#data{conn = undefined}, Method, _Params, _OnResult, OnError) ->
    % Ensure the error callback is called anyway
    ReqCtx = #{on_error => OnError},
    self() ! {error, undefined, Method, not_connected, ReqCtx},
    {error, not_connected};
feature_post(#data{conn = Conn}, Method, Params, OnResult, OnError) ->
    ReqCtx = #{on_result => OnResult, on_error => OnError},
    grisp_connect_connection:post(Conn, Method, Params, ReqCtx).

feature_error(#data{conn = undefined}, _Code, _Message, _Data, _ReqRef) ->
    {error, not_connected};
feature_error(#data{conn = Conn}, Code, Message, Data, ReqRef) ->
    grisp_connect_connection:error(Conn, Code, Message, Data, ReqRef).

feature_reply(#data{conn = undefined}, _Result, _ReqRef) ->
    {error, not_connected};
feature_reply(#data{conn = Conn}, Result, ReqRef) ->
    grisp_connect_connection:reply(Conn, Result, ReqRef).


%--- Behaviour gen_statem Generic Callbacks ------------------------------------

callback_mode() -> [state_functions, state_enter].

init([]) ->
    process_flag(trap_exit, true),
    AutoConnect = ?ENV(connect, is_boolean(V)),
    Domain = ?ENV(domain, is_binary(V) orelse is_list(V), as_bin(V)),
    Port = ?ENV(port, is_integer(V) andalso V >= 0 andalso V < 65536),
    WsTransport = ?ENV(ws_transport, V =:= tls orelse V =:= tcp),
    WsPath = ?ENV(ws_path, is_binary(V) orelse is_list(V), as_bin(V)),
    WsPingTimeout = ?ENV(ws_ping_timeout, V =:= infinity orelse is_integer(V)),
    WsReqTimeout = ?ENV(ws_request_timeout, V =:= infinity orelse is_integer(V)),
    RebootAllowed = ?ENV(reboot_allowed, is_boolean(V)),
    ReconnMaxRetries = ?ENV(reconn_max_retries, V =:= infinity orelse is_integer(V)),
    ReconnBaseDelay = ?ENV(reconn_base_delay, is_integer(V) andalso V >= 0),
    ReconnMaxDelay = ?ENV(reconn_max_delay, is_integer(V) andalso V >= 0),
    ReconnMultiplier = ?ENV(reconn_multiplier, is_integer(V) andalso V > 0),
    ReconnJitter = ?ENV(reconn_jitter, is_integer(V) andalso V >= 0),
    Features = ?ENV(features),
    Data = features_init(#{
        domain = Domain,
        port = Port,
        ws_transport = WsTransport,
        ws_path = WsPath,
        ws_ping_timeout = WsPingTimeout,
        ws_request_timeout = WsReqTimeout,
        reboot_allowed = RebootAllowed,
        reconn_max_retries = ReconnMaxRetries,
        reconn_base_delay = ReconnBaseDelay,
        reconn_max_delay = ReconnMaxDelay,
        reconn_multiplier = ReconnMultiplier,
        reconn_jitter = ReconnJitter
    }, Features),
    NextState = case AutoConnect of
        true -> waiting_ip;
        false -> idle
    end,
    {ok, NextState, Data}.

terminate(Reason, _State, Data) ->
    features_terminate(conn_close(Data), Reason),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.


%--- Behaviour gen_statem State Callbacks --------------------------------------

idle(enter, _OldState, Data) ->
    Data2 = call_reply(Data, connect, {error, cancelled}),
    statem_enter(Data2, ?FUNCTION_NAME, []);
idle({call, From}, connect, Data) ->
    {next_state, waiting_ip, call_add(Data, connect, From)};
idle({call, From}, disconnect, Data) ->
    {keep_state, Data, [{reply, From, ok}]};
?HANDLE_COMMON.

waiting_ip(enter, _OldState, _Data) ->
    statem_enter(Data, ?FUNCTION_NAME, [{state_timeout, 0, retry}]);
waiting_ip(state_timeout, retry, Data) ->
    case grisp_connect_inet:ipv4_external_address() of
        undefined ->
            ?LOG_DEBUG("No external IPv4 address available", [],
                       #{event => waiting_ip}),
            {next_state, waiting_ip, Data,
             [{state_timeout, ?CHECK_IP_INTERVAL, retry}]};
        Ip ->
            ?LOG_INFO("Found external IPv4 address ~s", [inet:ntoa(Ip)],
                      #{event => checked_ip, ip => Ip}),
            {next_state, disconnected, Data};
    end;
?HANDLE_COMMON.

disconnected(enter, _OldState,
             Data = #data{reconn_retries = Retries,
                          reconn_max_retries = MaxRetries})
  when MaxRetries =/= infinity, Retries > MaxRetries ->
    statem_enter(Data, ?FUNCTION_NAME, [{state_timeout, 0, retries_exausted}]);
disconnected(enter, _OldState, Data = #data{reconn_delay = undefined}) ->
    % The first connection does not have any delay
    statem_enter(Data, ?FUNCTION_NAME, [{state_timeout, 0, reconnect}]);
disconnected(enter, _OldState,
             Data = #data{reconn_retries = Retries,
                          reconn_delay = Delay}) ->
    ?LOG_DEBUG("Reconnecting to ~s in ~w ms (~w)",
               [format_addr(Data), Delay, Retries],
               #{event = backoff, delay = Delay, retries = Retries}),
    statem_enter(Data, ?FUNCTION_NAME, [{state_timeout, Delay, reconnect}]);
disconnected(state_timeout, reconnect, Data) ->
    {next_state, connecting, Data};
disconnected(state_timeout, retries_exausted,
             Data = #data{reconn_error = Reason}) ->
    ?LOG_ERROR("Maximum number of reconnection attempt reached, terminating",
               #{event => stop_reconnecting, reason => retries_exausted,
                 last_reason => Reason}),
    Data2 = call_reply(Data, connect, {error, retries_exausted}),
    {next_state, idle, Data2};
?HANDLE_COMMON.

connecting(enter, _OldState, Data) ->
    statem_enter(Data, ?FUNCTION_NAME, [{state_timeout, 0, connect}]);
connecting(state_timeout, connect, Data = #data{conn = undefined}) ->
    ?LOG_INFO("Connecting to ~s", [format_ws_uri(Data)],
              #{event => connecting}),
    case conn_start(Data) of
        {ok, Data2} ->
            {keep_state, Data2, [{state_timeout, ?CONNECT_TIMEOUT, timeout}]};
        {error, Reason} ->
            ?LOG_WARNING("Failed to connect to grisp.io: ~p", [Reason],
                         #{event => connection_failed, reason => Reason}),
            {next_state, waiting_ip, retry_failed(Data, Reason)}
    end;
connecting(state_timeout, timeout, Data) ->
    Reason = connect_timeout,
    ?LOG_WARNING("Timeout while connecting to grisp.io", [],
                 #{event => connection_failed, reason => Reason}),
    Data2 = conn_close(retry_failed(Data, Reason), Reason),
    {next_state, waiting_ip, Data2};
connecting(info, {conn, Conn, connected}, Data = #data{conn = Conn}) ->
    % Received from the connection process
    % Waiting for the handshake to succeed before reseting the retries...
    ?LOG_INFO("Connected to ~s", [format_ws_uri(Data)],
              #{event = connected}),
    {next_state, handshake, Data};
?HANDLE_COMMON.

handshake(enter, _OldState, Data) ->
    ?LOG_DEBUG("Start protocol handshake...", []),
    Data2 = proto_start_handshake(Data),
    statem_enter(Data2, ?FUNCTION_NAME,
                 [{state_timeout, ?HANDSHAKE_TIMEOUT, timeout}]);
handshake(state_timeout, timeout, Data) ->
    Reason = handshake_timeout,
    ?LOG_WARNING("Timeout while handshaking with grisp.io", [],
                 #{event => connection_failed, reason => Reason}),
    Data2 = conn_close(retry_failed(Data, Reason), Reason),
    {next_state, waiting_ip, Data2};
handshake(info, {conn, Conn, Msg}, Data = #data{conn = Conn}) ->
    % Not using statem_connection_message to handle messages because
    % of the retries handling and the special logging.
    case proto_handshake(Data, Msg) of
        {error, Reason, Data2} ->
            ?LOG_WARNING("Protocol handshake with grisp.io failed: ~p", [Reason],
                 #{event => handshake_failed, reason => Reason}),
            Data2 = conn_close(retry_failed(Data, Reason), Reason),
            {next_state, waiting_ip, Data2};
        {continue, Data2} ->
            {keep_state, Data2};
        {unlinked, Data2} ->
            ?LOG_NOTICE("Unlinked device connected to grisp.io", [],  %<<<<<<<<<<<<<<<<<< Device serial ?
                        #{event => active, linked => false}),
            {next_state, unlinked, retry_done(Data2)};
        {linked, Data2} ->
            ?LOG_NOTICE("Linked device connected to grisp.io", [], %<<<<<<<<<<<<<<<<<< Device serial ?
                        #{event => active, linked => true}),
            {next_state, linked, retry_done(Data2)}
    end;
?HANDLE_COMMON.

unlinked(enter, _OldState, Data) ->
    Data2 = call_reply(Data, connect, {ok, false}),
    statem_enter(Data2, ?FUNCTION_NAME, []);
unlinked({call, From}, connect, Data) ->
    {keep_state, Data, [{reply, From, {ok, false}}]};
unlinked({call, From}, {link_device, _Token}, Data) ->
    {keep_state, proto_start_link_device(Data, From)};
unlinked(info, {conn, Conn, Msg}, Data = #data{conn = Conn}) ->
    statem_connection_message(Data, unlinked, Msg, fun proto_unlinked/2);
?HANDLE_COMMON.

linked(enter, _OldState, Data) ->
    Data2 = call_reply(Data, connect, {ok, true}),
    statem_enter(Data2, ?FUNCTION_NAME, []);
linked({call, From}, connect, Data) ->
    {keep_state, Data, [{reply, From, {ok, true}}]};
linked({call, From}, {link_device, _Token}, Data) ->
    {keep_state, Data, [{reply, From, {error, already_linked}}]};
linked(info, {conn, Conn, Msg}, Data = #data{conn = Conn}) ->
    statem_connection_message(Data, linked, Msg, fun proto_linked/2);
?HANDLE_COMMON.

rebooting(enter, _OldState, Data = #data{reboot_allowed = false}) ->
    Data2 = call_reply(Data, connect, {ok, canceled}),
    Data3 = call_reply(Data, reboot, manual_reboot_required),
    statem_enter(Data2, ?FUNCTION_NAME, []);
rebooting(enter, _OldState, Data = #data{reboot_allowed = true}) ->
    Data2 = call_reply(Data, connect, {ok, canceled}),
    Data3 = call_reply(Data, reboot, ok),
    statem_enter(Data2, ?FUNCTION_NAME,
                 [{state_timeout, ?REBOOT_DELAY, reboot}]);
rebooting(state_timeout, reboot, Data) ->
    init:stop(),
    {stop, normal};
rebooting({call, From}, connect, Data) ->
    {keep_state, Data, [{reply, From, {error, cancelled}}]};
handle_common({call, From}, disconnect, _State, Data) ->
    {keep_state, Data, [{reply, From, ok}]};
rebooting({call, From}, reboot, Data = #data{reboot_allowed = false}) ->
    {keep_state, Data, [{reply, From, manual_reboot_required}]};
rebooting({call, From}, reboot, Data = #data{reboot_allowed = true}) ->
    {keep_state, Data, [{reply, From, ok}]};
handle_common(info, feature_reboot, _Data) ->
    keep_state_and_data;
?HANDLE_COMMON.

% Common event handling appended as last match case to each state_function
handle_common({call, From}, is_connected, State, Data) ->
    IsConnected = State =:= linked orelse State =:= unlinked,
    {keep_state, Data, [{reply, From, IsConnected}]};
handle_common({call, From}, connect, _State, Data) ->
    {keep_state, call_add(Data, connect, From)};
handle_common({call, From}, disconnect, _State, Data) ->
    Data2 = conn_close(retry_done(Data), cancelled),
    {next_state, idle, Data2, [{reply, From, ok}]};
handle_common({call, From}, reboot, _State, Data) ->
    Data2 = conn_close(retry_done(Data)),
    {next_state, rebooting, add_call(Data2, reboot, From)};
handle_common({call, _From}, {link_device, _Token}, _State, Data) ->
    {keep_state, Data, [{reply, From, {error, not_connected}}]};
handle_common(info, {'EXIT', Conn, Reason}, _State, Data = #data{conn = Conn}) ->
    % The connection process died
    ?LOG_WARNING("The connection to grisp.io died unexpectedly: ~p", [Reason],
                 #{event = connection_failed, reason = Reason}),
    Data2 = conn_died(retry_failed(Data, timeout)),
    {next_state, waiting_ip, Data2};
handle_common(info, {conn, Conn, Msg}, State, _Data) ->
    ?LOG_DEBUG("Received message from unknown connection ~p in state ~w: ~p",
               [Conn, State, Msg]),
    keep_state_and_data;
handle_common(info, feature_reboot, State, _Data) ->
    {next_state, rebooting, conn_close(retry_done(Data))};
handle_common(cast, Cast, State, Data) ->
    ?LOG_ERROR("Unexpected cast in state ~w: ~p", [State, Cast],
               #{reason => unexpected_cast, state => State}),
    {stop, unexpected_cast, Data}.
handle_common({call, From}, Call, State, Data) ->
    ?LOG_ERROR("Unexpected call from ~p in state ~w: ~p", [From, State, Call],
               #{reason => unexpected_call, state => State}),
    {stop_and_reply, unexpected_call,
     [{reply, From, {error, unexpected_call}}], Data};
handle_common(info, Info, State, Data) ->
    ?LOG_WARNING("Unexpected info message in state ~w: ~p", [State, Info],
                 #{event => unexpected_info, state => State, info => Info})
    keep_state_and_data.


%--- Internal Functions --------------------------------------------------------

generic_errors() -> [
    {client_not_supported,   -42001, <<"Client not supported">>},
    {device_not_linked,      -42002, <<"Device not linked">>},
    {feature_not_available,  -42003, <<"Feature not available">>},
    {invalid_token,          -42004, <<"Invalid token">>},
    {token_expired,          -42005, <<"Token expired">>},
    {already_linked,         -42006, <<"Device already linked">>},
    {not_implemented,        -42999, <<"Not implemented">>}
].

grisp_hardware_info() ->
    Platform = grisp:platform(),
    case grisp_hw:eeprom_read() of
        {ok, #{grisp_serial => Serial, grisp_version => Version}} ->
            #{platform => Platform, serial => Serial,
              version => Version, crc => true};
        {invalid_crc, #{grisp_serial => Serial, grisp_version => Version}} ->
            #{platform => Platform, serial => Serial,
              version => Version, crc => false}
    end.

format_addr(#data{domain = Domain, port = Port}) ->
    io_lib:format("~s:~w", [Domain, Port]).

format_ws_uri(#data{domain = Domain, port = Port,
             ws_transport = Transport, ws_path = Path}) ->
    Proto = case Transport of
        tcp -> <<"ws">>;
        tls -> <<"wss">>
    end,
    io_lib:format("~s://~s:~w~s", [Proto, Domain, Port, Path]).

%% @doc Calculate the next reconnection delay.
%% NextDelay = min(MaxDelay, LastDelay * Multiplier + Jitter)
%% Where jitter is a number that in function of J being the given
%% startup option reconn_jitter is a random number between
%% (-(V div 2) - (V rem 2)) and (V div 2).
update_backoff(Data = #data{reconn_delay = undefined}) ->
    #data{reconn_base_delay = BaseDelay} = Data,
    Data#data{reconn_delay = BaseDelay};
update_backoff(Data = #data{reconn_delay = LastDelay}) ->
    #data{reconn_max_delay = MaxDelay,
          reconn_multiplier = Multipier,
          reconn_jitter = Jitter} = Data,
    Extra = rand:uniform(Jitter + 1) - (Jitter div 2) - (Jitter rem 2) - 1,
    NewDelay = min(MaxDelay, LastDelay * Multipier + Extra),
    Data#data{reconn_delay = NewDelay}.

%% Reset the reconnection backoff.
reset_backoff(Data) ->
    #data{reconn_base_delay = BaseDelay} = Data,
    Data#data{reconn_delay = BaseDelay}.

retry_failed(Data = #data{reconn_retries = Retries}, Reason) ->
    update_backoff(Data#data{
        reconn_error = Reason,
        reconn_retries = Retries + 1
    }).

% Safe to call multiple times
retry_done(Data) ->
    reset_backoff(Data#data{
        reconn_error = undefined,
        reconn_retries = 1
    }).


% Protocol Functions

% {request, Method :: method(), Params :: map(), ReqRef :: binary()}
% {response, Method :: method(), Result :: jsx:json_term(), ReqCtx :: term()}
% {error, Method :: method(), Code :: atom() | integer(), Message :: undefined | binary(), Data :: undefined | jsx:json_term(), ReqCtx :: term()}
% {error, Method :: method(), Reason :: term(), ReqCtx :: term()}
% {timeout, Method :: binary(), ReqCtx :: term()}
% {notification, Method :: method(), Params :: jsx:json_term()}
% {error, Code :: atom() | integer(), Message :: undefined | binary(), Data :: undefined | jsx:json_term()}


proto_start_handshake(Data = #data{features = FeatMap}) ->
    #{
        platform := Platform,
        version := PlatformVersion,
        serial := Serial,
    } = grisp_hardware_info(),
    conn_post(Data, 'Handshake', #{
                version => ?PROTOCOL_VERSION,
                platform => Platform,
                platform_version => PlatformVersion,
                serial => Serial
                capabilities => maps:keys(FeatMap)
              },
              fun proto_handshake_result/3,
              fun proto_handshake_error/6),
    Data.

#### make feature_post take the feature name....

proto_handshake_result(Data, )

proto_start_link_device(Data, Token) ->
    % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    Data.

proto_handshake(Data, Msg) ->
    % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    {linked, Data}. % {unlinked, Data} {error, Reason, Data} | {continue, Data2}

proto_unlinked(Data, Msg) ->
    % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    {linked, Data}. % {unlinked, Data} {error, Reason, Data} | {continue, Data2}

proto_linked(Data, Msg) ->
    % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    {linked, Data}. % {unlinked, Data} {error, Reason, Data} | {continue, Data2}


% Call Managment Functions

call_add(Data = #data{pending_calls = PendingCalls}, Tag, From) ->
    CallList = maps:get(Tag, PendingCalls, []),
    Data#data{pending_calls = PendingCalls#{Tag => [Tag | CallList]}}.

call_reply(Data = #data{pending_calls = PendingCalls}, Tag, Reply) ->
    case maps:take(Tag, PendingCalls) ->
        error -> Data;
        {CallList, PendingCalls2} ->
            lists:foreach(fun(From) ->
                gen_statem:reply(From, Reply)
            end, CallList),
            Data#data{pending_calls = PendingCalls2}
    end.


% State-aware Functions

statem_filter_reply(Actions) ->
    [R || {reply, _From, _Reply} <- Actions].

statem_stop(Data, Reason, Actions) ->
    case statem_filter_reply(Actions) of
        [] -> {stop, REason, Data};
        Replies -> {stop_and_reply, Reason, Replies, Data}
    end.

statem_enter(Data, State, Actions),
    case features_enter(Data, State) of
        {ok, Data2} -> {keep_state, Data2, Actions};
        {error, Reason} -> statem_stop(Data, Reason, Actions)
    end.

statem_connection_message(Data, State, Msg, Handler) ->
    case Handler(Data, Msg) of
        {error, Reason, Data2} ->
            ?LOG_WARNING("Protocol error with grisp.io: ~p", [Reason],
                 #{event => protocol_error, linked => State =:= linked,
                   reason => Reason}),
            {next_state, waiting_ip, conn_close(Data2, Reason)};
        {continue, Data2} ->
            {keep_state, Data2};
        {NextState, Data2} when NextState =:= State ->
            {keep_state, Data2};
        {NextState, Data2} when NextState =:= linked, NextState =:= unlinked ->
            {next_state, NextState, Data2}
    end.


% Connection Functions

conn_start(Data = #data{conn = undefined,
                              domain = Domain,
                              port = Port,
                              ws_path = WsPath,
                              ws_transport = WsTransport,
                              ws_ping_timeout = WsPingTimeout,
                              ws_request_timeout = WsRequestTimeout,
                              error_list = ErrorList}) ->
    ConnTransport = case WsTransport of
        tcp -> tcp;
        tls -> {tls, grisp_cryptoauth_tls:options(Domain)}
    end,
    ConnOpts = #{
        domain => Domain,
        port => Port,
        transport => ConnTransport,
        path => WsPath,
        errors => ErrorList,
        ping_timeout => WsPingTimeout,
        request_timeout => WsRequestTimeout,
    },
    case grisp_connect_connection:start_link(self(), ConnOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Conn} -> {ok, Data#data{conn = Conn}}
    end.

% Safe to call in any state
conn_close(Data = #data{conn = undefined}) -> Data;
conn_close(Data = #data{conn = Conn}) ->
    grisp_connect_connection:disconnect(Conn),
    Data#data{conn = undefined}.

% Safe to call in any state
conn_died(Data) ->
    Data#data{conn = undefined}.

conn_post(Data = #data{conn = Conn}, Method, Params, OnResult, OnError)
  when Conn =/= undefined ->
    ReqCtx = #{on_result => OnResult, on_error => OnError},
    _ = grisp_connect_connection:post(Conn, Method, Params, ReqCtx),
    ok.

conn_notify(Data = #data{conn = Conn}, Method, Params)
  when Conn =/= undefined ->
    _ = grisp_connect_connection:notify(Conn, Method, Params),
    ok.

conn_error(Data = #data{conn = Conn}, Reason)
  when Conn =/= undefined ->
    _ = grisp_connect_connection:error(Conn, Reason, undefined,
                                       undefined, undefined),
    ok.

conn_error(Data = #data{conn = Conn}, Reason, ReqRef)
  when Conn =/= undefined ->
    _ = grisp_connect_connection:error(Conn, Reason, undefined,
                                       undefined, ReqRef),
    ok.

conn_reply(Data = #data{conn = Conn}, Result, ReqRef)
  when Conn =/= undefined ->
    _ = grisp_connect_connection:reply(Conn, Reslt, ReqRef),
    ok.


% Features Functions

features_init(Data, FeatureSpec) ->
    case features_init(Data, generic_errors(), [], FeatureSpec) of
        {error, _Reason} = Error -> Error;
        {ok, ErrorList, FeatMap} ->
            {ok, Data#data{error_list = ErrorList, features = FeatMap}}
    end.

features_init(Data, ErrorAcc, FeatAcc, []) ->
    {ok, lists:append(lists:reverse(ErrorAcc)), maps:from_list(FeatAcc)};
features_init(Data, ErrorAcc, FeatAcc, [{FeatMod, FeatOpts} | Rest])
  when is_atom(FeatMod), is_map(FeatOpts) ->
    case FeatMod:init(Data, FeatOpts) of
        {ok, FeatNamespace, FeatErrorList, FeatData} ->
            ErrorAcc2 = [FeatErrorList, ErrorAcc],
            FeatAcc2 = [{FeatMod, FeatData} | FeatAcc]
            features_init(ErrorAcc2, FeatAcc2, Rest);
        {error, Reason} ->
            features_terminate(Data, Reason, FeatAcc),
            {error, Reason}
    end;
features_init(Data, ErrorAcc, FeatAcc, [{FeatMod, FeatOpts} | Rest])
  when is_atom(FeatMod), is_list(FeatOpts) ->
    FeatOpts2 = proplists:to_map(FeatOpts),
    features_init(Data, ErrorAcc, FeatAcc, [{FeatMod, FeatOpts2} | Rest]);
features_init(Data, ErrorAcc, FeatAcc, [FeatMod | Rest])
  when is_atom(FeatMod) ->
    features_init(Data, ErrorAcc, FeatAcc, [{FeatMod, #{}} | Rest]).

features_terminate(Data = #data{features = FeatMap}, Reason) ->
    features_terminate(Data, Reason, maps:to_list(FeatMap)).

features_terminate(_Data, _Reason, []) -> ok;
features_terminate(Data, Reason, [{FeatMod, FeatData} | Rest]) ->
    FeatMod:terminate(Data, FeatData, Reason),
    features_terminate(Data, Reason, Rest).

features_enter(Data = #data{features = FeatMap}, State) ->
    case features_enter(Data, State, maps:to_list(FeatMap)) of
        {error, _Reason} = Error -> Error;
        {ok, FeatAcc} -> {ok, Data#data{features = maps:from_list(FeatAcc)}}
    end.

features_enter(Data, State, FeatList) ->
    features_enter(Data, State, FeatList, []).

features_enter(_Data, _State, [], FeatAcc) -> {ok, FeatAcc};
features_enter(Data, State, [{FeatMod, FeatData} | Rest], FeatAcc) ->
    case erlang:function_exported(FeatMod, enter, 3) of
        false ->
            features_enter(Data, State, Rest, FeatAcc);
        true ->
            case FeatMod:enter(Data, FeatData, State) of
                {error, _Reason} = Error -> Error;
                {ok, FeatData2} ->
                    FeatAcc2 = [{FeatMod, FeatData2} | FeatAcc],
                    features_enter(Data, State, Rest, FeatAcc2)
            end
    end.

%feature_on_request
% on_request(Client, State, Method, _Params, ReqRef) ->
%     ?LOG_WARNING("Unexpected request ~s to the system feature",
%                  [grisp_connect_feature:format_method(Method)],
%                  #{event => unexpected_request, feature => system,
%                    method => Method}),
%     grisp_connect_feature:error(Client, method_not_found, ReqRef),
%     {ok, State}.

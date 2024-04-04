%% @doc Client to interact with grisp.io
%%
%% This module contains a state machine to ensure connectivity with grisp.io.
%% JsonRPC traffic is managed here.
%% @end
-module(grisp_connect_client).

% External API
-export([start_link/0]).
-export([connect/0]).
-export([is_connected/0]).
-export([request/3]).

% Internal API
-export([disconnected/0]).
-export([handle_message/1]).

-behaviour(gen_statem).
-export([init/1, terminate/3, code_change/4, callback_mode/0, handle_event/4]).

-include_lib("kernel/include/logger.hrl").

-define(STD_TIMEOUT, 1000).

-record(data, {
    requests = #{}
}).

% API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

is_connected() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).

request(Method, Type, Params) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Method, Type, Params}).

disconnected() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

handle_message(Payload) ->
    gen_statem:cast(?MODULE, {?FUNCTION_NAME, Payload}).

% gen_statem CALLBACKS ---------------------------------------------------------

init([]) ->
    {ok, Connect} = application:get_env(grisp_connect, connect),
    NextState = case Connect of
        true -> waiting_ip;
        false -> idle
    end,
    {ok, NextState, #data{}}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

callback_mode() -> [handle_event_function, state_enter].

%%% STATE CALLBACKS ------------------------------------------------------------

% Generic events handling
handle_event(cast, connect, State, _Data) when State =/= idle ->
    keep_state_and_data;

handle_event({call, From}, is_connected, connected, _) ->
    {keep_state_and_data, [{reply, From, true}]};
handle_event({call, From}, is_connected, _, _) ->
    {keep_state_and_data, [{reply, From, false}]};

handle_event({call, From}, {request, _, _, _}, State, _Data)
when State =/= connected ->
    {keep_state_and_data, [{reply, From, {error, disconnected}}]};
handle_event({call, From}, {request, Method, Type, Params}, connected,
            #data{requests = Requests} = Data) ->
    {ID, Payload} = grisp_connect_api:request(Method, Type, Params),
    grisp_connect_ws:send(Payload),
    NewRequests = Requests#{ID => From},
    {keep_state,
     Data#data{requests = NewRequests},
     [{{timeout, ID}, request_timeout(), request}]};

handle_event(cast, {handle_message, Payload}, connected,
            #data{requests = Requests} = Data) ->
    Replies = grisp_connect_api:handle_msg(Payload),
    % A reduce operation is needed to support jsonrpc batch comunications
    case Replies of
        [] ->
            keep_state_and_data;
        [{request, Response}] -> % Response for a GRiSP.io request
            grisp_connect_ws:send(Response),
            keep_state_and_data;
        [{response, ID, Response}] -> % GRiSP.io response
            {OtherRequests, Actions} = dispatch_response(ID, Response, Requests),
            {keep_state, Data#data{requests = OtherRequests}, Actions}
    end;

handle_event({timeout, ID}, request, connected,
            #data{requests = Requests} = Data) ->
    Caller = maps:get(ID, Requests),
    {keep_state,
        Data#data{requests = maps:remove(ID, Requests)},
        [{reply, Caller, {error, timeout}}]};

handle_event({call, From}, _, State, Data) ->
    {keep_state, Data, [{reply, From, {bad_client_state, State}}]};

% STATE MACHINE Transitions

% IDLE
handle_event(enter, _OldState, idle, _Data) ->
    keep_state_and_data;
handle_event(cast, connect, idle, Data) ->
    {next_state, waiting_ip, Data};

% WAITING_IP
handle_event(enter, _OldState, waiting_ip, Data) ->
    {next_state, waiting_ip, Data, [{state_timeout, 0, retry}]};
handle_event(state_timeout, retry, waiting_ip, Data) ->
    case check_inet_ipv4() of
        {ok, IP} ->
            ?LOG_INFO(#{event => checked_ip, ip => IP}),
            {next_state, connecting, Data};
        invalid ->
            ?LOG_INFO(#{event => waiting_ip}),
            {next_state, waiting_ip, Data, [{state_timeout, ?STD_TIMEOUT, retry}]}
    end;

% CONNECTING
handle_event(enter, _OldState, connecting, _Data) ->
    {ok, Domain} = application:get_env(grisp_connect, domain),
    {ok, Port} = application:get_env(grisp_connect, port),
    ?LOG_NOTICE(#{event => connecting, domain => Domain, port => Port}),
    grisp_connect_ws:connect(Domain, Port),
    {keep_state_and_data, [{state_timeout, 0, retry}]};
handle_event(state_timeout, retry, connecting, Data) ->
    case grisp_connect_ws:is_connected() of
        true ->
            ?LOG_NOTICE(#{event => connected}),
            {next_state, connected, Data};
        false ->
            ?LOG_INFO(#{event => waiting_ws_connection}),
            {keep_state_and_data, [{state_timeout, ?STD_TIMEOUT, retry}]}
    end;

% CONNECTED
handle_event(enter, _OldState, connected, _Data) ->
    grisp_connect_log_server:start(),
    keep_state_and_data;
handle_event(cast, disconnected, connected, Data) ->
    ?LOG_WARNING(#{event => disconnected}),
    grisp_connect_log_server:stop(),
    {next_state, waiting_ip, Data};

handle_event(E, OldS, NewS, Data) ->
    ?LOG_ERROR(#{event => unhandled_gen_statem_event,
                 gen_statem_event => E,
                 old_state => OldS,
                 new_state => NewS}),
    {keep_state, Data}.

% INTERNALS --------------------------------------------------------------------

dispatch_response(ID, Response, Requests) ->
    case maps:take(ID, Requests) of
        {Caller, OtherRequests} ->
            Actions = [{{timeout, ID}, cancel}, {reply, Caller, Response}],
            {OtherRequests, Actions};
        error ->
            ?LOG_DEBUG(#{event => ?FUNCTION_NAME, reason => {missing_id, ID},
                         data => Response}),
            {Requests, []}
    end.

request_timeout() ->
    {ok, V} = application:get_env(grisp_connect, ws_requests_timeout),
    V.

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

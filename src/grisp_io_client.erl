%% @doc State machine to ensure connectivity with GRiSP.io
-module(grisp_io_client).

% External API
-export([start_link/0]).
-export([connect/0]).
-export([request/3]).

% Internal API
-export([handle_message/1]).

-behaviour(gen_statem).
-export([init/1, terminate/3, code_change/4, callback_mode/0, handle_event/4]).

-include_lib("kernel/include/logger.hrl").

-define(state_timeout, 1000).
-define(request_timeout, 5_000).

% Records

-record(data, {requests = #{}}).

% API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

request(Method, Type, Params) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Method, Type, Params}).


% TODO: make this function a cast to not block the WS gen_server
handle_message(Payload) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Payload}).


% gen_statem CALLBACKS ---------------------------------------------------------

init([]) ->
    {ok, Connect} = application:get_env(grisp_io, connect),
    NextState = case Connect of
        true -> waiting_ip;
        false -> idle
    end,
    {ok, NextState, #data{}}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

callback_mode() -> [handle_event_function, state_enter].

%%% STATE CALLBACKS ------------------------------------------------------------

handle_event({call, From}, {request, Method, Type, Params}, connected,
            #data{requests = Requests} = Data) ->
    {ID, Payload} = grisp_io_jsonrpc_api:request(Method, Type, Params),
    grisp_io_ws:send(Payload),
    NewRequests = Requests#{ID => From},
    {keep_state,
     Data#data{requests = NewRequests},
     [{{timeout, ID}, ?request_timeout, request}]};
handle_event({call, WebSocket}, {handle_message, Payload}, connected,
            #data{requests = Requests} = Data) ->
    Replyes = grisp_io_jsonrpc_api:handle_msg(Payload),
    % TODO: support jsonrpc batch communications,
    % needs handling of multiple gen_statem reply actions
    {NewData, Actions} = case Replyes of
        [] ->
            {Data, [{reply, WebSocket, noreply}]};
        [{request, Response}] ->
            {Data, [{reply, WebSocket, Response}]};
        [{response, ID, Response}] ->
            case maps:take(ID, Requests) of
                {Caller, NewRequests} ->
                    Acts = [
                        {{timeout, ID}, cancel},
                        {reply, Caller, Response},
                        {reply, WebSocket, noreply}],
                    {Data#data{requests = NewRequests}, Acts};
                error ->
                    ?LOG_WARNING("Unexpected jsonrpc response: ~p", [ID]),
                    {Data, [{reply, WebSocket, noreply}]}
            end
    end,
    {keep_state, NewData, Actions};
handle_event({timeout, ID}, request, connected,
            #data{requests = Requests} = Data) ->
    Caller = maps:get(ID, Requests),
    {keep_state,
        Data#data{requests = maps:remove(ID, Requests)},
        [{reply, Caller, {error, timeout}}]};
handle_event({call, From}, _, State, Data) ->
    {keep_state, Data, [{reply, From, {bad_client_state, State}}]};


handle_event(cast, Cast, State, _Data) ->
    ?LOG_WARNING("Unhandled cast in state ~p: ~p",[State, Cast]),
    keep_state_and_data;

%--- State Machine -------------------------------------------------------------

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
            ?LOG_INFO("Detected IP: ~p", [IP]),
            {next_state, connecting, Data};
        invalid ->
            ?LOG_INFO("Waiting IP..."),
            {next_state, waiting_ip, Data, [{state_timeout, ?state_timeout, retry}]}
    end;

% CONNECTING
handle_event(enter, _OldState, connecting, _Data) ->
    {ok, Domain} = application:get_env(grisp_io, grisp_io_domain),
    {ok, Port} = application:get_env(grisp_io, grisp_io_port),
    ?LOG_NOTICE("Connecting to ~p:~p",[Domain, Port]),
    grisp_io_ws:connect(),
    {keep_state_and_data, [{state_timeout, ?state_timeout, retry}]};
handle_event(state_timeout, retry, connecting, Data) ->
    case grisp_io_ws:is_connected() of
        true ->
            ?LOG_NOTICE("Connection enstablished!"),
            {next_state, connected, Data};
        false ->
            ?LOG_NOTICE("Waiting connection ..."),
            {keep_state_and_data, [{state_timeout, ?state_timeout, retry}]}
    end;

% CONNECTED
handle_event(enter, _OldState, connected, _Data) ->
    keep_state_and_data;
handle_event(info, disconnected, connected, Data) ->
    ?LOG_WARNING("Disconnected!"),
    {next_state, waiting_ip, Data};

handle_event(E, OldS, NewS, Data) ->
    ?LOG_ERROR("Unhandled Event = ~p, OldS = ~p, NewS = ~p",[E, OldS, NewS]),
    {keep_state, Data}.

% INTERNALS --------------------------------------------------------------------

check_inet_ipv4() ->
    case get_ip_of_valid_interfaces() of
        {_,_,_,_} = IP when IP =/= {127,0,0,1} -> {ok, IP};
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

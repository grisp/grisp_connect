%% @doc State machine to ensure connectivity with grisp.io
-module(grisp_io_connection).

% External API
-export([start_link/0]).
-export([connect/0]).

% Internal API
-export([disconnected/0]).

-behaviour(gen_statem).
-export([init/1, terminate/3, code_change/4, callback_mode/0, handle_event/4]).

-include_lib("kernel/include/logger.hrl").

-define(STD_TIMEOUT, 1000).

% API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_statem:cast(?MODULE, ?FUNCTION_NAME).

disconnected() ->
    gen_statem:castl(?MODULE, ?FUNCTION_NAME).

% gen_statem CALLBACKS ---------------------------------------------------------

init([]) ->
    {ok, Connect} = application:get_env(grisp_seawater, connect),
    NextState = case Connect of
        true -> waiting_ip;
        false -> idle
    end,
    {ok, NextState, []}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

callback_mode() -> [handle_event_function, state_enter].

%%% STATE CALLBACKS ------------------------------------------------------------

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
            {next_state, waiting_ip, Data, [{state_timeout, ?STD_TIMEOUT, retry}]}
    end;

% CONNECTING
handle_event(enter, _OldState, connecting, _Data) ->
    {ok, Domain} = application:get_env(grisp_seawater, seawater_domain),
    {ok, Port} = application:get_env(grisp_seawater, seawater_port),
    ?LOG_NOTICE("Connecting to ~p:~p",[Domain, Port]),
    {keep_state_and_data, [{state_timeout, 0, retry}]};
handle_event(state_timeout, retry, connecting, Data) ->
    case grisp_seawater_client:connect() of
        ok ->
            ?LOG_NOTICE(#{event => connected}),
            {next_state, connected, Data};
        Error ->
            ?LOG_ERROR("Connection failed with error ~p, Retryng ...",[Error]),
            {keep_state_and_data, [{state_timeout, ?STD_TIMEOUT, retry}]}
    end;

% CONNECTED
handle_event(enter, _OldState, connected, _Data) ->
    keep_state_and_data;
handle_event(cast, disconnected, connected, Data) ->
    ?LOG_WARNING(#{event => disconnected}),
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

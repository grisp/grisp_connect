%% @doc State machine to ensure connectivity with grisp.io
-module(grisp_io_connection).

% API
-export([start_link/0]).
-behaviour(gen_statem).
-export([init/1, terminate/3, code_change/4, callback_mode/0, handle_event/4]).

-include_lib("kernel/include/logger.hrl").

-define(STD_TIMEOUT, 1000).

% API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_statem CALLBACKS ---------------------------------------------------------

init([]) -> {ok, waiting_ip, []}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

callback_mode() -> [handle_event_function, state_enter].

%%% STATE CALLBACKS ------------------------------------------------------------

% WAITING_IP
handle_event(enter, _OldState, waiting_ip, Data) ->
    ?LOG_NOTICE("Waiting IP..."),
    {next_state, waiting_ip, Data, [{state_timeout, ?STD_TIMEOUT, retry}]};
handle_event(state_timeout, retry, waiting_ip, Data) ->
    case check_inet_ipv4() of
        true ->
            {next_state, connecting, Data};
        false ->
            {next_state, waiting_ip, Data, [{state_timeout, ?STD_TIMEOUT, retry}]}
    end;

% CONNECTING
handle_event(enter, _OldState, connecting, Data) ->
    {next_state, connecting, Data, [{state_timeout, ?STD_TIMEOUT, retry}]};
handle_event(state_timeout, retry, connecting, Data) ->
    {ok, Domain} = application:get_env(grisp_seawater, seawater_domain),
    {ok, Port} = application:get_env(grisp_seawater, seawater_port),
    ?LOG_NOTICE("Connecting to ~p:~p",[Domain, Port]),
    case grisp_seawater_client:connect() of
        ok ->
            ?LOG_NOTICE("Connection enstablished!"),
            {next_state, pinging, Data};
        {error, E} ->
            ?LOG_ERROR("Failed to connect to seawater: ~p",[E]),
            {next_state, connecting, Data, [{state_timeout, ?STD_TIMEOUT, retry}]}
    end;

% PINGING
handle_event(enter, _OldState, pinging, Data) ->
    {next_state, pinging, Data, [{state_timeout, ?STD_TIMEOUT, retry}]};
handle_event(state_timeout, retry, pinging, Data) ->
    case grisp_seawater_client:ping() of
        {ok, <<"pong">>} ->
            {next_state, connected, Data};
        {ok, <<"pang">>} ->
            ?LOG_WARNING("Device not linked!"),
            {next_state, connected, Data};
        {error, disconnected} ->
            {next_state, connecting, Data}
    end;

% CONNECTED
handle_event(enter, _OldState, connected, Data) ->
    {keep_state, Data};

handle_event( E, OldS, NewS, Data) ->
    ?LOG_ERROR("Unhandled Event = ~p, OldS = ~p, NewS = ~p",[E, OldS, NewS]),
    {keep_state, Data}.

% INTERNALS --------------------------------------------------------------------

check_inet_ipv4() ->
    case get_ip_of_valid_interfaces() of
        {_,_,_,_} = IP when IP =/= {127,0,0,1} -> true;
        _ -> false
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

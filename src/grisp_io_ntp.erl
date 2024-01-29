-module(grisp_io_ntp).

% API
-export([start_link/0]).
-export([get_time/0, get_time/1]).

-behaviour(gen_statem).
-export([init/1, terminate/3, code_change/4, callback_mode/0, handle_event/4]).

-define(NTP_PORT,       123).                   % udp
-define(SERVER_TIMEOUT, 5000).                  % ms
-define(EPOCH,          2208988800).            % offset yr 1900 to unix epochù
-define(RETRY_TIMEOUT, 1000).

-include_lib("kernel/include/logger.hrl").



% API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

get_time() ->
    get_time(random_ntp_server()).

get_time(Host) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Host}).

% gen_statem CALLBACKS ---------------------------------------------------------

init([]) -> {ok, waiting_ip, []}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

callback_mode() -> [handle_event_function, state_enter].

%%% STATE CALLBACKS ------------------------------------------------------------

handle_event({call, From}, {get_time, _}, State, Data) when State =/= ready ->
    {keep_state, Data, [{reply, From, {error, State}}]};

handle_event(enter, _OldState, ready, Data) ->
    {keep_state, Data};
handle_event({call, From}, {get_time, Host}, ready, Data) ->
    {keep_state, Data, [{reply, From, do_get_time(Host)}]};

handle_event(enter, _OldState, waiting_ip, Data) ->
    {next_state, waiting_ip, Data, [{state_timeout, ?RETRY_TIMEOUT, retry}]};
handle_event(state_timeout, retry, waiting_ip, Data) ->
    case check_inet_ipv4() of
        true ->
            ?LOG_INFO("ip detected, tryng to contact ntp server..."),
            {next_state, waiting_server, Data};
        false ->
            {next_state, waiting_ip, Data,
                                    [{state_timeout, ?RETRY_TIMEOUT, retry}]}
    end;

handle_event(enter, _OldState, waiting_server, Data) ->
    {next_state, waiting_server, Data,
                                [{state_timeout, ?RETRY_TIMEOUT, retry}]};
handle_event(state_timeout, retry, waiting_server, Data) ->
    try
        set_current_time(),
        ?LOG_INFO("Grisp clock set!"),
        {next_state, ready, Data}
    catch
        Ex:Er ->
            ?LOG_ERROR("ntp request failed: ~p, ~p",[Ex,Er]),
            {next_state, waiting_server, Data,
                                    [{state_timeout, ?RETRY_TIMEOUT, retry}]}
    end;

handle_event( E, OldS, NewS, Data) ->
    ?LOG_WARNING("Unhandled Event = ~p, OldS = ~p, NewS = ~p",[E, OldS, NewS]),
    {keep_state, Data}.

% INTERNALS --------------------------------------------------------------------

set_current_time() ->
    Time = do_get_time(random_ntp_server()),
    RefTS1970 = round(proplists:get_value(receiveTimestamp, (tuple_to_list(Time)))),
    CurrSecs = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) + RefTS1970,
    CurrDateTime = calendar:gregorian_seconds_to_datetime(CurrSecs),
    grisp_rtems:clock_set({CurrDateTime, 0}).

ntp_servers() ->
    ["0.europe.pool.ntp.org"].

random_ntp_server() ->
    lists:nth(rand:uniform(length(ntp_servers())), ntp_servers()).

do_get_time(Host) ->
    Resp = ntp_request(Host, create_ntp_request()),
    process_ntp_response(Resp) .

ntp_request(Host, Binary) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Host, ?NTP_PORT, Binary),
    {ok, {_Address, _Port, Resp}} = gen_udp:recv(Socket, 0, 500),
    gen_udp:close(Socket),
    Resp.

process_ntp_response(Ntp_response) ->
    <<LI:2, Version:3, Mode:3, Stratum:8, Poll:8/signed, Precision:8/signed,
        RootDel:32, RootDisp:32, R1:8, R2:8, R3:8, R4:8, RtsI:32, RtsF:32,
        OtsI:32, OtsF:32, RcvI:32, RcvF:32, XmtI:32, XmtF:32>> = Ntp_response,
  {NowMS, NowS, NowUS} = erlang:timestamp(),
  NowTimestamp = NowMS * 1.0e6 + NowS + NowUS/1000,
  TransmitTimestamp = XmtI - ?EPOCH + binfrac(XmtF),
    {{li, LI}, {vn, Version}, {mode, Mode}, {stratum, Stratum},
        {poll, Poll}, {precision, Precision},
        {rootDelay, RootDel}, {rootDispersion, RootDisp},
        {referenceId, R1, R2, R3, R4},
        {referenceTimestamp, RtsI - ?EPOCH + binfrac(RtsF)},
        {originateTimestamp, OtsI - ?EPOCH + binfrac(OtsF)},
        {receiveTimestamp,   RcvI - ?EPOCH + binfrac(RcvF)},
        {transmitTimestamp,  TransmitTimestamp},
        {clientReceiveTimestamp, NowTimestamp},
        {offset, TransmitTimestamp - NowTimestamp}}.

create_ntp_request() ->
    << 0:2, 4:3, 3:3,  0:(3*8 + 3*32 + 4*64) >>.

binfrac(Bin) ->
    binfrac(Bin, 2, 0).
binfrac(0, _, Frac) ->
    Frac;
binfrac(Bin, N, Frac) ->
    binfrac(Bin bsr 1, N*2, Frac + (Bin band 1)/N).

% INET IP CHECK UTILS ----------------------------------------------------------

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

-module(grisp_connect_ntp).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").


%--- Exports -------------------------------------------------------------------

% API functions
-export([start_link/0]).
-export([get_time/0, get_time/1]).

% Behaviour gen_statem callback functions
-export([callback_mode/0]).
-export([init/1]).

% Behaviour gen_statem states callback functions
-export([waiting_ip/3]).
-export([refresh_time/3]).
-export([ready/3]).


%--- Types ---------------------------------------------------------------------

-record(data, {
    retry_count = 0 :: non_neg_integer()
}).


%--- Macros --------------------------------------------------------------------

-define(NTP_PORT, 123). % NTP's UDP port
-define(EPOCH, 2208988800). % offset yr 1900 to unix epochù

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(EventType, EventContent, Data) ->
        handle_common(EventType, EventContent, ?FUNCTION_NAME, Data)).


%--- API FUNCTIONS -------------------------------------------------------------

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

get_time() ->
    get_time(random_ntp_server()).

get_time(Host) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Host}).


%--- BEHAVIOUR gen_statem CALLBACK FUNCTIONS -----------------------------------

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, waiting_ip, #data{}}.



%--- BEHAVIOUR gen_statem STATES CALLBACK FUNCTIONS -----------------------------

waiting_ip(enter, _OldState, _Data) ->
    % First IP check do not have any delay
    {keep_state_and_data, [{state_timeout, 0, check_ip}]};
waiting_ip(state_timeout, check_ip, Data) ->
    case grisp_connect_utils:check_inet_ipv4() of
        {ok, _IP} -> {next_state, refresh_time, Data};
        invalid -> {keep_state_and_data, [{state_timeout, 1000, check_ip}]}
    end;
?HANDLE_COMMON.

refresh_time(enter, _OldState, #data{retry_count = RetryCount}) ->
    Delay = grisp_connect_utils:retry_delay(RetryCount),
    {keep_state_and_data, [{state_timeout, Delay, request_time}]};
refresh_time(state_timeout, request_time,
             Data = #data{retry_count = RetryCount}) ->
    NTPServer = random_ntp_server(),
    case refresh_current_time(NTPServer) of
        {error, Reason} ->
            ?LOG_INFO("Failed to get time from NTP server ~s: ~w",
                      [NTPServer, Reason]),
            {next_state, waiting_ip, Data#data{retry_count = RetryCount + 1}};
        {ok, Datetime} ->
            ?LOG_INFO("GRiSP clock set from NTP to ~s",
                      [format_datetime(Datetime)]),
            {next_state, ready, Data#data{retry_count = 0}}
    end;
?HANDLE_COMMON.

ready(enter, _OldState, _Data) ->
    Period = refresh_period(),
    ?LOG_DEBUG("Schedule NTP time refresh in ~w seconds", [Period]),
    {keep_state_and_data, [{state_timeout, Period * 1000, refresh_time}]};
ready({call, From}, {get_time, Host}, _Data) ->
    Reply = case do_get_time(Host) of
        {error, _Reason} = Error -> Error;
        {ok, Time} -> {ok, Time}
    end,
    {keep_state_and_data, [{reply, From, Reply}]};
ready(state_timeout, refresh_time, Data) ->
    {next_state, refresh_time, Data};
?HANDLE_COMMON.

handle_common({call, _From}, {get_time, _Host}, _State, _Data) ->
    {keep_state_and_data, [postpone]};
handle_common({call, From}, Msg, State, _Data) ->
    ?LOG_WARNING("Unexpected call from ~w in state ~w: ~w",
                 [From, State, Msg]),
    {keep_state_and_data, [{reply, From, {error, unexpected_call}}]};
handle_common(cast, Msg, State, _Data) ->
    ?LOG_WARNING("Unexpected cast in state ~w: ~w", [State, Msg]),
    keep_state_and_data;
handle_common(info, Msg, State, _Data) ->
    ?LOG_DEBUG("Unexpected message in state ~w: ~w", [State, Msg]),
    keep_state_and_data.


%--- INTERNAL FUNCTIONS --------------------------------------------------------

format_datetime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    iolist_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Min, Sec]
    )).

refresh_current_time(NTPServer) ->
    case do_get_time(NTPServer) of
        {error, _Reason} = Error -> Error;
        {ok, Time} ->
            try
                RefTS1970 = round(proplists:get_value(receiveTimestamp, (tuple_to_list(Time)))),
                CurrSecs = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) + RefTS1970,
                CurrDateTime = calendar:gregorian_seconds_to_datetime(CurrSecs),
                grisp_rtems:clock_set({CurrDateTime, 0}),
                {ok, CurrDateTime}
            catch
                _:Reason -> {error, Reason}
            end
    end.

ntp_servers() ->
    {ok, NTPServers} = application:get_env(grisp_connect, ntp_servers),
    NTPServers.

% NTP refresh perido in seconds
refresh_period() ->
    {ok, Period} = application:get_env(grisp_connect, ntp_refresh_period),
    Period.

random_ntp_server() ->
    lists:nth(rand:uniform(length(ntp_servers())), ntp_servers()).

do_get_time(Host) ->
    case ntp_request(Host, create_ntp_request()) of
        {error, _Reason} = Error -> Error;
        {ok, Resp} ->
            try process_ntp_response(Resp) of
                Time -> {ok, Time}
            catch _:Reason ->
                {error, Reason}
            end
        end.

ntp_request(Host, Binary) ->
    case gen_udp:open(0, [binary, {active, false}]) of
        {error, _Reason} = Error -> Error;
        {ok, Socket} ->
            try gen_udp:send(Socket, Host, ?NTP_PORT, Binary) of
                {error, _Reason} = Error -> Error;
                ok ->
                    case gen_udp:recv(Socket, 0, 500) of
                        {error, _Reason} = Error -> Error;
                        {ok, {_Address, _Port, Resp}} -> {ok, Resp}
                    end
            after
                gen_udp:close(Socket)
            end
        end.

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

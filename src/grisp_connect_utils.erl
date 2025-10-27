-module(grisp_connect_utils).


%--- Exports -------------------------------------------------------------------

% API functions
-export([using_grisp_netman/0]).
-export([retry_delay/1]).
-export([check_inet_ipv4/0]).


%--- API Functions -------------------------------------------------------------

using_grisp_netman() ->
    RunningApps = application:which_applications(),
    lists:keymember(grisp_netman, 1, RunningApps).

check_inet_ipv4() ->
    case get_ip_of_valid_interfaces() of
        {ok, {IP1, _, _, _} = IP} when IP1 =/= 127 -> {ok, IP};
        _ -> invalid
    end.


%--- Internal Functions --------------------------------------------------------

retry_delay(0) ->
    0;
retry_delay(RetryCount) ->
    %% Calculate the connection delay in milliseconds with exponential backoff.
    %% The delay is selected randomly between `1000' and
    %% `2 ^ RETRY_COUNT - 1000' with a maximum value of `64000'.
    %% Loosely inspired by https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
    MinDelay = 1000,
    MaxDelay = 64000,
    MaxRandomDelay = min(MaxDelay, (1 bsl RetryCount) * 1000) - MinDelay,
    MinDelay + rand:uniform(MaxRandomDelay).

get_ipv4_from_opts([]) ->
    undefined;
get_ipv4_from_opts([{addr, {_1, _2, _3, _4}} | _]) ->
    {ok, {_1, _2, _3, _4}};
get_ipv4_from_opts([_ | TL]) ->
    case get_ipv4_from_opts(TL) of
        {ok, _IP} = Result -> Result;
        Other -> Other
    end.

has_ipv4(Opts) ->
    get_ipv4_from_opts(Opts) =/= undefined.

flags_are_ok(Flags) ->
    lists:member(up, Flags) and
        lists:member(running, Flags) and
        not lists:member(loopback, Flags).

get_valid_interfaces() ->
    case inet:getifaddrs() of
        {error, _Reason} = Error -> Error;
        {ok, Interfaces} ->
            {ok, [Opts || {_Name, [{flags, Flags} | Opts]} <- Interfaces,
                          flags_are_ok(Flags), has_ipv4(Opts)]}
    end.

get_ip_of_valid_interfaces() ->
    case get_valid_interfaces() of
        {error, _Reason} = Error -> Error;
        {ok, [Opts | _]} -> get_ipv4_from_opts(Opts);
        _ -> undefined
    end.

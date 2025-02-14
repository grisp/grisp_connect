%% @doc Test helper functions for the client setup to connect to the server
%% @end

-module(grisp_connect_test_client).

-export([cert_dir/0]).
-export([serial_number/0]).
-export([wait_connection/0]).
-export([wait_connection/1]).
-export([wait_disconnection/0]).
-export([wait_disconnection/1]).

%--- API -----------------------------------------------------------------------

cert_dir() -> filename:join([code:lib_dir(grisp_connect), "test", "certs"]).

serial_number() -> <<"0000">>.

wait_connection() ->
    grisp_connect_client:wait_connected(30_000).

wait_connection(0) ->
    {error, timeout};
wait_connection(Timeout) ->
    grisp_connect_client:wait_connected(Timeout).

wait_disconnection() ->
    wait_disconnection(30_000).

wait_disconnection(0) ->
    {error, timeout};
wait_disconnection(N) ->
    case grisp_connect:is_connected() of
        true ->
            ct:sleep(1),
            wait_disconnection(N - 1);
        false -> ok
    end.

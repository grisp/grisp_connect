%% @doc Test helper functions for the client setup to connect to grisp_manager
%% @end

-module(grisp_connect_test_client).

-export([cert_dir/0]).
-export([serial_number/0]).
-export([wait_connection/0]).

%--- API -----------------------------------------------------------------------

cert_dir() -> filename:join(code:lib_dir(grisp_connect, test), "certs"). 

serial_number() -> <<"0000">>.

wait_connection() ->
    wait_connection(20).

wait_connection(0) ->
    {error, timeout};
wait_connection(N) ->
    case grisp_connect:is_connected() of
       true -> ok;
       false ->
           ct:sleep(100),
           wait_connection(N - 1)
    end.

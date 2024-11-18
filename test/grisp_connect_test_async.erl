%% @doc Helper function for asynchronous function calls
%% @end

-module(grisp_connect_test_async).

-export([async_eval/1]).
-export([async_get_result/1]).

%--- API -----------------------------------------------------------------------

async_eval(Fun) ->
    spawn_link(
      fun() ->
              Res = Fun(),
              receive
                  {'$async_get_result', Pid} -> Pid ! {'$async_result', Res}
              end
      end).

async_get_result(Pid) ->
    MRef = monitor(process, Pid),
    unlink(Pid),
    Pid ! {'$async_get_result', self()},
    receive
        {'$async_result', Res} ->
            receive {'DOWN', MRef, process, Pid, normal} -> ok
            after 10 -> error({timeout, waiting_exit})
            end,
            Res;
        {'DOWN', MRef, process, Pid, Reason} ->
            error({process_down, Reason})
    after 1000 ->
            error({timeout, waiting_result})
    end.

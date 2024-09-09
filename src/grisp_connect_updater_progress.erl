-module(grisp_connect_updater_progress).
-behaviour(grisp_updater_progress).

%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").


%--- Types ---------------------------------------------------------------------


%--- Exports -------------------------------------------------------------------


% Behaviour grisp_updater_progress callbacks
-export([progress_init/1]).
-export([progress_update/2]).
-export([progress_warning/3]).
-export([progress_error/3]).
-export([progress_done/2]).


%--- records -------------------------------------------------------------------

-record(state, {
    client :: pid(),
    last_notification :: undefined | integer()
}).


%--- API Functions -------------------------------------------------------------


%--- Behavior grisp_updater_progress Callback ----------------------------------

progress_init(#{client := PID} = _Opts) ->
    {ok, #state{
        last_notification = erlang:system_time(millisecond),
        client = PID
    }}.

progress_update(#state{last_notification = LastLog} = State, Stats) ->
    case (erlang:system_time(millisecond) - LastLog) > 1000 of
        false -> {ok, State};
        true ->
            % Recheck log level when there is another way to check the progress update
            ?LOG_NOTICE("Update progress: ~b%", [progress_percent(Stats)]),
            {ok, State#state{last_notification = erlang:system_time(millisecond)}}
    end.

progress_warning(State, Msg, Reason) ->
    ?LOG_WARNING("Update warning; ~s: ~p", [Msg, Reason]),
    {ok, State}.

progress_error(#state{}, Stats, Reason) ->
    ?LOG_ERROR("Update failed after ~b% : ~p",
               [progress_percent(Stats), Reason]),
    ok.

progress_done(#state{}, _Stats) ->
    ?LOG_NOTICE("Update done", []),
    ok.


%--- Internal Functions --------------------------------------------------------

progress_percent(Stats) ->
    #{data_total := Total, data_checked := Checked,
      data_skipped := Skipped, data_written := Written} = Stats,
    (Checked + Skipped + Written) * 100 div (Total * 2).

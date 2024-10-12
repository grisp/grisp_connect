%% @doc Library module containing grisp_updater helper functions
-module(grisp_connect_updater).


%--- INCLUDES ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").


%--- EXPORTS -------------------------------------------------------------------

-export([is_available/0]).
-export([system_info/0]).
-export([start_update/1]).
-export([validate/0]).
-export([cancel/0]).


%--- API -----------------------------------------------------------------------

is_available() ->
    is_running(grisp_updater).

system_info() ->
    RelInfo = try release_handler:which_releases() of
        [{RelName, RelVsn, _, _}] ->
            #{relname => list_to_binary(RelName),
              relvsn => list_to_binary(RelVsn)}
    catch
        exit:{noproc, _} ->
            % Running in a shell
            #{relname => null, relvsn => null};
        error:undef ->
            % Sasl is not running
            #{relname => null, relvsn => null}
    end,
    UpdateInfo = update_info(),
    maps:merge(RelInfo, UpdateInfo).

start_update(URL) ->
    case is_running(grisp_updater) of
        true -> grisp_updater:start(URL,
                                    grisp_connect_updater_progress,
                                    #{client => self()}, #{});
        false -> {error, grisp_updater_unavailable}
    end.

validate() ->
    case is_running(grisp_updater) of
        true -> grisp_updater:validate();
        false -> {error, grisp_updater_unavailable}
    end.

cancel() ->
    case is_running(grisp_updater) of
        true -> grisp_updater:cancel();
        false -> {error, grisp_updater_unavailable}
    end.


%--- INTERNAL FUNCTIONS --------------------------------------------------------

is_running(AppName) ->
    Apps = application:which_applications(),
    case [App || {App, _Desc, _VSN} <- Apps, App =:= AppName] of
        [] -> false;
        [_] -> true
    end.

update_info() ->
    case is_running(grisp_updater) of
        false ->
            #{update_enabled => false};
        true ->
            Info = grisp_updater:info(),
            Status = grisp_updater:status(),
            #{boot := Boot, valid := Valid, next := Next} = Info,
            case {Status, Boot, Valid, Next} of
                % Ready for update from removable media
                {ready,
                 #{type := removable} = Boot,
                 #{type := system, id := ValidId},
                 #{type := system, id := NextId}}
                    when ValidId =:= NextId ->
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => ready,
                          update_message => <<"Device ready for update">>};
                % Ready for update from valid system
                {ready,
                 #{type := system, id := BootId} = Boot,
                 #{type := system, id := ValidId},
                 #{type := system, id := NextId}}
                    when ValidId =:= NextId, ValidId =:= BootId ->
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => ready,
                          update_message => <<"Device ready for update">>};
                % Updating
                {{updating, Stats}, Boot, _, _} ->
                        #{data_total := Total, data_checked := Checked,
                          data_skipped := Skipped, data_written := Written} = Stats,
                        Percent = (Checked + Skipped + Written) * 100 div (Total * 2),
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => updating,
                          update_progress => Percent,
                          update_message => <<"Device is updating">>};
                % Update Failed
                {{error, canceled}, Boot, _, _} ->
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => canceled,
                          update_message => <<"Device update canceled">>};
                % Update Failed
                {{error, _Reason}, Boot, _, _} ->
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => failed,
                          update_message => <<"Device update failed">>};
                % Update succeed
                {{success, _Stats},
                 Boot,
                 #{type := system, id := ValidId},
                 #{type := system, id := NextId}}
                    when ValidId =/= NextId ->
                        #{update_enabled => true,
                          update_status => updated,
                          boot_source => Boot,
                          update_message => <<"Device updated, reboot required to validate the update">>,
                          action_required => reboot};
                % Booted from removable after update
                {ready,
                 #{type := removable} = Boot,
                 #{type := system, id := ValidId},
                 #{type := system, id := NextId}}
                    when ValidId =/= NextId ->
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => updated,
                          update_message => <<"Device updated but the SD card wasn't removed before rebooting">>,
                          action_required => remove_sdcard_and_reboot};
                % Updated and rebooted
                {ready,
                 #{type := system, id := TestId} = Boot,
                 #{type := system, id := ValidId},
                 #{type := system, id := NextId}}
                    when ValidId =/= TestId, ValidId =:= NextId ->
                        #{update_enabled => true,
                          boot_source => Boot,
                          update_status => updated,
                          update_message => <<"Device updated, validation required">>,
                          action_required => validate};
                _ ->
                    #{update_enabled => true}
            end
    end.

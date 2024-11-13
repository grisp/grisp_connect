-module(grisp_connect_reconnect_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

-import(grisp_connect_test_client, [wait_connection/0]).
-import(grisp_connect_test_client, [wait_connection/1]).
-import(grisp_connect_test_client, [wait_disconnection/0]).
-import(grisp_connect_test_client, [wait_disconnection/1]).
-import(grisp_connect_test_client, [serial_number/0]).
-import(grisp_connect_test_client, [cert_dir/0]).

-import(grisp_connect_test_server, [start_cowboy/1]).
-import(grisp_connect_test_server, [stop_cowboy/0]).
-import(grisp_connect_test_server, [close_websocket/0]).
-import(grisp_connect_test_server, [flush/0]).

%--- API -----------------------------------------------------------------------

all() ->
    [
        F
        ||
        {F, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(F))
    ].

init_per_suite(Config) ->
    {ok, Apps} = application:ensure_all_started(cowboy),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    [?assertEqual(ok, application:stop(App)) || App <- ?config(apps, Config)].

init_per_testcase(_, Config) ->
    start_cowboy(cert_dir()),
    {ok, _} = application:ensure_all_started(grisp_emulation),
    application:set_env(grisp_connect, ws_ping_timeout, 120_000),
    {ok, _} = application:ensure_all_started(grisp_connect),
    Config.

end_per_testcase(_, Config) ->
    ok = application:stop(grisp_connect),
    stop_cowboy(),
    flush(),
    Config.

%--- Tests ---------------------------------------------------------------------

reconnect_on_gun_crash_test(_) ->
    ?assertMatch(ok, wait_connection(100)),
    {state, GunPid, _, _, _, _} = sys:get_state(grisp_connect_ws),
    proc_lib:stop(GunPid),
    ?assertMatch(ok, wait_disconnection()),
    ?assertMatch(ok, wait_connection()).

reconnect_on_disconnection_test(Config) ->
    ?assertMatch(ok, wait_connection()),
    stop_cowboy(),
    ?assertMatch(ok, wait_disconnection()),
    start_cowboy(cert_dir()),
    ?assertMatch(ok, wait_connection(100)),
    Config.

reconnect_on_ping_timeout_test(_) ->
    ?assertMatch(ok, wait_connection()),
    {state, GunPid, _, _, _, _} = sys:get_state(grisp_connect_ws),
    proc_lib:stop(GunPid),
    % Now decrease ping timeout so that the WS closes after just 1 second
    application:set_env(grisp_connect, ws_ping_timeout, 1500),
    ?assertMatch(ok, wait_disconnection()),
    ?assertMatch(ok, wait_connection(150)),
    ?assertMatch(ok, wait_disconnection()),
    ?assertMatch(ok, wait_connection(150)),
    ?assertMatch(ok, wait_disconnection()).

reconnect_on_closed_frame_test(_) ->
    ?assertMatch(ok, wait_connection()),
    close_websocket(),
    ?assertMatch(ok, wait_disconnection()),
    ?assertMatch(ok, wait_connection(100)).

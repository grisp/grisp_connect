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

%--- API -----------------------------------------------------------------------

all() ->
    [
        F
        ||
        {F, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(F))
    ].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    CertDir = cert_dir(),

    PolicyFile = filename:join(PrivDir, "policies.term"),
    ?assertEqual(ok, file:write_file(PolicyFile, <<>>)),
    application:set_env(seabac, policy_file, PolicyFile),

    Config2 = grisp_connect_manager:start(Config),
    [{cert_dir, CertDir} | Config2].

end_per_suite(Config) ->
    grisp_connect_manager:cleanup_apps(?config(apps, Config)).

init_per_testcase(_, Config) ->
    % the kraf instance links to this process
    process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(kraft),
    KraftRef = grisp_connect_manager:kraft_start(?config(cert_dir, Config)),
    {ok, _} = application:ensure_all_started(grisp_emulation),
    application:set_env(grisp_connect, test_cert_dir, ?config(cert_dir, Config)),
    {ok, _} = application:ensure_all_started(grisp_connect),
    [{kraft_instance, KraftRef} | Config].

end_per_testcase(_, Config) ->
    ok = application:stop(grisp_connect),
    kraft:stop(?config(kraft_instance, Config)),
    ok = application:stop(kraft),
    mnesia:activity(transaction, fun() ->
        mnesia:delete({grisp_device, serial_number()})
    end),
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
    ok = kraft:stop(?config(kraft_instance, Config)),
    ?assertMatch(ok, wait_disconnection()),
    KraftRef2 = grisp_connect_manager:kraft_start(cert_dir()),
    ?assertMatch(ok, wait_connection(100)),
    [{kraft_instance, KraftRef2} | proplists:delete(kraft_instance, Config)].

reconnect_on_ping_timeout_test(_) ->
    ?assertMatch(ok, wait_connection()),
    {state, GunPid, _, _, _, _} = sys:get_state(grisp_connect_ws),
    proc_lib:stop(GunPid),
    % Now decrease ping timeout so that the WS closes after just 1 second
    application:set_env(grisp_connect, ws_ping_timeout, 1000),
    ?assertMatch(ok, wait_disconnection()),
    ?assertMatch(ok, wait_connection(100)),
    ?assertMatch(ok, wait_disconnection()),
    ?assertMatch(ok, wait_connection(100)),
    ?assertMatch(ok, wait_disconnection()).

%--- Internal ------------------------------------------------------------------

flush() ->
    receive Any -> ct:pal("Flushed: ~p", [Any]), flush()
    after 0 -> ok
    end.

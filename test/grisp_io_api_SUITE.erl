-module(grisp_io_api_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

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
    DataDir = ?config(data_dir, Config),
    CertDir = grisp_io_manager:get_cert_dir(DataDir),

    PolicyFile = filename:join(PrivDir, "policies.term"),
    ?assertEqual(ok, file:write_file(PolicyFile, <<>>)),
    application:set_env(seabac, policy_file, PolicyFile),

    Apps = grisp_io_manager:start(PrivDir, CertDir),
    [{cert_dir, CertDir} | [{apps, Apps} | Config]].

end_per_suite(Config) ->
    grisp_io_manager:cleanup_apps(?config(apps, Config)).

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(grisp_emulation),
    application:set_env(grisp_io, test_cert_dir, ?config(cert_dir, Config)),
    {ok, _} = application:ensure_all_started(grisp_io),
    Config.

end_per_testcase(_, Config) ->
    ok = application:stop(grisp_io),
    mnesia:activity(transaction, fun() ->
        mnesia:delete({grisp_device, <<"0000">>})
    end),
    flush(),
    Config.

%--- Tests ---------------------------------------------------------------------

auto_connect_test(_) ->
    assert_connection().

ping_test(_) ->
    assert_connection(),
    ?assertMatch({ok, <<"pang">>}, grisp_io:ping()).

link_device_test(_) ->
    assert_connection(),
    ?assertMatch({error, token_undefined}, grisp_io:link_device()),
    application:set_env(grisp_io, device_linking_token, <<"token">>),
    ?assertMatch({error, invalid_token}, grisp_io:link_device()),
    Token = grisp_manager_token:get_token(<<"Testuser">>),
    application:set_env(grisp_io, device_linking_token, Token),
    ?assertMatch({ok, <<"ok">>}, grisp_io:link_device()),
    ?assertMatch({ok, <<"pong">>}, grisp_io:ping()).

%--- Internal ------------------------------------------------------------------

assert_connection() ->
    ct:sleep(1000),
    ?assert(grisp_io:is_connected()).

flush() ->
    receive Any -> ct:pal("Flushed: ~p", [Any]), flush()
    after 0 -> ok
    end.

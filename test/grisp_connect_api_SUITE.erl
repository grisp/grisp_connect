-module(grisp_connect_api_SUITE).

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
    CertDir =  filename:join(?config(data_dir, Config), "certs"),

    PolicyFile = filename:join(PrivDir, "policies.term"),
    ?assertEqual(ok, file:write_file(PolicyFile, <<>>)),
    application:set_env(seabac, policy_file, PolicyFile),

    Config2 = grisp_connect_manager:start(CertDir, Config),
    [{cert_dir, CertDir} | Config2].

end_per_suite(Config) ->
    grisp_connect_manager:cleanup_apps(?config(apps, Config)).

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(grisp_emulation),
    application:set_env(grisp_connect, test_cert_dir, ?config(cert_dir, Config)),
    {ok, _} = application:ensure_all_started(grisp_connect),
    Config.

end_per_testcase(_, Config) ->
    ok = application:stop(grisp_connect),
    mnesia:activity(transaction, fun() ->
        mnesia:delete({grisp_device, <<"0000">>})
    end),
    flush(),
    Config.

%--- Tests ---------------------------------------------------------------------

auto_connect_test(_) ->
    ?assertMatch(ok, wait_connection(20)).

ping_test(_) ->
    ?assertMatch(ok, wait_connection(20)),
    ?assertMatch({ok, <<"pang">>}, grisp_connect:ping()).

link_device_test(_) ->
    ?assertMatch(ok, wait_connection(20)),
    ?assertMatch({error, token_undefined}, grisp_connect:link_device()),
    application:set_env(grisp_connect, device_linking_token, <<"token">>),
    ?assertMatch({error, invalid_token}, grisp_connect:link_device()),
    Token = grisp_manager_token:get_token(<<"Testuser">>),
    application:set_env(grisp_connect, device_linking_token, Token),
    ?assertMatch({ok, <<"ok">>}, grisp_connect:link_device()),
    ?assertMatch({ok, <<"pong">>}, grisp_connect:ping()).

%--- Internal ------------------------------------------------------------------

wait_connection(0) ->
    {error, timeout};
wait_connection(N) ->
    case grisp_connect:is_connected() of
       true -> ok;
       false ->
           ct:sleep(100),
           wait_connection(N - 1)
    end.

flush() ->
    receive Any -> ct:pal("Flushed: ~p", [Any]), flush()
    after 0 -> ok
    end.

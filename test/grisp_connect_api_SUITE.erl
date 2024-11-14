-module(grisp_connect_api_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("grisp_connect_test.hrl").

-compile([export_all, nowarn_export_all]).

-import(grisp_connect_test_client, [wait_connection/0]).
-import(grisp_connect_test_client, [wait_connection/1]).
-import(grisp_connect_test_client, [wait_disconnection/0]).
-import(grisp_connect_test_client, [wait_disconnection/1]).
-import(grisp_connect_test_client, [serial_number/0]).
-import(grisp_connect_test_client, [cert_dir/0]).

-import(grisp_connect_test_server, [flush/0]).
-import(grisp_connect_test_server, [send_jsonrpc_result/2]).
-import(grisp_connect_test_server, [send_jsonrpc_error/3]).

%--- API -----------------------------------------------------------------------

all() ->
    [
        F
        ||
        {F, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(F))
    ].

init_per_suite(Config) ->
    CertDir = cert_dir(),
    Apps = grisp_connect_test_server:start(CertDir),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    [?assertEqual(ok, application:stop(App)) || App <- ?config(apps, Config)].

init_per_testcase(TestCase, Config) ->
    process_flag(trap_exit, true),
    {ok, _} = application:ensure_all_started(grisp_emulation),
    {ok, _} = application:ensure_all_started(grisp_connect),
    case TestCase of
        auto_connect_test -> ok;
        _ ->
            ?assertEqual(ok, wait_connection()),
            grisp_connect_test_server:listen()
    end,
    Config.

end_per_testcase(_, Config) ->
    ok = application:stop(grisp_connect),
    ?assertEqual([], flush()),
    Config.

%--- Tests ---------------------------------------------------------------------

auto_connect_test(_) ->
    ?assertMatch(ok, wait_connection()).

ping_test(_) ->
    Pid = ?asyncAssertEqual({ok, <<"pong">>}, grisp_connect:ping()),
    Id = ?receiveRequest(<<"post">>, #{type := <<"ping">>}),
    send_jsonrpc_result(<<"pong">>, Id),
    ?asyncWait(Pid).

link_device_test(_) ->
    ?assertMatch({error, token_undefined}, grisp_connect:link_device()),
    Token = <<"token">>,
    application:set_env(grisp_connect, device_linking_token, Token),

    % handle successful responses
    % ok
    Pid1 = ?asyncAssertEqual({ok, <<"ok">>},
                                      grisp_connect:link_device()),
    Id1 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_result(<<"ok">>, Id1),
    ?asyncWait(Pid1),
    % device_already_linked
    Pid2 = ?asyncAssertEqual({ok, <<"device_already_linked">>},
                             grisp_connect:link_device()),
    Id2 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_result(<<"device_already_linked">>,
                                                  Id2),
    ?asyncWait(Pid2),

    % handle error responses
    % device_already_linked
    Pid3 = ?asyncAssertEqual({error, device_already_linked},
                             grisp_connect:link_device()),
    Id3 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_error(-3, <<"device already linked">>, Id3),
    ?asyncWait(Pid3),
    % invalid_token
    Pid4 = ?asyncAssertEqual({error, invalid_token},
                             grisp_connect:link_device()),
    Id4 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_error(-4, <<"invalid token">>, Id4),
    ?asyncWait(Pid4).

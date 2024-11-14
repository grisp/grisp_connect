-module(grisp_connect_api_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("grisp_connect_test.hrl").

-compile([export_all, nowarn_export_all]).

-import(grisp_connect_test_async, [async_eval/1]).
-import(grisp_connect_test_async, [async_get_result/1]).

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
    Pid = async_eval(fun grisp_connect:ping/0),
    Id = ?receiveRequest(<<"post">>, #{type := <<"ping">>}),
    send_jsonrpc_result(<<"pong">>, Id),
    ?assertEqual({ok, <<"pong">>}, async_get_result(Pid)).

link_device_test(_) ->
    ?assertMatch({error, token_undefined}, grisp_connect:link_device()),
    Token = <<"token">>,
    application:set_env(grisp_connect, device_linking_token, Token),

    % handle successful responses
    % ok
    Pid1 = async_eval(fun grisp_connect:link_device/0),
    Id1 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_result(<<"ok">>, Id1),
    ?assertEqual({ok, <<"ok">>}, async_get_result(Pid1)),
    % device_already_linked
    Pid2 = async_eval(fun grisp_connect:link_device/0),
    Id2 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_result(<<"device_already_linked">>,
                                                  Id2),
    ?assertEqual({ok, <<"device_already_linked">>}, async_get_result(Pid2)),

    % handle error responses
    % device_already_linked
    Pid3 = async_eval(fun grisp_connect:link_device/0),
    Id3 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_error(-3, <<"device already linked">>, Id3),
    ?assertEqual({error, device_already_linked}, async_get_result(Pid3)),
    % invalid_token
    Pid4 = async_eval(fun grisp_connect:link_device/0),
    Id4 = ?receiveRequest(<<"post">>,
                          #{type := <<"device_linking_token">>,
                            token := Token}),
    send_jsonrpc_error(-4, <<"invalid token">>, Id4),
    ?assertEqual({error, invalid_token}, async_get_result(Pid4)).

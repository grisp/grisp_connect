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
-import(grisp_connect_test_server, [send_jsonrpc_request/3]).

%--- API -----------------------------------------------------------------------

all() ->
    [
        F
        ||
        {F, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(F))
    ].

init_per_testcase(TestCase, Config)
  when TestCase =:= bad_client_version_test;
       TestCase =:= bad_server_version_test;
       TestCase =:= exponential_backoff_test  ->
    Config;
init_per_testcase(TestCase, Config) ->
    CertDir = cert_dir(),
    Apps = grisp_connect_test_server:start(#{cert_dir => CertDir}),
    {ok, _} = application:ensure_all_started(grisp_emulation),
    {ok, _} = application:ensure_all_started(grisp_connect),
    case TestCase of
        auto_connect_test -> ok;
        _ ->
            ?assertEqual(ok, wait_connection()),
            grisp_connect_test_server:listen()
    end,
    [{apps, Apps} | Config].

end_per_testcase(TestCase, Config)
    when TestCase =:= bad_client_version_test;
         TestCase =:= bad_server_version_test;
         TestCase =:= exponential_backoff_test  ->
      Config;
end_per_testcase(_, Config) ->
    ok = application:stop(grisp_connect),
    grisp_connect_test_server:wait_disconnection(),
    ?assertEqual([], flush()),
    grisp_connect_test_server:stop(proplists:get_value(apps, Config)),
    Config.

%--- Tests ---------------------------------------------------------------------

exponential_backoff_test(_) ->
    CertDir = cert_dir(),
    CallRef = make_ref(),
    Self = self(),

    % First we test the exponential backoff algorithm when failing to connect

    Apps = grisp_connect_test_server:start(#{
        cert_dir => CertDir,
        init_callback => fun(Req, Opts) ->
            Self ! {CallRef, init, os:timestamp()},
            {ok, cowboy_req:reply(400, #{}, <<"Canceled">>, Req), Opts}
        end}),

    {ok, _} = application:ensure_all_started(grisp_emulation),
    application:load(grisp_connect),
    {ok, OldConectEnv} = application:get_env(grisp_connect, connect),
    application:set_env(grisp_connect, connect, false),
    {ok, _} = application:ensure_all_started(grisp_connect),

    {T1, T2, T3, T4} = try
        T1a = os:timestamp(),
        grisp_connect:connect(),
        T2a = receive
            {CallRef, init, V2} -> V2
        after 300 ->
            erlang:error(timeout2)
        end,
        T3a = receive
            {CallRef, init, V3} -> V3
        after 2300 ->
            erlang:error(timeout3)
        end,
        T4a = receive
            {CallRef, init, V4} -> V4
        after 4300 ->
            erlang:error(timeout4)
        end,
        {T1a, T2a, T3a, T4a}
    catch
        C1:R1:S1 ->
            ok = application:stop(grisp_connect),
            application:set_env(grisp_connect, connect, OldConectEnv),
            erlang:raise(C1, R1, S1)
    after
        grisp_connect_test_server:wait_disconnection(),
        ?assertEqual([], flush()),
        grisp_connect_test_server:stop(Apps)
    end,

    % Then we allow the connection to succeed

    grisp_connect_test_server:start(#{
        cert_dir => CertDir,
        init_callback => fun(Req, Opts) ->
            Self ! {CallRef, init, os:timestamp()},
            Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, <<"grisp-io-v1">>, Req),
            {cowboy_websocket, Req2, Opts}
        end}),
    try
        T5 = receive
            {CallRef, init, V5} -> V5
        after 8300 ->
            erlang:error(timeout5)
        end,

        ?assertMatch(ok, wait_connection()),
        D1 = timer:now_diff(T2, T1) div 1000,
        D2 = timer:now_diff(T3, T2) div 1000,
        D3 = timer:now_diff(T4, T3) div 1000,
        D4 = timer:now_diff(T5, T4) div 1000,
        ?assert(D1 < 300, D1),
        ?assert(D2 > 1000, D2),
        ?assert(D2 < 100 + 1000 * 1 bsl 1, D2), % Extra 100 ms for safety
        ?assert(D3 > 1000, D3),
        ?assert(D3 < 100 + 1000 * 1 bsl 2, D3), % Extra 100 ms for safety
        ?assert(D4 > 1000, D4),
        ?assert(D4 < 100 + 1000 * 1 bsl 3, D4) % Extra 100 ms for safety

    catch
        C2:R2:S2 ->
            ok = application:stop(grisp_connect),
            application:set_env(grisp_connect, connect, OldConectEnv),
            erlang:raise(C2, R2, S2)
    after
        grisp_connect_test_server:stop(Apps),
        ?assertEqual([], flush())
    end,

    % Wait for grisp_connect to not be connected anymore
    fun WaitNotConnected() ->
        case grisp_connect:is_connected() of
            false -> ok;
            true ->
                timer:sleep(50),
                WaitNotConnected()
        end
    end(),
    T6 = os:timestamp(),

    % Finally we test the delay is reset when reconnecting

    grisp_connect_test_server:start(#{
        cert_dir => CertDir,
        init_callback => fun(Req, Opts) ->
            Self ! {CallRef, init, os:timestamp()},
            {ok, cowboy_req:reply(400, #{}, <<"Canceled">>, Req), Opts}
        end}),
    try
        T7 = receive
            {CallRef, init, V7} -> V7
        after 2300 ->
            erlang:error(timeout7)
        end,
        T8 = receive
            {CallRef, init, V8} -> V8
        after 4300 ->
            erlang:error(timeout8)
        end,
        D5 = timer:now_diff(T7, T6) div 1000,
        D6 = timer:now_diff(T8, T7) div 1000,
        ?assert(D5 > 1000, D5),
        ?assert(D5 < 100 + 1000 * 1 bsl 1, D5), % Extra 100 ms for safety
        ?assert(D6 > 1000, D6),
        ?assert(D6 < 100 + 1000 * 1 bsl 2, D6) % Extra 100 ms for safety
    after
        ok = application:stop(grisp_connect),
        application:set_env(grisp_connect, connect, OldConectEnv),
        grisp_connect_test_server:wait_disconnection(),
        ?assertEqual([], flush()),
        grisp_connect_test_server:stop(Apps)
    end,
    ok.

bad_client_version_test(_) ->
    CertDir = cert_dir(),
    Apps = grisp_connect_test_server:start(#{
        cert_dir => CertDir,
        expected_protocol => <<"grisp-io-v42">>}),
    try
        {ok, _} = application:ensure_all_started(grisp_emulation),
        application:load(grisp_connect),
        {ok, OldMaxRetryEnv} = application:get_env(grisp_connect, ws_max_retries),
        application:set_env(grisp_connect, ws_max_retries, 2),
        {ok, _} = application:ensure_all_started(grisp_connect),
        try
            ?assertMatch({error, ws_upgrade_failed}, wait_connection())
        after
            ok = application:stop(grisp_connect),
            application:set_env(grisp_connect, ws_max_retries, OldMaxRetryEnv)
        end
    after
        grisp_connect_test_server:wait_disconnection(),
        ?assertEqual([], flush()),
        grisp_connect_test_server:stop(Apps)
    end,
    ok.

bad_server_version_test(_) ->
    CertDir = cert_dir(),
    Apps = grisp_connect_test_server:start(#{
        cert_dir => CertDir,
        selected_protocol => <<"grisp-io-v42">>}),
    try
        {ok, _} = application:ensure_all_started(grisp_emulation),
        application:load(grisp_connect),
        {ok, OldMaxRetryEnv} = application:get_env(grisp_connect, ws_max_retries),
        application:set_env(grisp_connect, ws_max_retries, 2),
        {ok, _} = application:ensure_all_started(grisp_connect),
        try
            % There is no way to know the reason why gun closed the connection
            % but we don't want jarl to crash because the protocol couldn't be
            % negociated.
            ?assertMatch({error, normal}, wait_connection())
        after
            ok = application:stop(grisp_connect),
            application:set_env(grisp_connect, ws_max_retries, OldMaxRetryEnv)
        end
    after
        grisp_connect_test_server:wait_disconnection(),
        ?assertEqual([], flush()),
        grisp_connect_test_server:stop(Apps)
    end,
    ok.

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
    send_jsonrpc_result(<<"device_already_linked">>, Id2),
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

software_update_test(_) ->
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 1),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := false}, 1),
    send_jsonrpc_request(post, #{type => <<"start_update">>}, 2),
    ?receiveError(-32603, <<"Invalid params">>, 2), % Internal error
    send_jsonrpc_request(post, #{type => <<"start_update">>, url => <<"http://dummy.fake/releases/test">>}, 3),
    ?receiveError(-10, <<"Software update unavailable">>, 3),
    send_jsonrpc_request(post, #{type => <<"validate">>}, 4),
    ?receiveError(-10, <<"Software update unavailable">>, 4),
    send_jsonrpc_request(post, #{type => <<"cancel">>}, 5),
    ?receiveError(-10, <<"Software update unavailable">>, 5),

    meck:new(grisp_connect_updater, [passthrough]),
    meck:new(grisp_connect_client, [passthrough]),
    meck:new(grisp_updater, [non_strict]),
    meck:expect(grisp_connect_updater, is_available, fun() -> true end),

    mock_system_info(ready, removable, 0, 0),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 6),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                     update_status := <<"ready">>,
                     boot_source := #{type := <<"removable">>},
                     update_message := <<"Device ready for update">>}, 6),

    mock_system_info(ready, 0, 0, 0),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 7),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                    update_status := <<"ready">>,
                    boot_source := #{id := 0, type := <<"system">>},
                    update_message := <<"Device ready for update">>}, 7),

    mock_system_info({updating, #{data_total => 100, data_checked => 50,
                      data_skipped => 0, data_written => 50}}, removable, 0, 0),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 8),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                     update_status := <<"updating">>,
                     boot_source := #{type := <<"removable">>},
                     update_progress := 50,
                     update_message := <<"Device is updating">>}, 8),

    mock_system_info({error, canceled}, 0, 0, 0),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 9),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                     update_status := <<"canceled">>,
                     boot_source := #{id := 0, type := <<"system">>},
                     update_message := <<"Device update canceled">>}, 9),

    mock_system_info({error, foobar}, removable, 0, 0),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 10),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                     update_status := <<"failed">>,
                     boot_source := #{type := <<"removable">>},
                     update_message := <<"Device update failed">>}, 10),

    mock_system_info({success, undefined}, removable, 0, 1),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 11),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                    update_status := <<"updated">>,
                    boot_source := #{type := <<"removable">>},
                    update_message := <<"Device updated, reboot required to validate the update">>,
                    action_required := <<"reboot">>}, 11),

    mock_system_info(ready, removable, 0, 1),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 12),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                     update_status := <<"updated">>,
                     boot_source := #{type := <<"removable">>},
                     update_message := <<"Device updated but the SD card wasn't removed before rebooting">>,
                     action_required := <<"remove_sdcard_and_reboot">>}, 12),

    mock_system_info(ready, 1, 0, 0),
    send_jsonrpc_request(get, #{type => <<"system_info">>}, 13),
    ?receiveResult(#{relname := null, relvsn := null, update_enabled := true,
                    update_status := <<"updated">>,
                    boot_source := #{id := 1, type := <<"system">>},
                    update_message := <<"Device updated, validation required">>,
                    action_required := <<"validate">>}, 13),

    mock_system_info(ready, 0, 0, 0),

    Url = <<"http://dummy.fake/releases/test">>,
    meck:expect(grisp_updater, start, fun(_Url, _Callback, _Args, _Opts) -> ok end),
    send_jsonrpc_request(post, #{type => <<"start_update">>, url => Url}, 14),
    ?receiveResult(<<"ok">>, 14),
    ?assert(meck:called(grisp_updater, start,
                        [Url, grisp_connect_updater_progress, '_', '_'])),

    meck:expect(grisp_updater, start, fun(_Url, _Callback, _Args, _Opts) -> {error, already_updating} end),
    send_jsonrpc_request(post, #{type => <<"start_update">>, url => Url}, 15),
    ?receiveError(-11, <<"Already updating">>, 15),

    meck:expect(grisp_updater, start, fun(_Url, _Callback, _Args, _Opts) -> {error, boot_system_not_validated} end),
    send_jsonrpc_request(post, #{type => <<"start_update">>, url => Url}, 16),
    ?receiveError(-12, <<"Boot system not validated">>, 16),

    meck:expect(grisp_updater, start, fun(_Url, _Callback, _Args, _Opts) -> {error, {foobar, toto}} end),
    send_jsonrpc_request(post, #{type => <<"start_update">>, url => Url}, 17),
    ?receiveError(-32603, <<"{foobar,toto}">>, 17), % Internal error

    meck:expect(grisp_updater, validate, fun() -> ok end),
    send_jsonrpc_request(post, #{type => <<"validate">>}, 18),
    ?receiveResult(<<"ok">>, 18),
    ?assert(meck:called(grisp_updater, validate, [])),

    meck:expect(grisp_updater, validate, fun() -> {error, {validate_from_unbooted, 0}} end),
    send_jsonrpc_request(post, #{type => <<"validate">>}, 19),
    ?receiveError(-13, <<"Validate from unbooted">>, 19),

    meck:expect(grisp_updater, validate, fun() -> {error, {test, #{}, "error"}} end),
    send_jsonrpc_request(post, #{type => <<"validate">>}, 20),
    ?receiveError(-32603, <<"{test,#{},\"error\"}">>, 20),

    meck:expect(grisp_connect_client, reboot, fun() -> ok end),
    send_jsonrpc_request(post, #{type => <<"reboot">>}, 21),
    ?receiveResult(<<"ok">>, 21),
    ?assert(meck:called(grisp_connect_client, reboot, [])),

    meck:expect(grisp_updater, cancel, fun() -> ok end),
    send_jsonrpc_request(post, #{type => <<"cancel">>}, 22),
    ?receiveResult(<<"ok">>, 22),
    ?assert(meck:called(grisp_updater, cancel, [])),

    meck:unload(grisp_connect_updater),
    meck:unload(grisp_connect_client),
    meck:unload(grisp_updater),

    ok.

unknown_method_test(_) ->
    send_jsonrpc_request(get, #{type => <<"unknown">>}, 1),
    ?receiveError(-32601, <<"Method not found">>, 1).

mock_system_info(Status, removable, Valid, Next) ->
    meck:expect(grisp_updater, info, fun() ->
        #{boot => #{type => removable},
          valid => #{type => system, id => Valid},
          next => #{type => system, id => Next}} end),
    meck:expect(grisp_updater, status, fun() -> Status end);
mock_system_info(Status, Boot, Valid, Next) ->
    meck:expect(grisp_updater, info, fun() ->
        #{boot => #{type => system, id => Boot},
          valid => #{type => system, id => Valid},
          next => #{type => system, id => Next}} end),
    meck:expect(grisp_updater, status, fun() -> Status end).
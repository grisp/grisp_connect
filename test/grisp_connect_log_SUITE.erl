-module(grisp_connect_log_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("grisp_connect_test.hrl").

-compile([export_all, nowarn_export_all]).

-import(grisp_connect_test_client, [cert_dir/0]).
-import(grisp_connect_test_client, [serial_number/0]).
-import(grisp_connect_test_client, [wait_connection/0]).

-import(grisp_connect_test_server, [flush/0]).
-import(grisp_connect_test_server, [receive_jsonrpc_request/0]).
-import(grisp_connect_test_server, [receive_jsonrpc_request/1]).
-import(grisp_connect_test_server, [send_jsonrpc_result/2]).

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

init_per_testcase(log_level_test, Config) ->
    #{level := Level} = logger:get_primary_config(),
    logger:set_primary_config(level, debug),
    init_per_testcase(other, [{default_level, Level} | Config]);
init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(grisp_emulation),
    {ok, _} = application:ensure_all_started(grisp_connect),
    ?assertEqual(ok, wait_connection()),
    grisp_connect_test_server:listen(),
    Config.

end_per_testcase(log_level_test, Config) ->
    logger:set_primary_config(level, ?config(default_level, Config)),
    end_per_testcase(other, Config);
end_per_testcase(_, Config) ->
    ok = application:stop(grisp_connect),
    ?assertEqual([], flush()),
    Config.

%--- Tests ---------------------------------------------------------------------

string_logs_test(_) ->
    S1 = "@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ€",
    S2 = <<"@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ€"/utf8>>,
    Strings = [S1, S2],
    Texts = [<<S2/binary>>, <<S2/binary>>],
    LastSeq = reset_log(last_seq()),
    Seqs = [LastSeq + 1, LastSeq + 2],
    Fun = fun({Seq, String, Text}) ->
                  grisp_connect:log(error, [String]),
                  check_log(Seq, <<"error">>, Text)
          end,
    lists:map(Fun, lists:zip3(Seqs, Strings, Texts)).

formatted_logs_test(_) ->
    ArgsList = [["~c, ~tc", [$ä, $€]],
                ["~p, ~tp", ['tést', 'tést']],
                ["~p, ~tp", [<<"tést">>, <<"tést"/utf8>>]],
                ["~s, ~ts", ["tést", "tést"]],
                ["~s, ~ts", [<<"tést">>, <<"tést"/utf8>>]]],
    Texts = [<<"ä, €"/utf8>>,
             <<"tést, tést"/utf8>>,
             <<"<<\"tést\">>, <<\"tést\"/utf8>>"/utf8>>,
             <<"tést, tést"/utf8>>,
             <<"tést, tést"/utf8>>],
    LastSeq = reset_log(last_seq()),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(ArgsList)),
    Fun = fun({Seq, Args, Text}) ->
                  grisp_connect:log(error, Args),
                  check_log(Seq, <<"error">>, Text)
          end,
    lists:map(Fun, lists:zip3(Seqs, ArgsList, Texts)).

structured_logs_test(_) ->
    Events = [#{event => <<"tést"/utf8>>},
              #{event => "tést"},
              #{event => 'tést'},
              #{event => ['tést1', <<"tést2"/utf8>>]},
              #{event => #{'tèst1' => true}},
              #{event => #{<<"errör"/utf8>> => false}},
              #{event => 1234},
              #{event => 0.1},
              #{event => {'äh', 'bäh'}}],
    Texts = [#{event => <<"tést"/utf8>>},
             #{event => "tést"},
             #{event => <<"tést"/utf8>>},
             #{event => [<<"tést1"/utf8>>,<<"tést2"/utf8>>]},
             #{event => #{'tèst1' => true}},
             #{event => #{<<"errör"/utf8>> => false}},
             #{event => 1234},
             #{event => 0.1},
             <<"[JSON incompatible term]\n#{event => {äh,bäh}}"/utf8>>],
    LastSeq = reset_log(last_seq()),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(Events)),
    Fun = fun({Seq, Event, Text}) ->
                  grisp_connect:log(error, [Event]),
                  check_log(Seq, <<"error">>, Text)
          end,
    lists:map(Fun, lists:zip3(Seqs, Events, Texts)).

log_level_test(_) ->
    Levels = [emergency,
              alert,
              critical,
              error,
              warning,
              notice,
              info,
              debug],
    LastSeq = reset_log(last_seq()),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(Levels)),
    Fun = fun({Seq, Level}) ->
                  grisp_connect:log(Level, ["level test"]),
                  check_log(Seq, atom_to_binary(Level), <<"level test"/utf8>>)
          end,
    lists:map(Fun, lists:zip(Seqs, Levels)),

    % Check that logs outside of configured level are not send to the server
    % One needs to be able to control the traffic
    logger:set_primary_config(level, notice),
    grisp_connect:log(info, ["level test"]),
    send_logs(),
    ct:sleep(100),
    ?assertEqual([], flush()).

meta_data_test(_) ->
    Meta = #{custom1 => <<"binäry"/utf8>>,
             custom2 => ['é1', <<"é2"/utf8>>],
             custom3 => 'åtom',
             custom4 => #{'kèy' => 'välüe'},
             custom5 => #{boolean => true},
             custom6 => 6,
             custom7 => 7.0},
    LastSeq = reset_log(last_seq()),
    Seq = LastSeq + 1,
    grisp_connect:log(error, ["Test meta", Meta]),
    send_logs(),
    ct:pal("Expected seq:~n~p~n", [Seq]),
    Id = ?receiveRequest(
            <<"post">>,
            #{type := <<"logs">>,
              events :=
              [[Seq, #{msg := <<"Test meta"/utf8>>,
                       meta :=
                       #{custom1 := <<"binäry"/utf8>>,
                         custom2 := [<<"é1"/utf8>>,<<"é2"/utf8>>],
                         custom3 := <<"åtom"/utf8>>,
                         custom4 := #{'kèy' := <<"välüe"/utf8>>},
                         custom5 := #{boolean := true},
                         custom6 := 6,
                         custom7 := 7.0},
                       level := <<"error">>}]]}),
    send_jsonrpc_result(#{seq => Seq, dropped => 0}, Id).

%--- Internal ------------------------------------------------------------------

send_logs() ->
    LogServer = whereis(grisp_connect_log_server),
    LogServer ! send_logs.

last_seq() ->
    send_logs(),
    Send = receive_jsonrpc_request(),
    ?assertMatch(#{method := <<"post">>,
                   params := #{type := <<"logs">>,
                               events := _,
                               dropped := _}},
                 Send),
    Id = maps:get(id, Send),
    Events = maps:get(events, maps:get(params, Send)),
    [Seq, _] = lists:last(Events),
    send_jsonrpc_result(#{seq => Seq, dropped => 0}, Id),
    Seq.

reset_log() ->
    reset_log(undefined).

reset_log(LasSeq) ->
    send_logs(),
    try receive_jsonrpc_request(200) of
        #{id := Id, params := #{type := <<"logs">>, events := Events}} ->
            AllSeq = [S || [S, _] <- Events],
            MaxSeq = lists:max(AllSeq),
            send_jsonrpc_result(#{seq => MaxSeq, dropped => 0}, Id),
            reset_log(MaxSeq);
        _Other ->
            reset_log(LasSeq)
    catch
        error:timeout -> LasSeq
    end.

check_log(Seq, Level, Text) ->
    send_logs(),
    ct:pal("Expected seq:~n~p~nExpected level:~n~p~nExpected text:~n~p",
           [Seq, Level, Text]),
    Id = ?receiveRequest(<<"post">>,
                         #{type := <<"logs">>,
                           events := [[Seq, #{msg := Text, level := Level}]]}),
    send_jsonrpc_result(#{seq => Seq, dropped => 0}, Id).

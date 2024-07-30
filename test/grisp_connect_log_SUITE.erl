-module(grisp_connect_log_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

-import(grisp_connect_test_client, [cert_dir/0]).
-import(grisp_connect_test_client, [serial_number/0]).
-import(grisp_connect_test_client, [wait_connection/0]).

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
    grisp_connect_manager:kraft_start(CertDir),
    grisp_connect_manager:link_device(),
    [{cert_dir, CertDir} | Config2].

end_per_suite(Config) ->
    grisp_connect_manager:cleanup_apps(?config(apps, Config)).

init_per_testcase(log_level_test, Config) ->
    #{level := Level} = logger:get_primary_config(),
    logger:set_primary_config(level, debug),
    init_per_testcase(other, [{default_level, Level} | Config]);
init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(grisp_emulation),
    {ok, _} = application:ensure_all_started(grisp_connect),
    ok = wait_connection(),
    ct:sleep(1000),
    send_logs(),
    ct:sleep(1000),
    Config.

end_per_testcase(log_level_test, Config) ->
    logger:set_primary_config(level, ?config(default_level, Config)),
    end_per_testcase(other, Config);
end_per_testcase(_, Config) ->
    Config.

%--- Tests ---------------------------------------------------------------------

string_logs_test(_) ->
    LastSeq = last_seq(),
    S1 = "@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ€",
    S2 = <<"@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ€"/utf8>>,
    Strings = [S1, S2],
    Texts = [<<S2/binary, "\n">>, <<S2/binary, "\n">>],
    Seqs = [LastSeq + 1, LastSeq + 2],
    Fun = fun({Seq, String, Text}) ->
                  grisp_connect:log(error, [String]),
                  send_logs(),
                  ct:sleep(100),
                  check_log(Seq, error, Text)
          end,
    lists:map(Fun, lists:zip3(Seqs, Strings, Texts)).

formatted_logs_test(_) ->
    ArgsList = [["~c, ~tc", [$ä, $€]],
                ["~p, ~tp", ['tést', 'tést']],
                ["~p, ~tp", [<<"tést">>, <<"tést"/utf8>>]],
                ["~s, ~ts", ["tést", "tést"]],
                ["~s, ~ts", [<<"tést">>, <<"tést"/utf8>>]]],
    LastSeq = last_seq(),
    Texts = [<<"ä, €\n"/utf8>>,
             <<"tést, tést\n"/utf8>>,
             <<"<<\"tést\">>, <<\"tést\"/utf8>>\n"/utf8>>,
             <<"tést, tést\n"/utf8>>,
             <<"tést, tést\n"/utf8>>],
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(ArgsList)),
    Fun = fun({Seq, Args, Text}) ->
                  grisp_connect:log(error, Args),
                  send_logs(),
                  ct:sleep(100),
                  check_log(Seq, error, Text)
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
    LastSeq = last_seq(),
    Texts = [<<"    event: <<\"tést\"/utf8>>\n"/utf8>>,
             <<"    event: tést\n"/utf8>>,
             <<"    event: <<\"tést\"/utf8>>\n"/utf8>>,
             <<"    event: [<<\"tést1\"/utf8>>,<<\"tést2\"/utf8>>]\n"/utf8>>,
             <<"    event: #{tèst1 => true}\n"/utf8>>,
             <<"    event: #{<<\"errör\"/utf8>> => false}\n"/utf8>>,
             <<"    event: 1234\n"/utf8>>,
             <<"    event: 0.1\n"/utf8>>,
             <<"[JSON incompatible term]\n#{event => {äh,bäh}}\n"/utf8>>],
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(Events)),
    Fun = fun({Seq, Event, Text}) ->
                  grisp_connect:log(error, [Event]),
                  send_logs(),
                  ct:sleep(100),
                  check_log(Seq, error, Text)
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
    LastSeq = last_seq(),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(Levels)),
    Fun = fun({Seq, Level}) ->
                  grisp_connect:log(Level, ["level test"]),
                  send_logs(),
                  ct:sleep(100),
                  check_log(Seq, Level, <<"level test\n"/utf8>>)
          end,
    lists:map(Fun, lists:zip(Seqs, Levels)),

    % Check that logs outside of configured level are not send to the server
    % One needs to be able to control the traffic
    logger:set_primary_config(level, notice),
    grisp_connect:log(info, ["level test"]),
    send_logs(),
    ct:sleep(100),
    Logs = grisp_manager_device_logs:fetch(serial_number(), last_seq()),
    ?assertEqual([], Logs).

meta_data_test(_) ->
    Meta = #{custom1 => <<"binäry"/utf8>>,
             custom2 => ['é1', <<"é2"/utf8>>],
             custom3 => 'åtom',
             custom4 => #{'kèy' => 'välüe'},
             custom5 => #{boolean => true},
             custom6 => 6,
             custom7 => 7.0},
    LoggerConfig = #{legacy_header => true,
                     single_line => false,
                     template => [[logger_formatter, header], "\n",
                                  custom1, "\n",
                                  custom2, "\n",
                                  custom3, "\n",
                                  custom4, "\n",
                                  custom5, "\n",
                                  custom6, "\n",
                                  custom7, "\n",
                                  msg, "\n"]},
    application:set_env(grisp_manager, device_log_config, LoggerConfig),
    Text = iolist_to_binary(
             [<<"<<\"binäry\"/utf8>>\n"/utf8>>,
              <<"\[<<\"é1\"/utf8>>,<<\"é2\"/utf8>>\]\n"/utf8>>,
              <<"<<\"åtom\"/utf8>>\n"/utf8>>,
              <<"#{kèy => <<\"välüe\"/utf8>>}\n"/utf8>>,
              <<"#{boolean => true}\n"/utf8>>,
              <<"6\n"/utf8>>,
              <<"7.0\n"/utf8>>,
              <<"Test meta\n"/utf8>>]),
    LastSeq = last_seq(),
    Seq = LastSeq + 1,
    grisp_connect:log(error, ["Test meta", Meta]),
    send_logs(),
    ct:sleep(100),
    check_log(Seq, error, Text).

%--- Internal ------------------------------------------------------------------

send_logs() ->
    LogServer = whereis(grisp_connect_log_server),
    LogServer ! send_logs.

last_seq() ->
    {Seq, _} = case grisp_manager_device_logs:fetch(serial_number(), -1) of
                   [] -> {0, []};
                   List -> lists:last(List)
               end,
    Seq.

check_log(Seq, Level, Text) ->
    Logs = grisp_manager_device_logs:fetch(serial_number(), Seq - 1),
    ?assertMatch([{Seq, _}], Logs, ["Text: ", Text]),
    [{_, Log}] = Logs,
    Split = binary:split(Log, <<"\n">>, [trim_all]),
    ?assertMatch([_, Text], Split, ["Text: ", Text]),
    Header = log_header(Level),
    ?assertMatch([Header, _],
                 binary:split(Log, <<"= ">>),
                 ["Header: ", Header]).

log_header(emergency) -> <<"=EMERGENCY REPORT===">>;
log_header(alert)     -> <<"=ALERT REPORT===">>;
log_header(critical)  -> <<"=CRITICAL REPORT===">>;
log_header(error)     -> <<"=ERROR REPORT===">>;
log_header(warning)   -> <<"=WARNING REPORT===">>;
log_header(notice)    -> <<"=NOTICE REPORT===">>;
log_header(info)      -> <<"=INFO REPORT===">>;
log_header(debug)     -> <<"=DEBUG REPORT===">>.

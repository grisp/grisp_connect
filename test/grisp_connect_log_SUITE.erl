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
-import(grisp_connect_test_server, [send_jsonrpc_notification/2]).
-import(grisp_connect_test_server, [send_jsonrpc_request/2]).
-import(grisp_connect_test_server, [send_jsonrpc_result/2]).


%--- MACROS --------------------------------------------------------------------

-define(FMT(F, A), binary_to_list(iolist_to_binary(io_lib:format(F, A)))).
-define(assert_logs(COUNT, BASE_SEQ, MATCH_SPEC), fun() ->
    __LogGetResult = log_get((COUNT), undefined),
    ?assertMatch(
        #{events := __E, dropped := __D}
            when is_list(__E) andalso is_integer(__D),
        __LogGetResult),
    #{events := __Events, dropped := __Dropped} = __LogGetResult,
    __EventCount = length(__Events),
    ct:pal("Requested ~w log entries, received ~w:~n"
           ++ lists:flatten(lists:duplicate(__EventCount, "~w: ~999999p~n"))
           ++ "Expecting sequence starting at ~w:~n~s",
           [(COUNT), __EventCount]
           ++ lists:flatten(__Events)
           ++ [(BASE_SEQ), ??MATCH_SPEC]),
    __Packed = [{S - (BASE_SEQ), L, T, M}
                || [S, #{msg := T, level := L, meta := M}] <- __Events],
    ?assertMatch(MATCH_SPEC, __Packed),
    case __Packed of
        [] -> undefined;
        [_|_] ->
            {__LastSeqDelta, _, _, _} = lists:last(__Packed),
            {__LastSeqDelta + (BASE_SEQ), __Dropped}
    end
end()).
-define(assert_log(SEQ, LEVEL, TEXT, META), fun() ->
    ?assert_logs(1, SEQ, [{0, LEVEL, TEXT, META}])
end()).
-define(assert_log(SEQ, LEVEL, TEXT), ?assert_log(SEQ, LEVEL, TEXT, _)).


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
    Apps = grisp_connect_test_server:start(#{cert_dir => CertDir}),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    grisp_connect_test_server:stop(?config(apps, Config)).

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
    grisp_connect_test_server:wait_disconnection(),
    ?assertEqual([], flush()),
    Config.

%--- Tests ---------------------------------------------------------------------

partial_sync_test(_) ->
    % Ensure there is at least one log entry, so LastSeq is defined
    grisp_connect:log(error, ["dummy"]),
    LastSeq = log_reset(),
    grisp_connect:log(error, ["Test sync 1"]),
    grisp_connect:log(error, ["Test sync 2"]),
    grisp_connect:log(error, ["Test sync 3"]),
    grisp_connect:log(error, ["Test sync 4"]),
    _ = ?assert_logs(3, LastSeq + 1, [
        {0, <<"error">>, <<"Test sync 1"/utf8>>, _},
        {1, <<"error">>, <<"Test sync 2"/utf8>>, _},
        {2, <<"error">>, <<"Test sync 3"/utf8>>, _}
    ]),
    log_sync(LastSeq + 2, 0), % Partial sync
    grisp_connect:log(error, ["Test sync 5"]),
    log_sync(?assert_logs(10, LastSeq + 3, [
        {0, <<"error">>, <<"Test sync 3"/utf8>>, _},
        {1, <<"error">>, <<"Test sync 4"/utf8>>, _},
        {2, <<"error">>, <<"Test sync 5"/utf8>>, _}
    ])),
    ok.

string_logs_test(_) ->
    S1 = "@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ€",
    S2 = <<"@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ€"/utf8>>,
    Strings = [S1, S2],
    Texts = [<<S2/binary>>, <<S2/binary>>],
    % Ensure there is at least one log entry, so LastSeq is defined
    grisp_connect:log(error, ["dummy"]),
    LastSeq = log_reset(),
    Seqs = [LastSeq + 1, LastSeq + 2],
    Fun = fun({Seq, String, Text}) ->
                  grisp_connect:log(error, [String]),
                  log_sync(?assert_log(Seq, <<"error">>, Text))
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
    % Ensure there is at least one log entry, so LastSeq is defined
    grisp_connect:log(error, ["dummy"]),
    LastSeq = log_reset(),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(ArgsList)),
    Fun = fun({Seq, Args, Text}) ->
                  grisp_connect:log(error, Args),
                  log_sync(?assert_log(Seq, <<"error">>, Text))
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
    % Ensure there is at least one log entry, so LastSeq is defined
    grisp_connect:log(error, ["dummy"]),
    LastSeq = log_reset(),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(Events)),
    Fun = fun({Seq, Event, Text}) ->
                  grisp_connect:log(error, [Event]),
                  log_sync(?assert_log(Seq, <<"error">>, Text))
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
    % Ensure there is at least one log entry, so LastSeq is defined
    grisp_connect:log(error, ["dummy"]),
    LastSeq = log_reset(),
    Seqs = lists:seq(LastSeq + 1, LastSeq + length(Levels)),
    Fun = fun({Seq, Level}) ->
                  grisp_connect:log(Level, ["level test"]),
                  BinLevel = atom_to_binary(Level),
                  log_sync(?assert_log(Seq, BinLevel, <<"level test"/utf8>>, _))
          end,
    lists:map(Fun, lists:zip(Seqs, Levels)),

    % Check that logs outside of configured level are not send to the server
    % One needs to be able to control the traffic
    logger:set_primary_config(level, notice),
    grisp_connect:log(info, ["level test"]),
    ?assertMatch(#{events := []}, log_get(100, undefined)),
    ok.

meta_data_test(_) ->
    Meta = #{custom1 => <<"binäry"/utf8>>,
             custom2 => ['é1', <<"é2"/utf8>>],
             custom3 => 'åtom',
             custom4 => #{'kèy' => 'välüe'},
             custom5 => #{boolean => true},
             custom6 => 6,
             custom7 => 7.0},
    % Ensure there is at least one log entry, so LastSeq is defined
    grisp_connect:log(error, ["dummy"]),
    LastSeq = log_reset(),
    Seq = LastSeq + 1,
    grisp_connect:log(error, ["Test meta", Meta]),
    log_sync(?assert_log(Seq, <<"error">>, <<"Test meta"/utf8>>, #{
        custom1 := <<"binäry"/utf8>>,
        custom2 := [<<"é1"/utf8>>,<<"é2"/utf8>>],
        custom3 := <<"åtom"/utf8>>,
        custom4 := #{'kèy' := <<"välüe"/utf8>>},
        custom5 := #{boolean := true},
        custom6 := 6,
        custom7 := 7.0
    })),
    ok.


%--- Internal ------------------------------------------------------------------

log_get_params(undefined, undefined) -> #{};
log_get_params(MaxBatchSize, undefined) -> #{max_batch_size => MaxBatchSize};
log_get_params(undefined, MaxByteSize) -> #{max_byte_size => MaxByteSize};
log_get_params(MaxBatchSize, MaxByteSize) ->
    #{max_batch_size => MaxBatchSize, max_byte_size => MaxByteSize}.

log_get(MaxBatchSize, MaxByteSize) ->
    Params = log_get_params(MaxBatchSize, MaxByteSize),
    ID = send_jsonrpc_request(<<"log.get">>, Params),
    ?receiveResult(#{dropped := _, events := _}, ID).

log_sync(Seq, Dropped) ->
    send_jsonrpc_notification(<<"log.sync">>, #{seq => Seq, dropped => Dropped}),
    wait_synced(Seq).

log_sync({Seq, Dropped}) ->
    log_sync(Seq, Dropped);
log_sync(undefined) ->
    ok.

last_seq() ->
    case grisp_connect_log:get(#{}) of
        #{events := []} -> undefined;
        #{events := Events} ->
            [Seq, _] = lists:last(Events),
            Seq
        end.

log_reset() ->
    log_reset(undefined).

log_reset(LastKnownSeq) ->
    case grisp_connect_log:get(#{}) of
        #{events := []} -> LastKnownSeq;
        #{events := Events, dropped := Dropped} ->
            [Seq, _] = lists:last(Events),
            grisp_connect_log:sync(#{seq => Seq, dropped => Dropped}),
            log_reset(Seq)
    end.

wait_synced(Seq) ->
    wait_synced(Seq, os:timestamp()).

wait_synced(Seq, StartTimestamp) ->
    case timer:now_diff(os:timestamp(), StartTimestamp) of
        Timeout when Timeout > 1000 ->
            ?assert(false, "Timeout waiting for log sync");
        _ ->
            case last_seq() of
                undefined -> ok; % No logs
                LastSeq when LastSeq >= Seq -> ok;
                _ -> wait_synced(Seq, StartTimestamp)
            end
    end.

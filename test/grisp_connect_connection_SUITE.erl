-module(grisp_connect_connection_SUITE).

-behaviour(ct_suite).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("grisp_connect_test.hrl").

-compile([export_all, nowarn_export_all]).

-import(grisp_connect_test_async, [async_eval/1]).
-import(grisp_connect_test_async, [async_get_result/1]).

-import(grisp_connect_test_server, [flush/0]).
-import(grisp_connect_test_server, [send_text/1]).
-import(grisp_connect_test_server, [send_jsonrpc_request/3]).
-import(grisp_connect_test_server, [send_jsonrpc_notification/2]).
-import(grisp_connect_test_server, [send_jsonrpc_result/2]).
-import(grisp_connect_test_server, [send_jsonrpc_error/3]).


%--- MACROS --------------------------------------------------------------------

-define(fmt(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).
-define(assertConnRequest(Conn, M, P, R), fun() ->
    receive {conn, Conn, {request, M, P = Result, R}} -> Result
    after 1000 ->
        ?assert(false, ?fmt("The client connection did not receive request ~s ~s ~s",
                            [??M, ??P, ??P]))
    end
end()).
-define(assertConnNotification(Conn, M, P), fun() ->
    receive {conn, Conn, {notification, M, P = Result}} -> Result
    after 1000 ->
        ?assert(false, ?fmt("The client did not receive notification ~s ~s",
                            [??M, ??P]))
    end
end()).
-define(assertConnResponse(Conn, V, X), fun() ->
    receive {conn, Conn, {response, V = Result, X}} -> Result
    after 1000 ->
        ?assert(false, ?fmt("The client connection did not receive response ~s ~s",
                            [??V, ??X]))
    end
end()).
-define(assertConnRemoteError(Conn, C, M, D, X), fun() ->
    receive {conn, Conn, {remote_error, C, M, D, X}} -> ok
    after 1000 ->
        ?assert(false, ?fmt("The client connection did not receive remote error ~s ~s ~s ~s",
                            [??C, ??M, ??D, ??X]))
    end
end()).
-define(assertConnRemoteError(Conn, C, M, D), fun() ->
    receive {conn, Conn, {remote_error, C, M, D}} -> ok
    after 1000 ->
        ?assert(false, ?fmt("The client connection did not receive remote error ~s ~s ~s",
                            [??C, ??M, ??D]))
    end
end()).
-define(assertConnLocalError(Conn, R, X), fun() ->
    receive {conn, Conn, {local_error, R, X}} -> ok
    after 1000 ->
        ?assert(false, ?fmt("The client connection did not receive local error ~s ~s",
                            [??R, ??X]))
    end
end()).


%--- API -----------------------------------------------------------------------

all() ->
    [
        F
        ||
        {F, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(F))
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gun),
    Apps = grisp_connect_test_server:start(),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    grisp_connect_test_server:stop(?config(apps, Config)).

init_per_testcase(_TestCase, Config) ->
    {ok, _} = application:ensure_all_started(grisp_emulation),
    Config.

end_per_testcase(_, Config) ->
    ?assertEqual([], flush()),
    Config.


%--- Tests ---------------------------------------------------------------------

basic_server_notifications_test(_) ->
    Conn = connect(),
    send_jsonrpc_notification(<<"ping">>, #{foo => null}),
    ?assertConnNotification(Conn, [ping], #{foo := undefined}),
    send_jsonrpc_notification(<<"foo.bar.ping">>, #{}),
    ?assertConnNotification(Conn, [foo, bar, ping], _),
    send_jsonrpc_notification(<<"foo.bar.NotAnAtom">>, #{}),
    ?assertConnNotification(Conn, [foo, bar, <<"NotAnAtom">>], _),
    disconnect(Conn).

basic_client_notifications_test(_) ->
    Conn = connect(),
    grisp_connect_connection:notify(Conn, ping, #{foo => undefined}),
    ?receiveNotification(<<"ping">>, #{foo := null}),
    grisp_connect_connection:notify(Conn, [foo, bar, ping], #{}),
    ?receiveNotification(<<"foo.bar.ping">>, _),
    grisp_connect_connection:notify(Conn, [foo, bar, <<"NotAnAtom">>], #{}),
    ?receiveNotification(<<"foo.bar.NotAnAtom">>, _),
    disconnect(Conn).

basic_server_request_test(_) ->
    Conn = connect(),
    send_jsonrpc_request(<<"toto">>, #{}, 1),
    ?assertConnRequest(Conn, [toto], _, 1),
    grisp_connect_connection:reply(Conn, <<"spam">>, 1),
    ?receiveResult(<<"spam">>, 1),
    send_jsonrpc_request(<<"foo.bar.tata">>, #{}, 2),
    ?assertConnRequest(Conn, [foo, bar, tata], _, 2),
    grisp_connect_connection:error(Conn, error1, undefined, undefined, 2),
    ?receiveError(-1, <<"Error Number 1">>, 2),
    send_jsonrpc_request(<<"foo.bar.toto">>, #{}, 3),
    ?assertConnRequest(Conn, [foo, bar, toto], _, 3),
    grisp_connect_connection:error(Conn, error2, <<"Custom">>, undefined, 3),
    ?receiveError(-2, <<"Custom">>, 3),
    send_jsonrpc_request(<<"foo.bar.titi">>, #{}, 4),
    ?assertConnRequest(Conn, [foo, bar, titi], _, 4),
    grisp_connect_connection:error(Conn, -42, <<"Message">>, undefined, 4),
    ?receiveError(-42, <<"Message">>, 4),
    disconnect(Conn).

basic_client_synchronous_request_test(_) ->
    Conn = connect(),
    Async1 = async_eval(fun() -> grisp_connect_connection:request(Conn, [toto], #{}) end),
    Id1 = ?receiveRequest(<<"toto">>, _),
    send_jsonrpc_result(<<"spam">>, Id1),
    ?assertEqual({ok, <<"spam">>}, async_get_result(Async1)),
    Async2 = async_eval(fun() -> grisp_connect_connection:request(Conn, tata, #{}) end),
    Id2 = ?receiveRequest(<<"tata">>, _),
    send_jsonrpc_error(-1, null, Id2),
    ?assertEqual({remote_error, error1, <<"Error Number 1">>, undefined}, async_get_result(Async2)),
    Async3 = async_eval(fun() -> grisp_connect_connection:request(Conn, titi, #{}) end),
    Id3 = ?receiveRequest(<<"titi">>, _),
    send_jsonrpc_error(-2, <<"Custom">>, Id3),
    ?assertEqual({remote_error, error2, <<"Custom">>, undefined}, async_get_result(Async3)),
    disconnect(Conn).

basic_client_asynchronous_request_test(_) ->
    Conn = connect(),
    grisp_connect_connection:post(Conn, toto, #{}, ctx1),
    Id1 = ?receiveRequest(<<"toto">>, _),
    send_jsonrpc_result(<<"spam">>, Id1),
    ?assertConnResponse(Conn, <<"spam">>, ctx1),
    grisp_connect_connection:post(Conn, tata, #{}, ctx2),
    Id2 = ?receiveRequest(<<"tata">>, _),
    send_jsonrpc_error(-1, null, Id2),
    ?assertConnRemoteError(Conn, error1, <<"Error Number 1">>, undefined, ctx2),
    grisp_connect_connection:post(Conn, titi, #{}, ctx3),
    Id3 = ?receiveRequest(<<"titi">>, _),
    send_jsonrpc_error(-2, <<"Custom">>, Id3),
    ?assertConnRemoteError(Conn, error2, <<"Custom">>, undefined, ctx3),
    disconnect(Conn).

basic_error_test(_) ->
    Conn = connect(),
    grisp_connect_connection:error(Conn, -1, undefined, undefined, undefined),
    ?receiveError(-1, <<"Error Number 1">>, null),
    send_jsonrpc_error(-2, null, null),
    ?assertConnRemoteError(Conn, error2, <<"Error Number 2">>, undefined),
    disconnect(Conn).

request_timeout_test(_) ->
    Conn = connect(),
    grisp_connect_connection:post(Conn, toto, #{}, ctx1),
    _Id1 = ?receiveRequest(<<"toto">>, _),
    timer:sleep(500),
    ?assertConnLocalError(Conn, timeout, ctx1),
    disconnect(Conn).

spec_example_test(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ExamplesFile = filename:join(DataDir, "jsonrpc_examples.txt"),

    Conn = connect(),

    {ok, ExData} = file:read_file(ExamplesFile),
    Examples = parse_examples(ExData),
    maps:foreach(fun(Desc, Actions) ->
        try
            lists:foreach(fun
                ({send, Text}) ->
                    send_text(Text);
                ({recv, Expected}) when is_list(Expected) ->
                    example_handler(Conn),
                    SortedExpected = lists:sort(Expected),
                    Received = grisp_connect_test_server:receive_jsonrpc(),
                    ?assert(is_list(Received),
                            ?fmt("Invalid response to a batch request during ~s: ~p",
                                 [Desc, Received])),
                    SortedReceived = lists:sort(Received),
                    ?assertEqual(SortedExpected, SortedReceived,
                                 ?fmt("Invalid response during ~s", [Desc]));
                ({recv, Expected}) ->
                    example_handler(Conn),
                    Received = grisp_connect_test_server:receive_jsonrpc(),
                    ?assertEqual(Expected, Received,
                                 ?fmt("Invalid response during ~s", [Desc]))
            end, Actions),
            example_handler(Conn),
            RemMsgs = flush(),
            ?assertEqual([], RemMsgs,
                         ?fmt("Unexpected message during example ~s: ~p",
                              [Desc, RemMsgs]))
        catch
            error:timeout ->
                ?assert(false, ?fmt("Timeout while testing example ~s", [Desc]))
        end
    end, Examples),

    disconnect(Conn).


%--- Internal Functions --------------------------------------------------------

connect() ->
    connect(#{}).

connect(Opts) ->
    DefaultOpts = #{domain => localhost, port => 3030, transport => tcp,
                    path => <<"/grisp-connect/ws">>,
                    request_timeout => 300,
                    errors => [
                        {error1, -1, <<"Error Number 1">>},
                        {error2, -2, <<"Error Number 2">>}
                    ]},
    ConnOpts = maps:merge(DefaultOpts, Opts),
    {ok, Conn} = grisp_connect_connection:start_link(self(), ConnOpts),
    receive
        {conn, Conn, connected} ->
            grisp_connect_test_server:listen(),
            Conn
    after
        1000 ->
            ?assert(false, "Connection to test server failed")
    end.

disconnect(Conn) ->
    unlink(Conn),
    grisp_connect_connection:disconnect(Conn),
    ok.

example_handler(Conn) ->
    receive
        {conn, Conn, {request, [subtract], [A, B], ReqRef}} ->
            grisp_connect_connection:reply(Conn, A - B, ReqRef),
            example_handler(Conn);
        {conn, Conn, {request, [subtract], #{minuend := A, subtrahend := B}, ReqRef}} ->
            grisp_connect_connection:reply(Conn, A - B, ReqRef),
            example_handler(Conn);
        {conn, Conn, {request, [sum], Values, ReqRef}} ->
            Result = lists:foldl(fun(V, Acc) -> V + Acc end, 0, Values),
            grisp_connect_connection:reply(Conn, Result, ReqRef),
            example_handler(Conn);
        {conn, Conn, {request, [get_data], _, ReqRef}} ->
            grisp_connect_connection:reply(Conn, [<<"hello">>, 5], ReqRef),
            example_handler(Conn);
        {conn, Conn, {request, _M, _P, ReqRef}} ->
            grisp_connect_connection:error(Conn, method_not_found, undefined, undefined, ReqRef),
            example_handler(Conn);
        {conn, Conn, {notification, _M, _P}} ->
            example_handler(Conn);
        % {conn, Conn, {response, R, Ctx :: term()}}
        {conn, Conn, {remote_error, _C, _M, _D}} ->
            example_handler(Conn)
    after
        100 -> ok
    end.

parse_examples(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    parse_examples_lines(Lines, #{}, undefined, undefined).

parse_examples_lines([], Acc, undefined, _Actions) ->
    Acc;
parse_examples_lines([], Acc, Desc, Actions) ->
    Acc#{Desc => lists:reverse(Actions)};
parse_examples_lines([<<"-->", RestBin/binary>> | RestLines], Acc, Desc, Actions)
  when Desc =/= undefined ->
    {Raw, RestLines2} = parse_examples_collect([RestBin | RestLines], <<>>),
    parse_examples_lines(RestLines2, Acc, Desc, [{send, Raw} | Actions]);
parse_examples_lines([<<"<--", RestBin/binary>> | RestLines], Acc, Desc, Actions)
  when Desc =/= undefined ->
    case parse_examples_collect([RestBin | RestLines], <<>>) of
        {<<"">>, RestLines2} ->
            parse_examples_lines(RestLines2, Acc, Desc, Actions);
        {Raw, RestLines2} ->
            Decoded = jsx:decode(Raw, [{labels, attempt_atom}, return_maps]),
            parse_examples_lines(RestLines2, Acc, Desc, [{recv, Decoded} | Actions])
    end;
parse_examples_lines([Line | RestLines], Acc, Desc, Actions) ->
    case re:replace(Line, "^\\s+|\\s+$", "", [{return, binary}, global]) of
        <<"">> -> parse_examples_lines(RestLines, Acc, Desc, Actions);
        <<"//", _/binary>> -> parse_examples_lines(RestLines, Acc, Desc, Actions);
        NewDesc ->
            NewAcc = case Desc =/= undefined of
                true -> Acc#{Desc => lists:reverse(Actions)};
                false -> Acc
            end,
            NewDesc2 = re:replace(NewDesc, ":+$", "", [{return, binary}, global]),
            parse_examples_lines(RestLines, NewAcc, NewDesc2, [])
    end.

parse_examples_collect([], Acc) -> {Acc, []};
parse_examples_collect([Line | RestLines], Acc) ->
    case re:replace(Line, "^\\s+|\\s+$", "", [{return, binary}, global]) of
        <<"">> -> {Acc, RestLines};
        <<"-->", _/binary>> -> {Acc, [Line | RestLines]};
        <<"<--", _/binary>> -> {Acc, [Line | RestLines]};
        <<"//", _/binary>> -> parse_examples_collect(RestLines, Acc);
        Line2 -> parse_examples_collect(RestLines, <<Acc/binary, Line2/binary>>)
    end.

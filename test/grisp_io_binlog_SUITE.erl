-module(grisp_io_binlog_SUITE).

% -include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-import(grisp_io_binlog, [
    defaults/1,
    new/0,
    new/1,
    insert/2,
    items/1,
    count/1,
    bytes/1,
    seq/1,
    peek/2,
    truncate/2,
    opts/1
]).

-define(assertItems(Items, Queue),
    ?assertEqual(Items, items(Queue)),
    ?assertEqual(length(Items), count(Queue)),
    ?assertEqual(lists:sum([byte_size(Bin) || {_, Bin} <- Items]), bytes(Queue)),
    case count(Queue) of
        0 -> ok;
        _ -> ?assertEqual(element(1, lists:last(items(Queue))), seq(Queue))
    end
).

%--- Tests ---------------------------------------------------------------------

all() ->
    [
        invalid,
        count_max,
        bytes_max,
        peek,
        truncate,
        out_of_sequence,
        options
    ].

invalid(_Config) ->
    Invalid = [foo, <<"foo">>, 1, {}, {1, foo}, {foo, <<"bar">>}],
    Item = {1, <<>>},
    Log = new(),
    [?assertError({invalid_log, I}, insert(Item, I)) || I <- Invalid ++ [Item]],
    [?assertError({invalid_item, I}, insert(I, Log)) || I <- Invalid ++ [Log]].

count_max(_Config) ->
    L0 = new(#{count_max => 2, bytes_max => 10}),
    ct:pal("~p", [L0]),
    {L1, D0} = insert_many([{1, <<>>}, {2, <<>>}], L0),
    ct:pal("~p ~p", [L1, D0]),
    ?assertEqual(0, D0),
    ?assertItems([{1, <<>>}, {2, <<>>}], L1),
    {L2, D2} = insert({3, <<>>}, L1),
    ?assertEqual(1, D2),
    ?assertItems([{2, <<>>}, {3, <<>>}], L2).

bytes_max(_Config) ->
    Five = <<0:5/integer-unit:8>>,
    Ten = <<0:10/integer-unit:8>>,
    Twenty = <<0:20/integer-unit:8>>,
    L0 = new(#{count_max => 5, bytes_max => 10}),
    {L1, D1} = insert_many([{1, Five}, {2, Five}], L0),
    ?assertEqual(0, D1),
    ?assertItems([{1, Five}, {2, Five}], L1),
    {L2, D2} = insert({3, Five}, L1),
    ?assertEqual(1, D2),
    ?assertItems([{2, Five}, {3, Five}], L2),
    {L3, D3} = insert({4, Ten}, L2),
    ?assertEqual(2, D3),
    ?assertItems([{4, Ten}], L3),
    {L4, D4} = insert({5, Twenty}, L3),
    ?assertEqual(1, D4),
    ?assertItems([{5, Twenty}], L4).

peek(_Config) ->
    L0 = new(),
    [?assertError({invalid_count, C}, peek(C, L0)) || C <- [0, -1, -127, c]],
    ?assertEqual([], peek(1, L0)),
    {L1, D1} = insert_many([{N, <<>>} || N <- lists:seq(0, 9)], L0),
    ?assertEqual(0, D1),
    ?assertEqual([{N, <<>>} || N <- lists:seq(0, 4)], peek(5, L1)),
    ?assertEqual([{N, <<>>} || N <- lists:seq(0, 9)], peek(10, L1)),
    ?assertEqual([{N, <<>>} || N <- lists:seq(0, 9)], peek(15, L1)).

truncate(_Config) ->
    L0 = new(),
    ?assertEqual(L0, truncate(5, L0)),
    {L1, D1} = insert_many([{N, <<>>} || N <- lists:seq(1, 10)], L0),
    ?assertEqual(0, D1),
    ?assertItems([{N, <<>>} || N <- lists:seq(6, 10)], truncate(5, L1)),
    ?assertItems([], truncate(15, L1)),
    ?assertEqual(10, seq(L1)).

out_of_sequence(_Config) ->
    L0 = new(),
    ?assertError({invalid_item, {-10, <<>>}}, insert({-10, <<>>}, L0)),
    {L1, D1} = insert_many([{N, <<>>} || N <- lists:seq(0, 10, 2)], L0),
    ?assertEqual(0, D1),
    [
        ?assertError({out_of_sequence, N}, insert({N, <<>>}, L1))
     || N <- lists:seq(0, 10)
    ],
    {L2, D2} = insert({12, <<>>}, L1),
    ?assertEqual(0, D2),
    ?assertItems([{N, <<>>} || N <- lists:seq(0, 12, 2)], L2).

options(_Config) ->
    ?assertEqual(
        #{count_max => defaults(count_max), bytes_max => defaults(bytes_max)},
        opts(new())
    ),
    Opts = #{count_max => 10, bytes_max => 256},
    ?assertEqual(Opts, opts(new(Opts))).

%--- Internal ------------------------------------------------------------------

insert_many(Items, Log) ->
    lists:foldl(
        fun(Item, {L0, N}) ->
            {L1, M} = insert(Item, L0),
            {L1, N + M}
        end,
        {Log, 0},
        Items
    ).

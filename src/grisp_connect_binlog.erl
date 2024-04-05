%% @doc Ring buffer for storing logger logs
%%
%% This module extends a standard queue into a ring buffer.
%% It can be truncated once a batch of logs has been sent out
%% and is not needed anymore.
%% @end
-module(grisp_connect_binlog).

% API
-export([defaults/1]).
-export([new/0]).
-export([new/1]).
-export([insert/2]).
-export([items/1]).
-export([count/1]).
-export([bytes/1]).
-export([seq/1]).
-export([opts/1]).
-export([peek/2]).
-export([truncate/2]).

-define(DEFAULT_COUNT_MAX, 10_000).
% 10 MiB:
-define(DEFAULT_BYTES_MAX, 10 * 1_024 * 1_024).

-record(binlog, {
    queue = queue:new(),
    seq = -1,
    count = 0,
    count_max = ?DEFAULT_COUNT_MAX,
    bytes = 0,
    bytes_max = ?DEFAULT_BYTES_MAX
}).

%--- API -----------------------------------------------------------------------

defaults(count_max) -> ?DEFAULT_COUNT_MAX;
defaults(bytes_max) -> ?DEFAULT_BYTES_MAX.

new() -> new(#{}).

new(Opts) ->
    Log = #binlog{},
    Log#binlog{
        count_max = maps:get(count_max, Opts, Log#binlog.count_max),
        bytes_max = maps:get(bytes_max, Opts, Log#binlog.bytes_max)
    }.

insert({Seq, Bin} = Item, #binlog{bytes_max = BytesMax, count = Count} = L) when
    is_integer(Seq), Seq >= 0, byte_size(Bin) >= BytesMax
->
    {add(Item, clear(L)), Count};
insert({Seq, Bin} = Item, #binlog{} = L) when
    is_integer(Seq), Seq >= 0, is_binary(Bin)
->
    flush(add(Item, L));
insert(Item, #binlog{}) ->
    error({invalid_item, Item});
insert(_Item, Log) ->
    error({invalid_log, Log}).

items(#binlog{queue = Q}) ->
    queue:to_list(Q).

count(#binlog{count = Count}) -> Count.

bytes(#binlog{bytes = Bytes}) -> Bytes.

seq(#binlog{seq = Seq}) -> Seq.

opts(#binlog{count_max = CountMax, bytes_max = BytesMax}) ->
    #{count_max => CountMax, bytes_max => BytesMax}.

peek(Count, #binlog{queue = Queue}) when is_integer(Count), Count > 0 ->
    peek(Count, Queue, []);
peek(Count, _L) ->
    error({invalid_count, Count}).

truncate(To, #binlog{seq = Seq} = L) when To >= Seq ->
    clear(L);
truncate(To, #binlog{queue = Q0} = L) ->
    case queue:out(Q0) of
        {{value, {Seq, Bin}}, Q1} when Seq =< To ->
            truncate(To, drop(Q1, Bin, L));
        _ ->
            L
    end.

%--- Internal ------------------------------------------------------------------

flush(L) -> flush(L, 0).

flush(#binlog{queue = Q0, count = Count, bytes = Bytes} = L, N) when
    Count > L#binlog.count_max; Bytes > L#binlog.bytes_max
->
    {{value, {_Seq, Bin}}, Q1} = queue:out(Q0),
    flush(drop(Q1, Bin, L), N + 1);
flush(L, N) ->
    {L, N}.

drop(Q1, Bin, #binlog{count = Count, bytes = Bytes} = L) ->
    L#binlog{
        queue = Q1,
        count = Count - 1,
        bytes = Bytes - byte_size(Bin)
    }.

add({Seq, Bin}, L) when Seq > L#binlog.seq ->
    L#binlog{
        seq = Seq,
        queue = queue:in({Seq, Bin}, L#binlog.queue),
        count = L#binlog.count + 1,
        bytes = L#binlog.bytes + byte_size(Bin)
    };
add({Seq, _Bin}, _L) ->
    error({out_of_sequence, Seq}).

peek(0, _Q, Acc) ->
    lists:reverse(Acc);
peek(Count, Q0, Acc) ->
    case queue:out(Q0) of
        {{value, Item}, Q1} -> peek(Count - 1, Q1, [Item | Acc]);
        {empty, _} -> peek(0, Q0, Acc)
    end.

clear(L) -> L#binlog{queue = queue:new(), count = 0, bytes = 0}.

%% @doc Log handler and formatter for the logger app
%%
%% It is optimized to use a fixed size ring buffer and
%% return chucks of older logs first while storing new ones.
%% It can be synched to discard old logs if they are not needed anymore.
%% If the buffer is filled, oldest logs are dropped
%% and a fake logger event is inserted to inform the user.
%% @end
-module(grisp_connect_logger_bin).

-include_lib("kernel/include/logger.hrl").

% API
-export([sync/2]).
-export([chunk/2]).

% Logger Callbacks
-export([adding_handler/1]).
-export([changing_config/3]).
-export([removing_handler/1]).
-export([log/2]).
-export([filter_config/1]).

% Logger Formatter Callbacks
-export([format/2]).

% logger_h_common Callbacks
-export([init/2]).
-export([check_config/4]).
-export([config_changed/3]).
-export([reset_state/2]).
-export([filesync/3]).
-export([write/4]).
-export([handle_info/3]).
-export([terminate/3]).

% Internal Callbacks
-export([queue_ctrl_init/1]).
-export([queue_ctrl_loop/1]).

%--- Macros --------------------------------------------------------------------

-define(CNT_KEY, {?MODULE, counters}).
-define(CNT_N, 1).
-define(CNT_SEQ, 1).

-define(DEFAULT_CONFIG, #{
    count_max => 10_000,
    % 10 MiB:
    bytes_max => 10 * 1_024 * 1_024
}).

-define(DEFAULT_CALL_TIMEOUT, 5000).

% FixMe:
% Sending over ~30_000 bytes over WS breaks rtems I/O driver.
% We want avoid to return chunks that are bigger then that.
-define(MAX_LOG_BYTES, 30_000).

%--- API -----------------------------------------------------------------------

sync(Seq, DroppedConfirmed) when
    is_integer(Seq), is_integer(DroppedConfirmed)
->
    call(whereis(?MODULE), {sync, Seq, DroppedConfirmed}).

%% @doc
%% The chunk will contain Count logs at maximum.
%% while ensuring that the whole chunk is less then MaxBytes.
chunk(MaxCount, MaxBytes) ->
    insert_drop_event(call(whereis(?MODULE), {chunk, MaxCount, MaxBytes})).

%--- Logger Callbacks ----------------------------------------------------------

adding_handler(Config) ->
    logger_h_common:adding_handler(Config).

changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    logger_h_common:changing_config(SetOrUpdate, OldConfig, NewConfig).

removing_handler(Config) ->
    logger_h_common:removing_handler(Config).

log(LogEvent, Config) ->
    logger_h_common:log(LogEvent, Config).

filter_config(Config) ->
    logger_h_common:filter_config(Config).

%--- Logger Formatter Callbacks ------------------------------------------------

format(Event, Config) ->
    case Config of
        #{stdout := {Formatter, FormatterConfig}} ->
            catch io:format(Formatter:format(Event, FormatterConfig));
        _Else ->
            ok
    end,
    jsonify_meta(jsonify_msg(Event)).

jsonify_msg(#{msg := {string, String}} = Event) ->
    maps:put(msg, String, Event);
jsonify_msg(#{msg := {report, Report}} = Event) ->
    case jsx:is_term(Report) of
        true ->
            maps:put(msg, Report, Event);
        false ->
            String = io_lib:format("[JSON incompatible term] ~p", [Report]),
            maps:put(msg, String, Event)
    end;
jsonify_msg(#{msg := {FormatString, Term}} = Event) ->
    String = io_lib:format(FormatString, Term),
    maps:put(msg, String, Event).

jsonify_meta(#{meta := Meta} = Event) ->
    MFA = case maps:is_key(mfa, Meta) of
              true ->
                  {M, F, A} = maps:get(mfa, Meta),
                  [M, F, A];
              false ->
                  null
          end,
    File = case maps:is_key(file, Meta) of
               true  -> unicode:characters_to_binary(maps:get(file, Meta));
               false -> null
           end,
    Default = #{mfa => MFA, file => File},
    Optional = maps:without(maps:keys(Default), Meta),
    FilterFun = fun(Key, Value) -> jsx:is_term(#{Key => Value}) end,
    maps:put(meta, maps:merge(maps:filter(FilterFun, Optional), Default), Event).

%--- logger_h_common Callbacks -------------------------------------------------

init(_Name, UserConfig) ->
    CleanConfig = maps:with(maps:keys(?DEFAULT_CONFIG), UserConfig),
    Counters =
        case persistent_term:get(?CNT_KEY, undefined) of
            undefined ->
                C = atomics:new(?CNT_N, [{signed, false}]),
                persistent_term:put(?CNT_KEY, C),
                C;
            C ->
                C
        end,
    Config = maps:merge(?DEFAULT_CONFIG, CleanConfig),
    {ok, #{pid => queue_ctrl_start(Config#{counters => Counters})}}.

check_config(_Name, _SetOrUpdate, _OldHConfig, NewHConfig0) ->
    {ok, NewHConfig0}.

config_changed(_Name, NewHConfig, #{pid := Pid} = State) ->
    call(Pid, {config_changed, NewHConfig}),
    State.

reset_state(_Name, State) -> State.

filesync(_Name, _SyncAsync, State) ->
    {ok, State}.

write(_Name, async, Bin, #{pid := Pid} = State) ->
    Pid ! {log, Bin},
    {ok, State};
write(_Name, sync, Bin, #{pid := Pid} = State) ->
    Result = call(Pid, {log, Bin}),
    {Result, State}.

handle_info(_, _, State) ->
    State.

terminate(_Name, _Reason, _State) ->
    ok.

%--- Internal ------------------------------------------------------------------

call(Pid, Msg) ->
    MRef = monitor(process, Pid),
    Pid ! {Msg, {self(), MRef}},
    receive
        {MRef, Result} ->
            demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, _Type, _Object, Reason} ->
            {error, Reason}
    after ?DEFAULT_CALL_TIMEOUT ->
        %% If this timeout triggers we will get a stray
        %% reply message in our mailbox eventually.
        %% That does not really matter though as it will
        %% end up in this module's handle_info and be ignored
        demonitor(MRef, [flush]),
        {error, {no_response, Pid}}
    end.

queue_ctrl_start(Config) ->
    spawn_link(fun() -> ?MODULE:queue_ctrl_init(Config) end).

queue_ctrl_init(#{counters := Counters} = Config) ->
    register(?MODULE, self()),
    ?MODULE:queue_ctrl_loop(#{
        counters => Counters,
        binlog => grisp_connect_binlog:new(Config),
        dropped => 0
    }).

queue_ctrl_loop(S0) ->
    S2 =
        receive
            {log, Bin} ->
                insert(Bin, S0);
            {{log, Bin}, From} ->
                S1 = insert(Bin, S0),
                reply(From, ok),
                S1;
            {config_changed, _Config} ->
                S0;
            {{sync, Seq, DroppedConfirmed}, From} ->
                S1 = sync(Seq, DroppedConfirmed, S0),
                reply(From, ok),
                S1;
            {{chunk, Count, Bytes}, From} ->
                reply(From, peek(Count, Bytes, S0)),
                S0
        end,
    ?MODULE:queue_ctrl_loop(S2).

reply({From, MRef}, Msg) -> From ! {MRef, Msg}.

insert(Event, #{counters := Counters, binlog := B0, dropped := D0} = State) ->
    Seq = atomics:add_get(Counters, ?CNT_SEQ, 1),
    {B1, D1} = grisp_connect_binlog:insert({Seq, Event}, B0),
    State#{binlog := B1, dropped := D0 + D1}.

sync(Seq, DroppedConfirmed, #{binlog := B0, dropped := Dropped} = State) ->
    B1 = grisp_connect_binlog:truncate(Seq, B0),
    State#{binlog := B1, dropped := Dropped - DroppedConfirmed}.

peek(Count, MaxBytes, #{binlog := B0, dropped := Dropped}) ->
    Events = grisp_connect_binlog:peek(Count, B0),
    {trim_to_size(Events, MaxBytes), Dropped}.

trim_to_size(Events, MaxBytes) ->
    rec_trim_to_size(Events, MaxBytes, 0, []).

rec_trim_to_size([], _, _, Acc) ->
    lists:reverse(Acc);
rec_trim_to_size([{_Seq, Bin} = E | TL], MaxBytes, AccSize, Acc) ->
    NewSize = byte_size(Bin) + AccSize,
    case NewSize > MaxBytes of
        true -> lists:reverse(Acc);
        false -> rec_trim_to_size(TL, MaxBytes, NewSize, [E | Acc])
    end.

insert_drop_event({Events, 0}) ->
    {Events, 0};
insert_drop_event({Events, Dropped}) ->
    {[drop_event(Dropped) | Events], Dropped}.

drop_event(Count) ->
    % Create a fake logger event mimicking a real log event as much as possible
    Meta = simulate_log_metadata(),
    Report = {report, #{event => log_drop, count => Count}},
    {null, format(#{level => warning, meta => Meta, msg => Report}, #{})}.

simulate_log_metadata() ->
    % See internal function logger:add_default_metadata/2
    DefaultMetadata = #{
        pid => self(),
        gl => group_leader(),
        time => logger:timestamp()
    },

    PrimaryConfig = logger:get_primary_config(),
    PrimaryMetadata = maps:get(logger_metadata, PrimaryConfig, #{}),

    % See internal function logger:log_allowed/4
    ProcessMetadata =
        case logger:get_process_metadata() of
            ProcMeta when is_map(ProcMeta) -> ProcMeta;
            _ -> #{}
        end,

    lists:foldl(
        fun(M, MAcc) ->
            maps:merge(MAcc, M)
        end,
        DefaultMetadata,
        [?LOCATION, PrimaryMetadata, ProcessMetadata]
    ).

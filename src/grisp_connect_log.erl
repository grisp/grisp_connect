%% @doc Utility module to handle logs.
%% @end
-module(grisp_connect_log).

-include_lib("kernel/include/logger.hrl").

% API functions
-export([get/1]).
-export([sync/1]).

%--- Macros --------------------------------------------------------------------

% FixMe:
% Sending over ~30_000 bytes over WS breaks rtems I/O driver.
% We want avoid to return chunks that are bigger then that.
-define(MAX_CHUNK_BYTES, 30_000).

%--- Types ---------------------------------------------------------------------

-type get_options() :: #{
    max_batch_size => non_neg_integer(),
    max_byte_size => non_neg_integer()
}.

-type sync_options() :: #{
    seq := non_neg_integer(),
    dropped => non_neg_integer()
}.


%--- API Functions -------------------------------------------------------------

-spec get(Opts :: get_options()) ->
    #{dropped := non_neg_integer(), events := list()}.
get(Opts) ->
    {ok, DefaultSize} = application:get_env(grisp_connect, logs_batch_size),
    BatchSize = maps:get(max_batch_size, Opts, DefaultSize),
    ByteSize = min(maps:get(max_byte_size, Opts, ?MAX_CHUNK_BYTES), ?MAX_CHUNK_BYTES),
    {Events, Dropped} = grisp_connect_logger_bin:chunk(BatchSize, ByteSize),
    #{events => [[Seq, jsonify(E)] || {Seq, E} <- Events],
      dropped => Dropped}.

-spec sync(Opts :: sync_options()) -> ok.
sync(#{seq := Seq, dropped := Dropped}) ->
    grisp_connect_logger_bin:sync(Seq, Dropped).


%--- Internal Functions --------------------------------------------------------

jsonify(Event) ->
    jsonify_meta(jsonify_msg(binary_to_term(base64:decode(Event)))).

jsonify_msg(#{msg := {string, String}} = Event) ->
    maps:put(msg, unicode:characters_to_binary(String), Event);
jsonify_msg(#{msg := {report, Report}} = Event) ->
    case is_json_compatible(Report) of
        true ->
            maps:put(msg, Report, Event);
        false ->
            String = unicode:characters_to_binary(
                       io_lib:format("~tp", [Report])),
            Meta = maps:get(meta, Event, #{}),
            Event2 = Event#{meta => Meta#{incompatible_term => true}},
            maps:put(msg, String, Event2)
    end;
jsonify_msg(#{msg := {FormatString, Term}} = Event) ->
    %FIXME: scan format and ensure unicode encoding
    String = unicode:characters_to_binary(io_lib:format(FormatString, Term)),
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

is_json_compatible(Term) ->
    try jsx:is_term(Term)
    catch error:_ ->
        false
    end.

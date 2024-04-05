%% @doc gen_server that sends logs to GRiSP.io on a timed intervall.
%%
%% This process is controlled by grisp_connect_client.
%% @end
-module(grisp_connect_log_server).

% API
-export([start_link/0]).
-export([start/0]).
-export([stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    active = false :: boolean(),
    timer :: undefined | timer:tref()
}).

% FixMe:
% Sending over ~30_000 bytes over WS breaks rtems I/O driver.
% We want avoid to return chunks that are bigger then that.
-define(MAX_CHUNK_BYTES, 30_000).

% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() -> gen_server:cast(?MODULE, ?FUNCTION_NAME).

stop() -> gen_server:cast(?MODULE, ?FUNCTION_NAME).

% gen_server CALLBACKS ---------------------------------------------------------

init([]) -> {ok, #state{}}.

handle_call(_, _, _) ->
    error(?FUNCTION_NAME).

handle_cast(start, #state{active = false, timer = undefined}) ->
    {ok, Interval} = application:get_env(grisp_connect, logs_interval),
    {ok, Tref} = timer:send_interval(Interval, send_logs),
    {noreply, #state{active = true, timer = Tref}};
handle_cast(stop, State) ->
    timer:cancel(State#state.timer),
    {noreply, #state{}}.

handle_info(send_logs, #state{active = true} = State) ->
    {ok, Size} = application:get_env(grisp_connect, logs_batch_size),
    case grisp_connect_logger_bin:chunk(Size, ?MAX_CHUNK_BYTES) of
        {[], _Dropped} -> ok;
        Chunk -> send_logs_chunk(Chunk)
    end,
    {noreply, State};
handle_info(send_logs, #state{active = false, timer = undefined} = State) ->
    ?LOG_WARNING(#{event => send_logs,
                   msg => "send_logs received when inactive"}),
    {noreply, State}.

send_logs_chunk({Events, Dropped}) ->
    LogUpdate = #{
        events => [[Seq, E] || {Seq, E} <- Events],
        dropped => Dropped
    },
    case grisp_connect_client:request(post, logs, LogUpdate) of
        {ok, #{seq := Seq, dropped := ServerDropped}} ->
            grisp_connect_logger_bin:sync(Seq, ServerDropped);
        E ->
            ?LOG_ERROR(#{event => send_logs, data => E})
    end.

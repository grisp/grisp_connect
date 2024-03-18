%% @doc gen_server that sends logs to GRiSP.io on a timed intervall.
%%
%% This process is controlled by grisp_io_client.
%% @end
-module(grisp_io_log_server).

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
    {ok, Interval} = application:get_env(grisp_io, logs_interval),
    {ok, Tref} = timer:send_interval(Interval, send_logs),
    {noreply, #state{active = true, timer = Tref}};
handle_cast(stop, State) ->
    timer:cancel(State#state.timer),
    {noreply, #state{}}.

handle_info(send_logs, #state{active = true} = State) ->
    {ok, Size} = application:get_env(grisp_io, logs_batch_size),
    case grisp_io_logger_bin:chunk(Size) of
        {[], _Dropped} -> ok;
        Chunk -> send_logs_chunk(Chunk)
    end,
    {noreply, State}.

send_logs_chunk({Events, Dropped}) ->
    LogUpdate = #{
        events => [[Seq, E] || {Seq, E} <- Events],
        dropped => Dropped
    },
    case grisp_io_client:request(post, logs, LogUpdate) of
        {ok, #{seq := Seq, dropped := ServerDropped}} ->
            grisp_io_logger_bin:sync(Seq, ServerDropped);
        E ->
            ?LOG_WARNING("Error sending logs = ~p\n",[E])
    end.

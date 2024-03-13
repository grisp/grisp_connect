% @doc temporary module to test logging
-module(grisp_io_logtest).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
    timer
}).

% -include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% Callbacks


init(_) ->
    {ok, Interval} = application:get_env(grisp_io, ws_logs_interval),
    Timer = erlang:start_timer(Interval, self(), logs),
    {ok, #state{timer = Timer}}.

handle_call(_Request, _From, _State) ->
    error(unknown_request).

handle_cast(_Msg, _State) ->
    error(unknown_cast).

handle_info({timeout, OldRef, logs}, #state{timer = OldRef} = S)->
    {ok, Size} = application:get_env(grisp_io, ws_logs_batch_size),
    {ok, Interval} = application:get_env(grisp_io, ws_logs_interval),
    {Events, Dropped} = Chunk = grisp_io_logger_bin:chunk(Size),
    io:format("Sending logs... events = ~p, dropped = ~p\n",
             [length(Events), Dropped]),
    case send_logs(Chunk) of
        ok -> ok;
        {ok, #{seq := Seq, dropped := ServerDropped}} ->
            dab_logger_bin:sync(Seq, ServerDropped);
        E ->
            io:format("Error sending logs = ~p\n",[E])
    end,
    erlang:cancel_timer(OldRef),
    NewTimer = erlang:start_timer(Interval, self(), logs),
    {noreply, S#state{timer = NewTimer}}.

%--- Internals -----------------------------------------------------------------

send_logs({[], _Dropped}) ->
    ok;
send_logs({Events, Dropped}) ->
    LogUpdate = #{
        events => [[Seq, E] || {Seq, E} <- Events],
        dropped => Dropped
    },
    grisp_io_ws:request(post, logs, LogUpdate).

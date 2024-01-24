%% @doc GRiSP.io Websocket Client
-module(grisp_io_ws).

-export([start_link/0]).
-export([connect/0]).
-export([connect/2]).
-export([is_connected/0]).
-export([send/1]).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {
    gun_pid,
    gun_ref,
    ws_stream,
    ws_up = false,
    requests = #{}
}).

-define(disconnected_state,
        #state{gun_pid = undefined, gun_ref = undefine, ws_up = false}).

-include_lib("kernel/include/logger.hrl").

%--- API Functions -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    {ok, Domain} = application:get_env(grisp_io, grisp_io_domain),
    {ok, Port} = application:get_env(grisp_io, grisp_io_port),
    connect(Domain, Port).

connect(Server, Port) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Server, Port}).


is_connected() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

send(Payload) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Payload}).

% gen_server callbacks ---------------------------------------------------------

init([]) -> {ok, #state{}}.

handle_call(is_connected, _, #state{ws_up = Up} = S) ->
    {reply, Up, S}.

handle_cast({connect, Server, Port}, #state{gun_pid = undefined} = S) ->
    case grisp_io_tls:connect(Server, Port) of
        {ok, GunPid} ->
            {noreply, #state{gun_pid = GunPid}};
        Error ->
            grisp_io_client ! {connection_fail, Error},
            {noreply, S}
    end;
handle_cast({connect, _Server, _Port}, S) ->
    {noreply, S};
handle_cast({send, Payload},  #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    gun:ws_send(Pid, Stream, {text, Payload}),
    {noreply, S}.

handle_info({gun_up, GunPid, _}, #state{gun_pid = GunPid} = S) ->
    ?LOG_INFO("HTTP connection enstablished, upgrading to WS..."),
    GunRef = monitor(process, GunPid),
    WsStream = gun:ws_upgrade(GunPid, "/grisp-connect/ws"),
    NewState = S#state{gun_pid = GunPid, gun_ref = GunRef, ws_stream = WsStream},
    {noreply, NewState};
handle_info({gun_upgrade, Pid, Stream, [<<"websocket">>], _},
            #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_INFO("WS Upgraded"),
    {noreply, S#state{ws_up = true}};
handle_info({gun_response, Pid, Stream, _, Status, Headers},
            #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_ERROR("WS Upgrade fail with status ~p", [Status]),
    grisp_io_client ! {connection_fail, upgrade},
    {noreply, shutdown_gun(S)};
handle_info({gun_ws, Conn, Stream, {text, Text}},
            #state{gun_pid = Conn, ws_stream = Stream} = S) ->
    case grisp_io_client:handle_message(Text) of
        noreply ->
            {noreply, S};
        {reply, Text} ->
            gun:ws_send(Conn, Stream, {text, Text}),
            {noreply, S}
    end;
handle_info({timeout, TRef, ID}, #state{requests = Reqs} = State) ->
    case maps:get(ID, Reqs, undefined) of
        {Caller, TRef} ->
            NewS = State#state{requests = maps:remove(ID, Reqs)},
            gen_server:reply(Caller, {error, timeout}),
            {noreply, NewS};
        _ -> error(unexpected_timeout)
    end;
handle_info({gun_down, Pid, ws, closed, [Stream]},
            #state{gun_pid = Pid, ws_stream = Stream, requests = Requests} = S) ->
    ?LOG_WARNING("Websocket down!"),
    [begin
        erlang:cancel_timer(Tref),
        gen_server:reply(Caller, {error, ws_closed})
     end || {Caller, Tref} <- maps:values(Requests)],
    {noreply, shutdown_gun(S#state{requests = #{}})};
handle_info(M, S) ->
    ?LOG_WARNING("Unandled WS message: ~p", [M]),
    {noreply, S}.

% internal functions -----------------------------

shutdown_gun(#state{gun_pid = Pid} = State) ->
    gun:shutdown(Pid),
    grisp_io_client ! disconnected,
    State?disconnected_state.

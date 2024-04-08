%% @doc Websocket client to connect to grisp.io
-module(grisp_connect_ws).

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
    ws_up = false
}).

-define(disconnected_state,
        #state{gun_pid = undefined, gun_ref = undefine, ws_up = false}).

-include_lib("kernel/include/logger.hrl").

%--- API Functions -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    {ok, Domain} = application:get_env(grisp_connect, domain),
    {ok, Port} = application:get_env(grisp_connect, port),
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
    case grisp_connect_tls:connect(Server, Port) of
        {ok, GunPid} ->
            {noreply, #state{gun_pid = GunPid}};
        Error ->
            ?LOG_ERROR(#{event => connection_failure, reason => Error}),
            {noreply, S}
    end;
handle_cast({connect, _Server, _Port}, S) ->
    {noreply, S};
handle_cast({send, _}, #state{ws_up = false} = S) ->
    ?LOG_ERROR(#{event => ws_send, reason => ws_disconnected}),
    {noreply, S};
handle_cast({send, Payload},  #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    gun:ws_send(Pid, Stream, {text, Payload}),
    {noreply, S}.

handle_info({gun_up, GunPid, _}, #state{gun_pid = GunPid} = S) ->
    ?LOG_INFO(#{event => connection_enstablished}),
    GunRef = monitor(process, GunPid),
    WsStream = gun:ws_upgrade(GunPid, "/grisp-connect/ws"),
    NewState = S#state{gun_pid = GunPid, gun_ref = GunRef, ws_stream = WsStream},
    {noreply, NewState};
handle_info({gun_upgrade, Pid, Stream, [<<"websocket">>], _},
            #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_INFO(#{event => ws_upgrade}),
    {noreply, S#state{ws_up = true}};
handle_info({gun_response, Pid, Stream, _, Status, _Headers},
            #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_ERROR(#{event => ws_upgrade_failure, status => Status}),
    {noreply, shutdown_gun(S)};
handle_info({gun_ws, Conn, Stream, {text, Text}},
            #state{gun_pid = Conn, ws_stream = Stream} = S) ->
    grisp_connect_client:handle_message(Text),
    {noreply, S};
handle_info({gun_down, Pid, ws, closed, [Stream]}, #state{gun_pid = Pid, ws_stream = Stream} = S) ->
    ?LOG_WARNING(#{event => ws_closed}),
    grisp_connect_client:disconnected(),
    {noreply, shutdown_gun(S)};
handle_info(M, S) ->
    ?LOG_WARNING(#{event => unhandled_info, info => M}),
    {noreply, S}.

% internal functions -----------------------------------------------------------

shutdown_gun(#state{gun_pid = Pid} = State) ->
    gun:shutdown(Pid),
    State?disconnected_state.
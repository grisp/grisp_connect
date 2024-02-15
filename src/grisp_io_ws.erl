%% @doc Websocket client to connect to grisp.io
-module(grisp_io_ws).

-export([start_link/0]).
-export([connect/0]).
-export([connect/2]).
-export([is_connected/0]).
-export([request/3]).

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

-define(call_timeout, ws_request_timeout() + 1000).
-define(disconnected_state,
        #state{gun_pid = undefined, gun_ref = undefine, ws_up = false}).

-include_lib("kernel/include/logger.hrl").

%--- API Functions -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    {ok, Domain} = application:get_env(grisp_io, domain),
    {ok, Port} = application:get_env(grisp_io, port),
    connect(Domain, Port).

connect(Server, Port) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Server, Port}).

is_connected() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

request(Method, Type, Params) ->
    gen_server:call(?MODULE,
                    {?FUNCTION_NAME, Method, Type, Params},
                    ?call_timeout).

% gen_server callbacks ---------------------------------------------------------

init([]) -> {ok, #state{}}.

handle_call(is_connected, _, #state{ws_up = Up} = S) ->
    {reply, Up, S};
handle_call(_, _, #state{ws_up = false} = S) ->
    {reply, {error, disconnected}, S};
handle_call({request, Method, Type, Params}, From,
            #state{gun_pid = GunPid, ws_stream = WsStream,
                   requests = Requests, ws_up = true} = S) ->
    {ID, Payload} = grisp_io_api:request(Method, Type, Params),
    gun:ws_send(GunPid, WsStream, {text, Payload}),
    TRef = erlang:start_timer(ws_request_timeout(), self(), ID),
    NewRequests = Requests#{ID => {From, TRef}},
    {noreply, S#state{requests= NewRequests}}.

handle_cast({connect, Server, Port}, #state{gun_pid = undefined} = S) ->
    case grisp_io_tls:connect(Server, Port) of
        {ok, GunPid} ->
            {noreply, #state{gun_pid = GunPid}};
        Error ->
            ?LOG_ERROR(#{event => connection_failure, reason => Error}),
            {noreply, S}
    end;
handle_cast({connect, _Server, _Port}, S) ->
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
handle_info({gun_ws, Conn, Stream, {text, JSON}},
            #state{gun_pid = Conn, ws_stream = Stream,
                   requests = Requests}= S) ->
    Replyes = grisp_io_api:handle_msg(JSON),
    NewS = case Replyes of
        [] -> S;
        [{request, Response}] ->
            gun:ws_send(Conn, Stream, {text, Response}),
            S;
        [{response, ID, Response}] ->
            case maps:take(ID, Requests) of
                {{Caller, Tref}, NewRequests} ->
                    erlang:cancel_timer(Tref),
                    gen_server:reply(Caller, Response),
                    S#state{requests = NewRequests};
                error ->
                    ?LOG_WARNING(#{event => unexpected_jsonrpc_responce,
                                   id => ID}),
                    S
            end
    end,
    {noreply, NewS};
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
    ?LOG_WARNING(#{event => ws_closed}),
    [begin
        erlang:cancel_timer(Tref),
        gen_server:reply(Caller, {error, ws_closed})
     end || {Caller, Tref} <- maps:values(Requests)],
    grisp_io_connection:disconnected(),
    {noreply, shutdown_gun(S#state{requests = #{}})};
handle_info(M, S) ->
    ?LOG_WARNING(#{event => unhandled_info, info => M}),
    {noreply, S}.

% internal functions -----------------------------------------------------------

shutdown_gun(#state{gun_pid = Pid} = State) ->
    gun:shutdown(Pid),
    State?disconnected_state.

ws_request_timeout() ->
    {ok, Timeout} = application:get_env(grisp_io, ws_requests_timeout),
    Timeout.

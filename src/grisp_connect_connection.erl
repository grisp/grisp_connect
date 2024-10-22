%% @doc JSONRpc 2.0 Websocket connection
-module(grisp_connect_connection).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-import([grisp_connect_utils, as_bin/1]).
-import([grisp_connect_utils, maybe_atom/1]).
-import([grisp_connect_utils, parse_method/1]).

% API Functions
-export([start_link/2]).
-export([request/3]).
-export([post/4]).
-export([notify/3]).
-export([reply/3]).
-export([error/5]).
-export([disconnect/1]).

% Behaviour gen_server Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).


%--- Types ---------------------------------------------------------------------

-record(req, {
    method :: method(),
    id :: binary(),
    tref :: reference(),
    from :: undefined | gen_server:from(),
    ctx :: undefined | term()
}).

-record(state, {
    handler :: pid(),
    domain :: binary(),
    port :: inet:port_number(),
    path :: binary(),
    ping_timeout :: infinity | pos_integer(),
    request_timeout :: infinity | pos_integer(),
    error_tags = #{} :: #{atom() => {integer(), binary()}},
    error_codes = #{} :: #{integer() => {atom(), binary()}},
    requests = #{} :: #{binary() => #req{}},
    gun_pid :: undefined | pid(),
    gun_ref :: undefined | reference(),
    ws_stream :: undefined | gun:stream_ref(),
    connected = false :: boolean(),
    ping_tref :: reference()
}).

-type error_mapping() :: {atom(), integer(), binary()}.
-type method() :: atom() | binary() | [atom() | binary()].
-type start_options() :: #{
    domain := atom() | string() | binary(),
    port := inet:port_number(),
    transport := tcp | tls | {tls, TlsOpts :: [ssl:tls_client_option()]},
    path := atom() | string() | binary(),
    errors => [error_mapping()],
    ping_timeout => infinity | pos_integer(),
    request_timeout => infinity | pos_integer()
}.

-export_type([error_mapping/0, method/0]).


%--- Macros --------------------------------------------------------------------

-define(ENABLE_TRACE, true).

-define(DEFAULT_PING_TIMEOUT, 60_000).
-define(DEFAULT_REQUEST_TIMEOUT, 5_000).
-define(DEFAULT_TRANSPORT, tcp).

-define(DEFAULT_JSONRPC_ERRORS, [
    {invalid_json, -32700, <<"Parse error">>},
    {invalid_request, -32600, <<"Invalid Request">>},
    {method_not_found, -32601, <<"Method not found">>},
    {invalid_params, -32602, <<"Invalid parameters">>},
    {internal_error, -32603, <<"Internal error">>}
]).


%--- API Functions -------------------------------------------------------------

-spec start_link(Handler :: pid(), Options :: start_options()) ->
    {ok, Conn :: pid()} | {error, Reason :: term()}.
start_link(Handler, Opts = #{domain := _, port := _, path := _}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                     [Handler, Opts], []).

-spec request(Conn :: pid(), Method :: method(), Params :: map()) ->
    {ok, Result :: term()} | {error, timeout} | {error, not_connected}
    | {error, Code :: integer(),
              Message :: undefined | binary(), Data :: term()}.
request(Conn, Method, Params) ->
    case gen_server:call(Conn, {request, parse_method(Method), Params}) of
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack);
        Result -> Result
    end.

-spec post(Conn :: pid(), Method :: method(), Params :: map(),
           ReqCtx :: term()) -> ok | {error, not_connected}.
post(Conn, Method, Params, ReqCtx) ->
    case gen_server:call(Conn, {post, parse_method(Method), Params, ReqCtx}) of
        {ok, Result} -> Result;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec notify(Conn :: pid(), Method :: method(), Params :: map()) ->
    ok | {error, not_connected}.
notify(Conn, Method, Params) ->
    case gen_server:call(Conn, {notify, parse_method(Method), Params}) of
        {ok, Result} -> Result;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec reply(Conn :: pid(), Result :: any(), ReqRef :: binary()) ->
    ok | {error, not_connected}.
reply(Conn, Result, ReqRef) ->
    case gen_server:call(Conn, {reply, Result, ReqRef}) of
        {ok, Result} -> Result;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec error(Conn :: pid(), Code :: atom() | integer(),
            Message :: undefined | binary(), Data :: term(),
            ReqRef :: undefined | binary()) ->
    ok | {error, not_connected}.
error(Conn, Code, Message, Data, ReqRef) ->
    case gen_server:call(Conn, {error, Code, Message, Data, ReqRef}) of
        {ok, Result} -> Result;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec disconnect(Conn :: pid()) -> ok.
disconnect(Conn) ->
    gen_server:call(Conn, disconnect).


%--- Behaviour gen_server Callbacks --------------------------------------------

init([Handler, Opts]) ->
    #{domain := Domain, port := Port, path := Path} = Opts,
    PingTimeout = maps:get(ping_timeout, Opts, ?DEFAULT_PING_TIMEOUT),
    ReqTimeout = maps:get(request_timeout, Opts, ?DEFAULT_REQUEST_TIMEOUT),
    Transport = maps:get(transport, Opts, ?DEFAULT_TRANSPORT),
    State = #state{
        handler = Handler,
        domain = as_bin(Domain),
        port = Port,
        path = as_bin(Path),
        ping_timeout = PingTimeout,
        request_timeout = ReqTimeout
    },
    State2 = index_errors(State, ?DEFAULT_JSONRPC_ERRORS),
    State3 = index_errors(State2, maps:get(errors, Opts, [])),
    case connection_start(State3, Transport) of
        {ok, _State2} = Result -> Result;
        {error, Reason} -> {stop, Reason}
    end.

handle_call({request, Method, _Params}, _From,
            State = #state{connected = false}) ->
    ?LOG_WARNING("Request ~s performed while disconnected", [Method],
                 #{event => request, reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({request, Method, Params}, From, State) ->
    try send_request(State, Method, Params, From, undefined) of
        State2 -> {noreply, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;

handle_call({post, Method, _Params, ReqCtx}, _From,
            State = #state{handler = Handler, connected = false}) ->
    ?LOG_WARNING("Request ~s posted while disconnected", [Method],
                 #{event => post, reason => not_connected}),
    % Notify the handler anyway so it doesn't have to make a special case
    Handler ! {conn, self(), {error, Method, not_connected, ReqCtx}},
    {reply, {error, not_connected}, State};
handle_call({post, Method, Params, ReqCtx}, _From, State) ->
    try send_request(State, Method, Params, undefined, ReqCtx) of
        State2 -> {reply, ok, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;        
handle_call({notify, Method, _Params}, _From,
            State = #state{connected = false}) ->
    ?LOG_WARNING("Notification ~s posted while disconnected", [Method],
                 #{event => notify, reason => not_connected}),
    {noreply, State};
handle_call({notify, Method, Params}, _From, State) ->
    try send_notification(State, Method, Params) of
        State2 -> {reply, ok, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call({reply, _Result, ReqRef}, _From,
            State = #state{connected = false}) ->
    ?LOG_WARNING("Reply to ~s posted while disconnected",
                 [req_tag(State, ReqRef)],
                 #{event => reply, reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({reply, Result, ReqRef}, _From, State) ->
    try send_response(State, Result, ReqRef) of
        State2 -> {reply, ok, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call({error, Code, _Message, _Data, undefined}, _From,
            State = #state{connected = false}) ->
    ?LOG_WARNING("Error ~w posted while disconnected", [Code],
                 #{event => error, reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({error, Code, _Message, _Data, ReqRef}, _From,
            State = #state{connected = false}) ->
    ?LOG_WARNING("Error ~w to ~s posted while disconnected",
                 [Code, req_tag(State, ReqRef)],
                 #{event => error, reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({error, Code, Message, Data, ReqRef}, _From, State) ->
    try send_error(State, Code, Message, Data, ReqRef) of
        State2 -> {reply, ok, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call(disconnect, _From, State) ->
    try connection_close(State) of
        State2 -> {reply, ok, State2}
    catch
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call(Call, From, State) ->
    ?LOG_ERROR("Unexpected call from ~p: ~p", [From, Call],
               #{reason => unexpected_call}),
    {stop, unexpected_call, {error, unexpected_call}, State}.

handle_cast(Cast, State) ->
    Reason = {unexpected_cast, Cast},
    ?LOG_ERROR(#{event => ws_internal, reason => Reason},
               #{reason => unexpected_cast}),
    {stop, Reason, State}.

handle_info({gun_up, GunPid, _}, State = #state{gun_pid = GunPid}) ->
    ?LOG_INFO("Connection to ~s:~w established",
              [State#state.domain, State#state.port],
              #{event => connection_enstablished}),
    {noreply, connection_upgrade(State)};
handle_info({gun_up, Pid, http}, State = #state{gun_pid = GunPid}) ->
    ?LOG_WARNING("Ignoring unexpected gun_up message"
                 " from pid ~p, current pid is ~p", [Pid, GunPid]),
    {noreply, State};
handle_info({gun_upgrade, Pid, Stream, [<<"websocket">>], _},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?LOG_DEBUG("Connection to ~s:~w upgraded to websocket",
               [State#state.domain, State#state.port]),
    {noreply, connection_established(State)};
handle_info({gun_response, Pid, Stream, _, Status, _Headers},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?LOG_ERROR("Connection to ~s:~w failed to upgrade to websocket: ~p",
               [State#state.domain, State#state.port, Status]),
    {stop, connection_close(State)};
handle_info({gun_ws, Pid, Stream, ping},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    {noreply, schedule_ping_timeout(State)};
handle_info({gun_ws, Pid, Stream, {text, Text}},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    {noreply, process_data(State, Text)};
handle_info({gun_ws, Pid, Stream, {close, Code, Message}},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?LOG_WARNING("Connection to ~s:~w closed: ~s (~w)",
                 [State#state.domain, State#state.port, Message, Code],
                 #{event => stream_closed, code => Code, reason => Message}),
    {stop, connection_closed(State, stream_closed)};
handle_info({gun_error, Pid, _Stream, Reason},
            State = #state{gun_pid = Pid}) ->
    ?LOG_ERROR("Connection to ~s:~w got an error: ~p",
               [State#state.domain, State#state.port, Reason],
               #{event => ws_closed, reason => Reason}),
    {stop, connection_close(State)};
handle_info({gun_down, Pid, ws, closed, [Stream]},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?LOG_WARNING("Connection to ~s:~w was closed by the server",
                 [State#state.domain, State#state.port],
                 #{event => ws_closed}),
    {stop, connection_close(State)};
handle_info({gun_down, Pid, ws, normal, _},
            State = #state{gun_pid = Pid}) ->
    ?LOG_INFO("Connection to ~s:~w closed",
              [State#state.domain, State#state.port],
              #{event => ws_closed, reason => normal}),
    {stop, connection_close(State)};
handle_info({'DOWN', _, process, Pid, Reason},
            State = #state{gun_pid = Pid}) ->
    ?LOG_WARNING("gun process of the connection to ~s:~w crashed: ~p",
                 [State#state.domain, State#state.port, Reason],
                 #{event => gun_crash, reason => Reason}),
    {noreply, connection_closed(State, gun_crashed)};
handle_info(ping_timeout, State) ->
    ?LOG_WARNING("Connection to ~s:~w timed out",
                 [State#state.domain, State#state.port],
                 #{event => ping_timeout}),
    {noreply, connection_close(State)};
handle_info({request_timeout, ReqRef}, State) ->
    ?LOG_WARNING("Request ~s time to ~s:~w timed out",
                 [req_tag(State, ReqRef), State#state.domain, State#state.port],
                 #{event => ping_timeout}),
    {noreply, request_timeout(State, ReqRef)};
handle_info(Msg, State) ->
    ?LOG_WARNING("Unexpected info message: ~p", [Msg],
                 #{event => unexpected_info, info => Msg}),
    {noreply, State}.


%--- INTERNAL FUNCTION ---------------------------------------------------------

make_reqref() ->
    list_to_binary(integer_to_list(erlang:unique_integer())).

send_after(infinity, _Message) -> undefined;
send_after(Timeout, Message) ->
    erlang:send_after(Timeout, self(), Message).

cancel_timer(undefined) -> ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

schedule_ping_timeout(State = #state{ping_timeout = Timeout}) ->
    State2 = cancel_ping_timeout(State),
    TRef = send_after(Timeout, ping_timeout),
    State2#state{ping_tref = TRef}.

cancel_ping_timeout(State = #state{ping_tref = undefined}) ->
    State;
cancel_ping_timeout(State = #state{ping_tref = TRef}) ->
    cancel_timer(TRef),
    State#state{ping_tref = undefined}.

% Returns either the request method of the request reference if not found.
% Only mean for logging.
req_tag(#state{requests = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> ReqRef;
        {ok, #req{method = Method}} -> format_method(Method)
    end.

index_errors(State = #state{error_tags = ErrorTags, error_codes = ErrorCodes},
             ErrorSpecs) ->
    {ErrorTags2, ErrorCodes2} =
        lists:foldl(fun({Tag, Code, Msg}, {Tags, Codes}) ->
            {Tags#{Tag => {Code, Msg}}, Codes#{Code => {Tag, Msg}}}
        end, {ErrorTags, ErrorCodes}, ErrorSpecs),
    State#state{error_tags = ErrorTags2, error_codes = ErrorCodes2}.

decode_error(#state{error_codes = ErrorCodes}, Code, Message)
  when is_integer(Code), Message =:= null orelse Message =:= undefined ->
    case maps:find(Code, ErrorCodes) of
        error -> {Code, undefined};
        {ok, {Tag, DefaultMessage}} -> {Tag, DefaultMessage}
    end;
decode_error(#state{error_codes = ErrorCodes}, Code, Message)
  when is_integer(Code) ->
    case maps:find(Code, ErrorCodes) of
        error -> {Code, Message};
        {ok, {Tag, _DefaultMessage}} -> {Tag, Message}
    end;
decode_error(#state{error_tags = ErrorTags}, Tag, Message)
  when is_atom(Tag), Message =:= null orelse Message =:= undefined ->
    case maps:find(Tag, ErrorTags) of
        error -> erlang:error(badarg);
        {ok, {_Code, DefaultMessage}} -> {Tag, DefaultMessage}
    end;
decode_error(#state{error_tags = ErrorTags}, Tag, Message)
  when is_atom(Tag) ->
    case maps:find(Tag, ErrorTags) of
        error -> erlang:error(badarg);
        {ok, {_Code, _DefaultMessage}} -> {Tag, Message}
    end.

encode_error(#state{error_codes = ErrorCodes}, Code, Message)
  when is_integer(Code), Message =:= null orelse Message =:= undefined ->
    case maps:find(Code, ErrorCodes) of
        error -> {Code, null};
        {ok, {_Tag, DefaultMessage}} -> {Code, DefaultMessage}
    end;
encode_error(_State, Code, Message)
  when is_integer(Code) ->
    {Code, Message};
encode_error(#state{error_tags = ErrorTags}, Tag, Message)
  when is_atom(Tag), Message =:= null orelse Message =:= undefined ->
    case maps:find(Tag, ErrorTags) of
        error -> erlang:error(badarg);
        {ok, {Code, DefaultMessage}} -> {Code, DefaultMessage}
    end;
encode_error(#state{error_tags = ErrorTags}, Tag, Message)
  when is_atom(Tag) ->
    case maps:find(Tag, ErrorTags) of
        error -> erlang:error(badarg);
        {ok, {Code, _DefaultMessage}} -> {Code, Message}
    end.

connection_start(State = #state{domain = Domain, port = Port}, TransportSpec) ->
    {Transport, TlsOpts} = case TransportSpec of
        tcp -> {tcp, undefined};
        tls -> {tls, undefined};
        {tls, Opts} -> {tls, Opts}
    end,
    GunOpts = #{
        protocols => [http],
        transport => Transport,
        retry => 0,
        tls_opts => TlsOpts
    },
    case gun:open(Domain, Port, GunOpts) of
        {ok, GunPid} ->
            GunRef = monitor(process, GunPid),
            {ok, State#state{gun_pid = GunPid, gun_ref = GunRef}};
        {error, Reason} ->
            ?LOG_ERROR(#{event => connection_failure, reason => Reason}),
            {error, Reason}
    end.

connection_upgrade(State = #state{path = Path, gun_pid = GunPid}) ->
    WsStream = gun:ws_upgrade(GunPid, Path,[], #{silence_pings => false}),
    State#state{ws_stream = WsStream}.

connection_established(State = #state{handler = Handler}) ->
    State2 = schedule_ping_timeout(State#state{connected = true}),
    Handler ! {conn, self(), connected},
    State2.

connection_close(State = #state{gun_pid = GunPid, gun_ref = GunRef}) ->
    demonitor(GunRef),
    gun:shutdown(GunPid),
    State2 = cancel_ping_timeout(State),
    State3 = requests_error(State2, not_connected),
    State3#state{gun_pid = undefined, gun_ref = undefined,
                 ws_stream = undefined, connected = false}.

connection_closed(State, Reason) ->
    State2 = cancel_ping_timeout(State),
    State3 = requests_error(State2, Reason),
    State3#state{gun_pid = undefined, gun_ref = undefined,
                 ws_stream = undefined, connected = false}.

send_request(State, Method, Params, From, Ctx) ->
    {ReqRef, State2} = request_add(State, Method, From, Ctx),
    Msg = {request, format_method(Method), Params, ReqRef},
    send_messages(State2, Msg).

send_notification(State, Method, Params) ->
    Msg = {notification, format_method(Method), Params},
    send_messages(State, Msg).

send_response(State, Result, ReqRef) ->
    Msg = {result, Result, ReqRef},
    send_messages(State, Msg).

send_error(State, Code, Message, Data, ReqRef) ->
    {Code2, Message2} = encode_error(State, Code, Message),
    Msg = {error, Code2, Message2, Data, ReqRef},
    send_messages(State, Msg).

send_messages(State = #state{gun_pid = GunPid, ws_stream = Stream}, Messages) ->
-if(?ENABLE_TRACE =:= true).
    TraceList = if is_list(Messages) -> Messages; true -> [Messages] end,
    lists:foreach(fun(P) -> ?LOG_DEBUG("<<<<<<<<<< ~p", [P]) end, TraceList),
-endif.    
    Payload = grisp_connect_jsonrpc:encode(Messages),
    gun:ws_send(GunPid, Stream, {text, Payload}),
    State.

process_data(State, Data) ->
    Messages = grisp_connect_jsonrpc:decode(Data),
-if(?ENABLE_TRACE =:= true).
    lists:foreach(fun(P) -> ?LOG_DEBUG(">>>>>>>>>> ~p", [P]) end, Messages),
-endif.    
    process_messages(State, [], ).

process_messages(State, [], []) -> State;
process_messages(State, Errors, []) ->
    send_messages(State, lists:reverse(Errors));
process_messages(State, Errors, [{decoding_error, _, _, _, _} = Error | Rest]) ->
    process_messages(State, [Error | Errors], Rest);
process_messages(State, Errors, [{request, Method, Params, ReqRef} | Rest]) ->
    {State2, Errors2} = process_request(State, Errors, Method, Params, ReqRef),
    process_messages(State2, Errors2, Rest);
process_messages(State, Errors, [{notifications, Method, Params} | Rest]) ->
    {State2, Errors2} = process_notification(State, Errors, Method, Params),
    process_messages(State2, Errors2, Rest);
process_messages(State, Errors, [{result, Result, ReqRef} | Rest]) ->
    {State2, Errors2} = process_response(State, Errors, Result, ReqRef),
    process_messages(State2, Errors2, Rest);
process_messages(State, Errors, [{error, Code, Message, Data, ReqRef} | Rest]) ->
    {State2, Errors2} = process_error(State, Errors, Code, Message, Data, ReqRef),
    process_messages(State2, Errors2, Rest).

process_request(State = #state{handler = Handler},
                Errors, Method, Params, ReqRef) ->
    Handler ! {conn, self(), {request, parse_method(Method), Params, ReqRef}},
    {State, Errors}.

process_notification(State = #state{handler = Handler},
                     Errors, Method, Params) ->
    Handler ! {conn, self(), {notification, parse_method(Method), Params}},
    {State, Errors}.

process_response(State = #state{handler = Handler}, Errors, Result, ReqRef) ->
    case request_del(State, ReqRef) of
        {error, not_found} ->
            %FIXME: Not sure what error should be returned...
            Error = {invalid_request, -32600, <<"Result for unknown request">>},
            {State, [Error | Errors]};
        {ok, Method, undefined, Ctx, State2} ->
            Handler ! {conn, self(), {response, Method, Result, Ctx}},
            {State2, Errors};
        {ok, _, From, _, State2} ->
            gen_server:reply(From, {ok, Result}),
            {State2, Errors}
    end.

process_error(State = #state{handler = Handler},
              Errors, Code, Message, Data, undefined) ->
    {Code2, Message2} = decode_error(State, Code, Message),
    Handler ! {conn, self(), {error, Code2, Message2, Data}},
    {State, Errors};
process_error(State = #state{handler = Handler},
              Errors, Code, Message, Data, ReqRef) ->
    case request_del(State, ReqRef) of
        {error, not_found} ->
            %FIXME: Not sure what error should be returned...
            Error = {invalid_request, -32600, <<"Error for unknown request">>},
            {State, [Error | Errors]};
        {ok, Method, undefined, Ctx, State2} ->
            {Code2, Message2} = decode_error(State, Code, Message),
            Handler ! {conn, self(), {error, Method, Code2, Message2, Data, Ctx}},
            {State2, Errors};
        {ok, _, From, _, State2} ->
            {Code2, Message2} = decode_error(State, Code, Message),
            gen_server:reply(From, {error, Code2, Message2, Data}),
            {State2, Errors}
    end.

request_add(State = #state{request_timeout = Timeout, requests = ReqMap},
            Method, From, Ctx) ->
    ReqRef = make_reqref(),
    TRef = send_after(Timeout, {request_timeout, ReqRef}),
    Req = #req{id = ReqRef, method = Method, tref = TRef, from = From, ctx = Ctx},
    {ReqRef, State#state{requests = ReqMap#{ReqRef => Req}}}.

request_del(State = #state{requests = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> {error, not_found};
        {ok, #req{method = Method, tref = TRef, from = From, ctx = Ctx}} ->
            cancel_timer(TRef),
            State2 = State#state{requests = maps:remove(ReqRef, ReqMap)},
            {ok, Method, From, Ctx, State2}
    end.

request_timeout(State = #state{handler = Handler}, ReqRef) ->
    case request_del(State, ReqRef) of
        {error, not_found} ->
            ?LOG_WARNING("Timeout for unknown request ~s", [ReqRef]),
            State;
        {ok, Method, undefined, Ctx, State2} ->
            Handler ! {conn, self(), {timeout, Method, Ctx}},
            State2;
        {ok, _, From, _, State2} ->
            gen_server:reply(From, {error, timeout}),
            State2
    end.

requests_error(State = #state{handler = Handler, requests = ReqMap}, Reason) ->
    maps:foreach(fun
        (_, #req{method = Method, from = undefined, ctx = Ctx}) ->
            Handler ! {conn, self(), {error, Method, Reason, Ctx}};
        (_, #req{from = From, ctx = undefined}) ->
            gen_server:reply(From, {error, Reason})
    end, ReqMap),
    State#state{requests = #{}}.

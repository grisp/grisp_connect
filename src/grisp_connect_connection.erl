%% @doc JSONRpc 2.0 Websocket connection
-module(grisp_connect_connection).

-behaviour(gen_server).

-include("grisp_connect_internal.hrl").

-import(grisp_connect_utils, [as_bin/1]).
-import(grisp_connect_utils, [parse_method/1]).
-import(grisp_connect_utils, [format_method/1]).

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
-export([terminate/2]).


%--- Types ---------------------------------------------------------------------

-record(batch, {
    bref :: reference(),
    refcount :: pos_integer(),
    responses :: list()
}).

-record(inbound_req, {
    method :: method(),
    id :: binary() | integer(),
    bref :: undefined | reference() % Set if part of a batch
}).

-record(outbound_req, {
    method :: method(),
    id :: binary() | integer(),
    tref :: undefined | reference(),
    from :: undefined | gen_server:from(),
    ctx :: undefined | term()
}).

-record(state, {
    handler :: pid(),
    uri :: iodata(),
    domain :: binary(),
    port :: inet:port_number(),
    path :: binary(),
    ping_timeout :: infinity | pos_integer(),
    request_timeout :: infinity | pos_integer(),
    batches = #{} :: #{reference() => #batch{}},
    inbound = #{} :: #{binary() | integer() => #inbound_req{}},
    outbound = #{} :: #{binary() | integer() => #outbound_req{}},
    gun_pid :: undefined | pid(),
    gun_ref :: undefined | reference(),
    ws_stream :: undefined | gun:stream_ref(),
    connected = false :: boolean(),
    ping_tref :: undefined | reference()
}).

-type error_mapping() :: {atom(), integer(), binary()}.
-type method() :: atom() | binary() | [atom() | binary()].
-type start_options() :: #{
    domain := atom() | string() | binary(),
    port := inet:port_number(),
    %FIXME: dialyzer do no like ssl:tls_client_option(), maybe some erlang version issue
    transport := tcp | tls | {tls, TlsOpts :: list()},
    path := atom() | string() | binary(),
    errors => [error_mapping()],
    ping_timeout => infinity | pos_integer(),
    request_timeout => infinity | pos_integer()
}.

% Type specfication of the messages that are sent to the handler:
-type handler_messages() ::
    {conn, pid(), connected}
  | {conn, pid(), {request, method(), Params :: map() | list(), ReqRef :: binary() | integer()}}
  | {conn, pid(), {notification, method(), Params :: map() | list()}}
  | {conn, pid(), {response, Result :: term(), Ctx :: term()}}
  | {conn, pid(), {remote_error, Code :: integer() | atom(), Message :: undefined | binary(), Data :: term()}}
  | {conn, pid(), {remote_error, Code :: integer() | atom(), Message :: undefined | binary(), Data :: term(), Ctx :: term()}}
  | {conn, pid(), {local_error, Reason:: atom(), Ctx :: term()}}.

-export_type([error_mapping/0, method/0, handler_messages/0]).


%--- Macros --------------------------------------------------------------------

-define(ENABLE_TRACE, false).
-if(?ENABLE_TRACE =:= true).
-define(TRACE_OUTPUT(ARG), ?GRISP_DEBUG("<<<<<<<<<< ~s", [ARG])).
-define(TRACE_INPUT(ARG), ?GRISP_DEBUG(">>>>>>>>>> ~s", [ARG])).
-else.
-define(TRACE_OUTPUT(ARG), ok).
-define(TRACE_INPUT(ARG), ok).
-endif.

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
    gen_server:start_link(?MODULE, [Handler, Opts], []).

-spec request(Conn :: pid(), Method :: method(), Params :: map()) ->
    {ok, Result :: term()} | {error, timeout} | {error, not_connected}
    | {error, Code :: integer(),
              Message :: undefined | binary(), Data :: term()}.
request(Conn, Method, Params) ->
    case gen_server:call(Conn, {request, parse_method(Method), Params}) of
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack);
        Other -> Other
    end.

-spec post(Conn :: pid(), Method :: method(), Params :: map(),
           ReqCtx :: term()) -> ok | {error, not_connected}.
post(Conn, Method, Params, ReqCtx) ->
    case gen_server:call(Conn, {post, parse_method(Method), Params, ReqCtx}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec notify(Conn :: pid(), Method :: method(), Params :: map()) ->
    ok | {error, not_connected}.
notify(Conn, Method, Params) ->
    case gen_server:call(Conn, {notify, parse_method(Method), Params}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec reply(Conn :: pid(), Result :: any(), ReqRef :: binary()) ->
    ok | {error, not_connected}.
reply(Conn, Result, ReqRef) ->
    case gen_server:call(Conn, {reply, Result, ReqRef}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec error(Conn :: pid(), Code :: atom() | integer(),
            Message :: undefined | binary(), Data :: term(),
            ReqRef :: undefined | binary()) ->
    ok | {error, not_connected}.
error(Conn, Code, Message, Data, ReqRef) ->
    case gen_server:call(Conn, {error, Code, Message, Data, ReqRef}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    end.

-spec disconnect(Conn :: pid()) -> ok.
disconnect(Conn) ->
    gen_server:call(Conn, disconnect).


%--- Behaviour gen_server Callbacks --------------------------------------------

init([Handler, Opts]) ->
    process_flag(trap_exit, true), % To ensure terminate/2 is called
    #{domain := Domain, port := Port, path := Path} = Opts,
    PingTimeout = maps:get(ping_timeout, Opts, ?DEFAULT_PING_TIMEOUT),
    ReqTimeout = maps:get(request_timeout, Opts, ?DEFAULT_REQUEST_TIMEOUT),
    Transport = maps:get(transport, Opts, ?DEFAULT_TRANSPORT),
    State = #state{
        handler = Handler,
        uri = format_ws_uri(Transport, Domain, Port, Path),
        domain = as_bin(Domain),
        port = Port,
        path = as_bin(Path),
        ping_timeout = PingTimeout,
        request_timeout = ReqTimeout
    },
    index_errors(?DEFAULT_JSONRPC_ERRORS),
    index_errors(maps:get(errors, Opts, [])),
    case connection_start(State, Transport) of
        {ok, _State2} = Result -> Result;
        {error, Reason} -> {stop, Reason}
    end.

handle_call({request, Method, _Params}, _From,
            State = #state{connected = false}) ->
    ?GRISP_INFO("Request ~s performed while disconnected",
                [format_method(Method)],
                #{event => rpc_request_error, method => Method,
                  reason => not_connected}),
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
    ?GRISP_INFO("Request ~s posted while disconnected",
                [format_method(Method)],
                #{event => rpc_post_error, method => Method,
                  reason => not_connected}),
    % Notify the handler anyway so it doesn't have to make a special case
    Handler ! {conn, self(), {local_error, not_connected, ReqCtx}},
    {reply, {error, not_connected}, State};
handle_call({post, Method, Params, ReqCtx}, _From, State) ->
    try send_request(State, Method, Params, undefined, ReqCtx) of
        State2 -> {reply, {ok, ok}, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call({notify, Method, _Params}, _From,
            State = #state{connected = false}) ->
    ?GRISP_INFO("Notification ~s posted while disconnected",
                [format_method(Method)],
                #{event => rpc_notify_error, method => Method,
                  reason => not_connected}),
    {noreply, State};
handle_call({notify, Method, Params}, _From, State) ->
    try send_notification(State, Method, Params) of
        State2 -> {reply, {ok, ok}, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call({reply, _Result, ReqRef}, _From,
            State = #state{connected = false}) ->
    ?GRISP_INFO("Reply to ~s posted while disconnected",
                [inbound_req_tag(State, ReqRef)],
                #{event => rpc_reply_error, ref => ReqRef,
                  method => inbound_method(State, ReqRef),
                  reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({reply, Result, ReqRef}, _From, State) ->
    try send_response(State, Result, ReqRef) of
        State2 -> {reply, {ok, ok}, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call({error, Code, _Message, _Data, undefined}, _From,
            State = #state{connected = false}) ->
    ?GRISP_INFO("Error ~w posted while disconnected", [Code],
                #{event => rpc_error_error, code => Code,
                  reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({error, Code, _Message, _Data, ReqRef}, _From,
            State = #state{connected = false}) ->
    ?GRISP_INFO("Error ~w to ~s posted while disconnected",
                [Code, inbound_req_tag(State, ReqRef)],
                #{event => rpc_error_error, code => Code, ref => ReqRef,
                  reason => not_connected}),
    {reply, {error, not_connected}, State};
handle_call({error, Code, Message, Data, ReqRef}, _From, State) ->
    try send_error(State, Code, Message, Data, ReqRef) of
        State2 -> {reply, {ok, ok}, State2}
    catch
        C:badarg:S -> {reply, {exception, C, badarg, S}, State};
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call(disconnect, _From, State) ->
    try connection_close(State) of
        State2 -> {reply, {ok, ok}, State2}
    catch
        C:R:S -> {stop, R, {exception, C, R, S}, State}
    end;
handle_call(Call, From, State) ->
    ?GRISP_ERROR("Unexpected call from ~p to ~s: ~p", [From, ?MODULE, Call],
                 #{event => unexpected_call, from => From, message => Call}),
    {stop, {unexpected_call, Call}, {error, unexpected_call}, State}.

handle_cast(Cast, State) ->
    Reason = {unexpected_cast, Cast},
    ?GRISP_ERROR("Unexpected cast to ~s: ~p", [?MODULE, Cast],
                 #{event => unexpected_cast, message => Cast}),
    {stop, Reason, State}.

handle_info({gun_up, GunPid, _}, State = #state{gun_pid = GunPid}) ->
    ?GRISP_DEBUG("Connection to ~s established",
                 [State#state.uri],
                 #{event => ws_connection_enstablished, uri => State#state.uri}),
    {noreply, connection_upgrade(State)};
handle_info({gun_up, Pid, http} = _Msg, State = #state{gun_pid = GunPid}) ->
    ?GRISP_DEBUG("Ignoring unexpected gun_up message"
                 " from pid ~p, current pid is ~p", [Pid, GunPid],
                 #{event => unexpected_gun_message, message => _Msg}),
    {noreply, State};
handle_info({gun_upgrade, Pid, Stream, [<<"websocket">>], _},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_DEBUG("Connection to ~s upgraded to websocket", [State#state.uri],
                 #{event => ws_upgraded, uri => State#state.uri}),
    {noreply, connection_established(State)};
handle_info({gun_response, Pid, Stream, _, Status, _Headers},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_INFO("Connection to ~s failed to upgrade to websocket: ~p",
                 [State#state.uri, Status],
                 #{event => ws_upgrade_failed, uri => State#state.uri,
                   status => Status}),
    {stop, ws_upgrade_failed, connection_close(State)};
handle_info({gun_ws, Pid, Stream, ping},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    {noreply, schedule_ping_timeout(State)};
handle_info({gun_ws, Pid, Stream, {text, Text}},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    {noreply, process_data(State, Text)};
handle_info({gun_ws, Pid, Stream, close},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_INFO("Connection to ~s closed without code", [State#state.uri],
                #{event => ws_stream_closed, uri => State#state.uri}),
    {stop, normal, connection_closed(State, closed)};
handle_info({gun_ws, Pid, Stream, {close, Code, Message}},
            State = #state{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_INFO("Connection to ~s closed: ~s (~w)",
                [State#state.uri, Message, Code],
                #{event => ws_stream_closed, uri => State#state.uri,
                  code => Code, reason => Message}),
    {stop, normal, connection_closed(State, closed)};
handle_info({gun_error, Pid, _Stream, Reason},
            State = #state{gun_pid = Pid}) ->
    ?GRISP_INFO("Connection to ~s got an error: ~p",
                [State#state.uri, Reason],
                #{event => ws_error, uri => State#state.uri,
                  reason => Reason}),
    {stop, Reason, connection_close(State)};
handle_info({gun_down, Pid, ws, Reason, [Stream]},
            State = #state{gun_pid = Pid, ws_stream = Stream})
  when Reason =:= closed; Reason =:= {error, closed}; Reason =:= normal ->
    ?GRISP_INFO("Connection to ~s was closed by the server", [State#state.uri],
                #{event => ws_closed_by_peer, uri => State#state.uri,
                  reason => closed}),
    {stop, normal, connection_close(State)};
handle_info({'DOWN', _, process, Pid, Reason},
            State = #state{gun_pid = Pid}) ->
    ?GRISP_INFO("gun process of the connection to ~s crashed: ~p",
                [State#state.uri, Reason],
                #{event => ws_gun_crash, uri => State#state.uri,
                  reason => Reason}),
    {stop, Reason, connection_closed(State, gun_crashed)};
handle_info(ping_timeout, State) ->
    ?GRISP_INFO("Connection to ~s timed out", [State#state.uri],
                #{event => ws_ping_timeout, uri => State#state.uri}),
    {stop, normal, connection_close(State)};
handle_info({outbound_timeout, ReqRef}, State) ->
    ?GRISP_INFO("Request ~s time to ~s timed out",
                [outbound_req_tag(State, ReqRef), State#state.uri],
                #{event => rpc_request_timeout_error, uri => State#state.uri,
                  ref => ReqRef, method => outbound_method(State, ReqRef)}),
    {noreply, outbound_timeout(State, ReqRef)};
handle_info(Msg, State) ->
    ?GRISP_WARN("Unexpected info message to ~s: ~p",
                [?MODULE, Msg],
                #{event => unexpected_info, message => Msg}),
    {noreply, State}.

terminate(_Reason, State) ->
    connection_close(State),
    persistent_term:erase({?MODULE, self(), tags}),
    persistent_term:erase({?MODULE, self(), codes}),
    ok.


%--- INTERNAL FUNCTION ---------------------------------------------------------

format_ws_uri(Transport, Domain, Port, Path) ->
    Proto = case Transport of
        tcp -> <<"ws">>;
        tls -> <<"wss">>;
        {tls, _} -> <<"wss">>
    end,
    ?FORMAT("~s://~s:~w~s", [Proto, Domain, Port, Path]).

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

% Returns either the method of the outbound request or its reference
% as a binary if not found. Only mean for logging.
outbound_req_tag(#state{outbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> as_bin(ReqRef);
        {ok, #outbound_req{method = Method}} -> format_method(Method)
    end.

outbound_method(#state{outbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> undefined;
        {ok, #outbound_req{method = Method}} -> Method
    end.

% Returns either the method of the inbound request or its reference
% as a binary if not found. Only mean for logging.
inbound_req_tag(#state{inbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> as_bin(ReqRef);
        {ok, #inbound_req{method = Method}} -> format_method(Method)
    end.

inbound_method(#state{inbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> undefined;
        {ok, #inbound_req{method = Method}} -> Method
    end.

index_errors(ErrorSpecs) ->
    ErrorTags = persistent_term:get({?MODULE, self(), tags}, #{}),
    ErrorCodes = persistent_term:get({?MODULE, self(), codes}, #{}),
    {ErrorTags2, ErrorCodes2} =
        lists:foldl(fun({Tag, Code, Msg}, {Tags, Codes}) ->
            {Tags#{Tag => {Code, Msg}}, Codes#{Code => {Tag, Msg}}}
        end, {ErrorTags, ErrorCodes}, ErrorSpecs),
    % The error list is put in a persistent term to not add noise to the state.
    persistent_term:put({?MODULE, self(), tags}, ErrorTags2),
    persistent_term:put({?MODULE, self(), codes}, ErrorCodes2),
    ok.

decode_error(Code, Message)
  when is_integer(Code), Message =:= undefined ->
    ErrorCodes = persistent_term:get({?MODULE, self(), codes}, #{}),
    case maps:find(Code, ErrorCodes) of
        error -> {Code, undefined};
        {ok, {Tag, DefaultMessage}} -> {Tag, DefaultMessage}
    end;
decode_error(Code, Message)
  when is_integer(Code) ->
    ErrorCodes = persistent_term:get({?MODULE, self(), codes}, #{}),
    case maps:find(Code, ErrorCodes) of
        error -> {Code, Message};
        {ok, {Tag, _DefaultMessage}} -> {Tag, Message}
    end.

encode_error(Code, Message)
  when is_integer(Code), Message =:= undefined ->
    ErrorCodes = persistent_term:get({?MODULE, self(), codes}, #{}),
    case maps:find(Code, ErrorCodes) of
        error -> {Code, null};
        {ok, {_Tag, DefaultMessage}} -> {Code, DefaultMessage}
    end;
encode_error(Code, Message)
  when is_integer(Code) ->
    {Code, Message};
encode_error(Tag, Message)
  when is_atom(Tag), Message =:= undefined ->
    ErrorTags = persistent_term:get({?MODULE, self(), tags}, #{}),
    case maps:find(Tag, ErrorTags) of
        error -> erlang:error(badarg);
        {ok, {Code, DefaultMessage}} -> {Code, DefaultMessage}
    end;
encode_error(Tag, Message)
  when is_atom(Tag) ->
    ErrorTags = persistent_term:get({?MODULE, self(), tags}, #{}),
    case maps:find(Tag, ErrorTags) of
        error -> erlang:error(badarg);
        {ok, {Code, _DefaultMessage}} -> {Code, Message}
    end.

connection_start(State = #state{uri = Uri, domain = Domain, port = Port},
                 TransportSpec) ->
    BaseGunOpts = #{protocols => [http], retry => 0},
    GunOpts = case TransportSpec of
        tcp -> BaseGunOpts#{transport => tcp};
        tls -> BaseGunOpts#{transport => tls};
        {tls, Opts} -> BaseGunOpts#{transport => tls, tls_opts => Opts}
    end,
    ?GRISP_DEBUG("Connecting to ~s", [Uri],
                 #{event => connecting, uri => Uri, options => GunOpts}),
    case gun:open(binary_to_list(Domain), Port, GunOpts) of
        {ok, GunPid} ->
            GunRef = monitor(process, GunPid),
            {ok, State#state{gun_pid = GunPid, gun_ref = GunRef}};
        {error, Reason} ->
            ?GRISP_ERROR("Failed to open connection to ~s: ~p", [Uri, Reason],
                         #{event => connection_failure, uri => Uri,
                           reason => Reason}),
            {error, Reason}
    end.

connection_upgrade(State = #state{path = Path, gun_pid = GunPid}) ->
    WsStream = gun:ws_upgrade(GunPid, Path,[], #{silence_pings => false}),
    State#state{ws_stream = WsStream}.

connection_established(State = #state{handler = Handler}) ->
    State2 = schedule_ping_timeout(State#state{connected = true}),
    Handler ! {conn, self(), connected},
    State2.

connection_close(State = #state{gun_pid = GunPid, gun_ref = GunRef})
  when GunPid =/= undefined, GunRef =/= undefined  ->
    demonitor(GunRef),
    gun:shutdown(GunPid),
    State2 = cancel_ping_timeout(State),
    State3 = requests_error(State2, not_connected),
    State3#state{gun_pid = undefined, gun_ref = undefined,
                 ws_stream = undefined, connected = false};
connection_close(State) ->
    State2 = requests_error(State, not_connected),
    State2#state{connected = false}.

connection_closed(State, Reason) ->
    State2 = cancel_ping_timeout(State),
    State3 = requests_error(State2, Reason),
    State3#state{gun_pid = undefined, gun_ref = undefined,
                 ws_stream = undefined, connected = false}.

send_request(State, Method, Params, From, Ctx) ->
    {ReqRef, State2} = outbound_add(State, Method, From, Ctx),
    Msg = {request, format_method(Method), Params, ReqRef},
    send_packet(State2, Msg).

send_notification(State, Method, Params) ->
    Msg = {notification, format_method(Method), Params},
    send_packet(State, Msg).

send_response(State, Result, ReqRef) ->
    Msg = {result, Result, ReqRef},
    inbound_response(State, Msg, ReqRef).

send_error(State, Code, Message, Data, ReqRef) ->
    {Code2, Message2} = encode_error(Code, Message),
    Msg = {error, Code2, Message2, Data, ReqRef},
    case ReqRef of
        undefined -> send_packet(State, Msg);
        _ -> inbound_response(State, Msg, ReqRef)
    end.

send_packet(State = #state{gun_pid = GunPid, ws_stream = Stream}, Packet) ->
    Payload = grisp_connect_jsonrpc:encode(Packet),
    ?TRACE_OUTPUT(Payload),
    gun:ws_send(GunPid, Stream, {text, Payload}),
    State.

process_data(State = #state{batches = BatchMap}, Data) ->
    ?TRACE_INPUT(Data),
    DecodedData = grisp_connect_jsonrpc:decode(Data),
    case DecodedData of
        Messages when is_list(Messages) ->
            BatchRef = make_ref(),
            case process_messages(State, BatchRef, 0, [], Messages) of
                {ReqCount, Replies, State2} when ReqCount > 0 ->
                    Batch = #batch{bref = BatchRef, refcount = ReqCount,
                                   responses = Replies},
                    State2#state{batches = BatchMap#{BatchRef => Batch}};
                {_ReqCount, [], State2} ->
                    State2;
                {_ReqCount, [_|_] = Replies, State2} ->
                    % All the requests got a reply right away
                    send_packet(State2, Replies)
            end;
        Message when is_tuple(Message) ->
            case process_messages(State, undefined, 0, [], [Message]) of
                {_, [Reply], State2} ->
                    send_packet(State2, Reply);
                {_, [], State2} ->
                    State2
            end
    end.

process_messages(State, _BatchRef, ReqCount, Replies, []) ->
    {ReqCount, lists:reverse(Replies), State};
process_messages(State, BatchRef, ReqCount, Replies,
                 [{decoding_error, _, _, _, _} = Error | Rest]) ->
    process_messages(State, BatchRef, ReqCount, [Error | Replies], Rest);
process_messages(State, BatchRef, ReqCount, Replies,
                 [{request, RawMethod, Params, ReqRef} | Rest]) ->
    Method = parse_method(RawMethod),
    State2 = process_request(State, BatchRef, Method, Params, ReqRef),
    process_messages(State2, BatchRef, ReqCount + 1, Replies, Rest);
process_messages(State, BatchRef, ReqCount, Replies,
                 [{notification, RawMethod, Params} | Rest]) ->
    Method = parse_method(RawMethod),
    State2 = process_notification(State, Method, Params),
    process_messages(State2, BatchRef, ReqCount, Replies, Rest);
process_messages(State, BatchRef, ReqCount, Replies,
                 [{result, Result, ReqRef} | Rest]) ->
    {State2, Replies2} = process_response(State, Replies, Result, ReqRef),
    process_messages(State2, BatchRef, ReqCount, Replies2, Rest);
process_messages(State, BatchRef, ReqCount, Replies,
                 [{error, Code, Message, Data, ReqRef} | Rest]) ->
    {State2, Replies2} = process_error(State, Replies, Code,
                                       Message, Data, ReqRef),
    process_messages(State2, BatchRef, ReqCount, Replies2, Rest).

process_request(State = #state{handler = Handler},
                BatchRef, Method, Params, ReqRef) ->
    Handler ! {conn, self(), {request, Method, Params, ReqRef}},
    inbound_add(State, BatchRef, Method, ReqRef).

process_notification(State = #state{handler = Handler}, Method, Params) ->
    Handler ! {conn, self(), {notification, Method, Params}},
    State.

process_response(State = #state{handler = Handler}, Replies, Result, ReqRef) ->
    case outbound_del(State, ReqRef) of
        {error, not_found} ->
            %FIXME: Not sure what error should be returned...
            Error = {invalid_request, -32600, <<"Result for unknown request">>},
            {State, [Error | Replies]};
        {ok, _Method, undefined, Ctx, State2} ->
            Handler ! {conn, self(), {response, Result, Ctx}},
            {State2, Replies};
        {ok, _, From, _, State2} ->
            gen_server:reply(From, {ok, Result}),
            {State2, Replies}
    end.

process_error(State = #state{handler = Handler},
              Replies, Code, Message, Data, undefined) ->
    {Code2, Message2} = decode_error(Code, Message),
    Handler ! {conn, self(), {remote_error, Code2, Message2, Data}},
    {State, Replies};
process_error(State = #state{handler = Handler},
              Replies, Code, Message, Data, ReqRef) ->
    case outbound_del(State, ReqRef) of
        {error, not_found} ->
            %FIXME: Not sure what error should be returned...
            Error = {invalid_request, -32600, <<"Error for unknown request">>},
            {State, [Error | Replies]};
        {ok, _Method, undefined, Ctx, State2} ->
            {Code2, Message2} = decode_error(Code, Message),
            Handler ! {conn, self(), {remote_error, Code2, Message2, Data, Ctx}},
            {State2, Replies};
        {ok, _, From, _, State2} ->
            {Code2, Message2} = decode_error(Code, Message),
            gen_server:reply(From, {remote_error, Code2, Message2, Data}),
            {State2, Replies}
    end.

inbound_response(State = #state{batches = BatchMap, inbound = ReqMap},
                 Message, ReqRef) ->
    case maps:take(ReqRef, ReqMap) of
        error ->
            ?GRISP_ERROR("Ask to send a response to the unknown request ~p",
                         [ReqRef],
                         #{event => internal_error, ref => ReqRef,
                           reason => unknown_request}),
            State;
        {#inbound_req{bref = undefined}, ReqMap2} ->
            % Not part of a batch response
            send_packet(State#state{inbound = ReqMap2}, Message);
        {#inbound_req{bref = BatchRef}, ReqMap2} ->
            % The batch must exists
            case maps:find(BatchRef, BatchMap) of
                {ok, #batch{refcount = 1, responses = Responses}} ->
                    % This is the last message of the batch
                    BatchMap2 = maps:remove(BatchRef, BatchMap),
                    send_packet(State#state{batches = BatchMap2,
                                              inbound = ReqMap2},
                                  [Message | Responses]);
                {ok, Batch = #batch{refcount = RefCount,
                                    responses = Responses}} ->
                    Batch2 = Batch#batch{refcount = RefCount - 1,
                                         responses = [Message | Responses]},
                    BatchMap2 = BatchMap#{BatchRef => Batch2},
                    State#state{batches = BatchMap2, inbound = ReqMap2}
            end
    end.

inbound_add(State = #state{inbound = ReqMap}, BatchRef, Method, ReqRef) ->
    %TODO: Should we add a timeout for inbound requests ?
    Req = #inbound_req{method = Method, id = ReqRef, bref = BatchRef},
    State#state{inbound = ReqMap#{ReqRef => Req}}.

outbound_add(State = #state{request_timeout = Timeout, outbound = ReqMap},
            Method, From, Ctx) ->
    ReqRef = make_reqref(),
    TRef = send_after(Timeout, {outbound_timeout, ReqRef}),
    Req = #outbound_req{id = ReqRef, method = Method, tref = TRef,
                       from = From, ctx = Ctx},
    {ReqRef, State#state{outbound = ReqMap#{ReqRef => Req}}}.

outbound_del(State = #state{outbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> {error, not_found};
        {ok, #outbound_req{method = Method, tref = TRef,
                          from = From, ctx = Ctx}} ->
            cancel_timer(TRef),
            State2 = State#state{outbound = maps:remove(ReqRef, ReqMap)},
            {ok, Method, From, Ctx, State2}
    end.

outbound_timeout(State = #state{handler = Handler}, ReqRef) ->
    case outbound_del(State, ReqRef) of
        {error, not_found} ->
            ?GRISP_WARN("Timeout for unknown request ~s", [ReqRef],
                        #{event => internal_error, ref => ReqRef,
                          reason => unknown_request}),
            State;
        {ok, _Method, undefined, Ctx, State2} ->
            Handler ! {conn, self(), {local_error, timeout, Ctx}},
            State2;
        {ok, _, From, _, State2} ->
            gen_server:reply(From, {error, timeout}),
            State2
    end.

requests_error(State = #state{handler = Handler, outbound = ReqMap}, Reason) ->
    maps:foreach(fun
        (_, #outbound_req{from = undefined, ctx = Ctx}) ->
            Handler ! {conn, self(), {local_error, Reason, Ctx}};
        (_, #outbound_req{from = From, ctx = undefined}) ->
            gen_server:reply(From, {error, Reason})
    end, ReqMap),
    State#state{outbound = #{}}.

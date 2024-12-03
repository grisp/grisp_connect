%% @doc JSONRpc 2.0 Websocket connection
-module(grisp_connect_connection).

-behaviour(gen_statem).

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

% Behaviour gen_statem callback functions
-export([callback_mode/0]).
-export([init/1]).
-export([code_change/4]).
-export([terminate/3]).

% Data Functions
-export([connecting/3]).
-export([connected/3]).


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
    from :: undefined | gen_statem:from(),
    ctx :: undefined | term()
}).

-record(data, {
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
  | {conn, pid(), {remote_error, Code :: integer() | atom(), Message :: undefined | binary(), ErData :: term()}}
  | {conn, pid(), {remote_error, Code :: integer() | atom(), Message :: undefined | binary(), ErData :: term(), Ctx :: term()}}
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

-define(HANDLE_COMMON,
    ?FUNCTION_NAME(EventType, EventContent, Data) ->
        handle_common(EventType, EventContent, ?FUNCTION_NAME, Data)).


%--- API Functions -------------------------------------------------------------

-spec start_link(Handler :: pid(), Options :: start_options()) ->
    {ok, Conn :: pid()} | {error, Reason :: term()}.
start_link(Handler, Opts = #{domain := _, port := _, path := _}) ->
    gen_statem:start_link(?MODULE, [Handler, Opts], []).

-spec request(Conn :: pid(), Method :: method(), Params :: map()) ->
    {ok, Result :: term()} | {error, timeout} | {error, not_connected}
    | {error, Code :: integer(),
              Message :: undefined | binary(), ErData :: term()}.
request(Conn, Method, Params) ->
    try gen_statem:call(Conn, {request, parse_method(Method), Params}) of
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack);
        Other -> Other
    catch
        exit:{noproc, _} -> {error, not_connected}
    end.

-spec post(Conn :: pid(), Method :: method(), Params :: map(),
           ReqCtx :: term()) -> ok | {error, not_connected}.
post(Conn, Method, Params, ReqCtx) ->
    try gen_statem:call(Conn, {post, parse_method(Method), Params, ReqCtx}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    catch
        exit:{noproc, _} -> {error, not_connected}
    end.

-spec notify(Conn :: pid(), Method :: method(), Params :: map()) ->
    ok | {error, not_connected}.
notify(Conn, Method, Params) ->
    try gen_statem:call(Conn, {notify, parse_method(Method), Params}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    catch
        exit:{noproc, _} -> {error, not_connected}
    end.

-spec reply(Conn :: pid(), Result :: any(), ReqRef :: binary()) ->
    ok | {error, not_connected}.
reply(Conn, Result, ReqRef) ->
    try gen_statem:call(Conn, {reply, Result, ReqRef}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    catch
        exit:{noproc, _} -> {error, not_connected}
    end.

-spec error(Conn :: pid(), Code :: atom() | integer(),
            Message :: undefined | binary(), ErData :: term(),
            ReqRef :: undefined | binary()) ->
    ok | {error, not_connected}.
error(Conn, Code, Message, ErData, ReqRef) ->
    try gen_statem:call(Conn, {error, Code, Message, ErData, ReqRef}) of
        {ok, CallResult} -> CallResult;
        {error, _Reason} = Error -> Error;
        {exception, Class, Reason, Stack} -> erlang:raise(Class, Reason, Stack)
    catch
        exit:{noproc, _} -> {error, not_connected}
    end.

-spec disconnect(Conn :: pid()) -> ok.
disconnect(Conn) ->
    try gen_statem:call(Conn, disconnect)
    catch
        exit:{noproc, _} -> ok
    end.


%--- Behaviour gen_statem Callbacks --------------------------------------------

callback_mode() -> [state_functions].

init([Handler, Opts]) ->
    process_flag(trap_exit, true), % To ensure terminate/2 is called
    #{domain := Domain, port := Port, path := Path} = Opts,
    PingTimeout = maps:get(ping_timeout, Opts, ?DEFAULT_PING_TIMEOUT),
    ReqTimeout = maps:get(request_timeout, Opts, ?DEFAULT_REQUEST_TIMEOUT),
    Transport = maps:get(transport, Opts, ?DEFAULT_TRANSPORT),
    Data = #data{
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
    case connection_start(Data, Transport) of
        {ok, Data2} -> {ok, connecting, Data2};
        {error, Reason} -> {stop, Reason}
    end.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, Data) ->
    connection_close(Data),
    persistent_term:erase({?MODULE, self(), tags}),
    persistent_term:erase({?MODULE, self(), codes}),
    ok.


%--- Behaviour gen_statem State Callback Functions -----------------------------

connecting({call, From}, {request, Method, _Params}, _Data) ->
    ?GRISP_INFO("Request ~s performed while connecting",
                [format_method(Method)],
                #{event => rpc_request_error, method => Method,
                  reason => not_connected}),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connecting({call, From}, {post, Method, _Params, ReqCtx},
            #data{handler = Handler}) ->
    ?GRISP_INFO("Request ~s posted while connecting",
                [format_method(Method)],
                #{event => rpc_post_error, method => Method,
                  reason => not_connected}),
    % Notify the handler anyway so it doesn't have to make a special case
    Handler ! {conn, self(), {local_error, not_connected, ReqCtx}},
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connecting({call, From}, {notify, Method, _Params}, _Data) ->
    ?GRISP_INFO("Notification ~s posted while connecting",
                [format_method(Method)],
                #{event => rpc_notify_error, method => Method,
                  reason => not_connected}),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connecting({call, From}, {reply, _Result, ReqRef}, Data) ->
    ?GRISP_INFO("Reply to ~s posted while connecting",
                [inbound_req_tag(Data, ReqRef)],
                #{event => rpc_reply_error, ref => ReqRef,
                  method => inbound_method(Data, ReqRef),
                  reason => not_connected}),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connecting({call, From}, {error, Code, _Message, _ErData, undefined}, _Data) ->
    ?GRISP_INFO("Error ~w posted while connecting", [Code],
                #{event => rpc_error_error, code => Code,
                  reason => not_connected}),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connecting({call, From}, {error, Code, _Message, _ErData, ReqRef}, Data) ->
    ?GRISP_INFO("Error ~w to ~s posted while connecting",
                [Code, inbound_req_tag(Data, ReqRef)],
                #{event => rpc_error_error, code => Code, ref => ReqRef,
                  reason => not_connected}),
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
connecting(info, {gun_up, GunPid, _}, Data = #data{gun_pid = GunPid}) ->
    ?GRISP_DEBUG("Connection to ~s established",
                 [Data#data.uri],
                 #{event => ws_connection_enstablished, uri => Data#data.uri}),
    {keep_state, connection_upgrade(Data)};
connecting(info, {gun_upgrade, Pid, Stream, [<<"websocket">>], _},
           Data = #data{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_DEBUG("Connection to ~s upgraded to websocket", [Data#data.uri],
                 #{event => ws_upgraded, uri => Data#data.uri}),
    {next_state, connected, connection_established(Data)};

connecting(info, {gun_response, Pid, Stream, _, Status, _Headers},
           Data = #data{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_INFO("Connection to ~s failed to upgrade to websocket: ~p",
                 [Data#data.uri, Status],
                 #{event => ws_upgrade_failed, uri => Data#data.uri,
                   status => Status}),
    {stop, ws_upgrade_failed, connection_close(Data)};
?HANDLE_COMMON.

connected({call, From}, {request, Method, Params}, Data) ->
    try send_request(Data, Method, Params, From, undefined) of
        Data2 -> {keep_state, Data2}
    catch
        C:badarg:S ->
            {keep_state_and_data, [{reply, From, {exception, C, badarg, S}}]};
        C:R:S ->
            {stop_and_reply, R, [{reply, From, {exception, C, R, S}}]}
    end;
connected({call, From}, {post, Method, Params, ReqCtx}, Data) ->
    try send_request(Data, Method, Params, undefined, ReqCtx) of
        Data2 -> {keep_state, Data2, [{reply, From, {ok, ok}}]}
    catch
        C:badarg:S ->
            {keep_state_and_data, [{reply, From, {exception, C, badarg, S}}]};
        C:R:S ->
            {stop_and_reply, R, [{reply, From, {exception, C, R, S}}]}
    end;
connected({call, From}, {notify, Method, Params}, Data) ->
    try send_notification(Data, Method, Params) of
        Data2 -> {keep_state, Data2, [{reply, From, {ok, ok}}]}
    catch
        C:badarg:S ->
            {keep_state_and_data, [{reply, From, {exception, C, badarg, S}}]};
        C:R:S ->
            {stop_and_reply, R, [{reply, From, {exception, C, R, S}}]}
    end;
connected({call, From}, {reply, Result, ReqRef}, Data) ->
    try send_response(Data, Result, ReqRef) of
        Data2 -> {keep_state, Data2, [{reply, From, {ok, ok}}]}
    catch
        C:badarg:S ->
            {keep_state_and_data, [{reply, From, {exception, C, badarg, S}}]};
        C:R:S ->
            {stop_and_reply, R, [{reply, From, {exception, C, R, S}}]}
    end;
connected({call, From}, {error, Code, Message, ErData, ReqRef}, Data) ->
    try send_error(Data, Code, Message, ErData, ReqRef) of
        Data2 -> {keep_state, Data2, [{reply, From, {ok, ok}}]}
    catch
        C:badarg:S ->
            {keep_state_and_data, [{reply, From, {exception, C, badarg, S}}]};
        C:R:S ->
            {stop_and_reply, R, [{reply, From, {exception, C, R, S}}]}
    end;
connected(info, {gun_ws, Pid, Stream, {text, Text}},
          Data = #data{gun_pid = Pid, ws_stream = Stream}) ->
    {keep_state, process_text(Data, Text)};
connected(info, ping_timeout, Data) ->
    ?GRISP_INFO("Connection to ~s timed out", [Data#data.uri],
                #{event => ws_ping_timeout, uri => Data#data.uri}),
    {stop, normal, connection_close(Data)};
connected(info, {outbound_timeout, ReqRef}, Data) ->
    ?GRISP_INFO("Request ~s time to ~s timed out",
                [outbound_req_tag(Data, ReqRef), Data#data.uri],
                #{event => rpc_request_timeout_error, uri => Data#data.uri,
                  ref => ReqRef, method => outbound_method(Data, ReqRef)}),
    {keep_state, outbound_timeout(Data, ReqRef)};
?HANDLE_COMMON.

handle_common({call, From}, disconnect, _StateName, Data) ->
    try connection_close(Data) of
        Data2 -> {keep_state, Data2, [{reply, From, {ok, ok}}]}
    catch
        C:R:S -> {stop_and_reply, R, [{reply, From, {exception, C, R, S}}]}
    end;
handle_common(info, {gun_ws, Pid, Stream, ping}, _StateName,
              Data = #data{gun_pid = Pid, ws_stream = Stream}) ->
    {keep_state, schedule_ping_timeout(Data)};
handle_common(info, {gun_ws, Pid, Stream, close}, StateName,
              Data = #data{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_INFO("Connection to ~s closed without code", [Data#data.uri],
                #{event => ws_stream_closed, uri => Data#data.uri,
                  state => StateName}),
    {stop, normal, connection_closed(Data, closed)};
handle_common(info, {gun_ws, Pid, Stream, {close, Code, Message}}, StateName,
              Data = #data{gun_pid = Pid, ws_stream = Stream}) ->
    ?GRISP_INFO("Connection to ~s closed: ~s (~w)",
                [Data#data.uri, Message, Code],
                #{event => ws_stream_closed, uri => Data#data.uri,
                  code => Code, reason => Message, state => StateName}),
    {stop, normal, connection_closed(Data, closed)};
handle_common(info, {gun_error, Pid, _Stream, Reason}, StateName,
              Data = #data{gun_pid = Pid}) ->
    ?GRISP_INFO("Connection to ~s got an error: ~p",
                [Data#data.uri, Reason],
                #{event => ws_error, uri => Data#data.uri,
                  reason => Reason, state => StateName}),
    {stop, Reason, connection_close(Data)};
handle_common(info, {gun_down, Pid, ws, Reason, [Stream]}, StateName,
              Data = #data{gun_pid = Pid, ws_stream = Stream})
  when Reason =:= closed; Reason =:= {error, closed}; Reason =:= normal ->
    ?GRISP_INFO("Connection to ~s was closed by the server", [Data#data.uri],
                #{event => ws_closed_by_peer, uri => Data#data.uri,
                  reason => closed, state => StateName}),
    {stop, normal, connection_close(Data)};
handle_common(info, {'DOWN', _, process, Pid, Reason}, StateName,
              Data = #data{gun_pid = Pid}) ->
    ?GRISP_INFO("gun process of the connection to ~s crashed: ~p",
                [Data#data.uri, Reason],
                #{event => ws_gun_crash, uri => Data#data.uri,
                  reason => Reason, state => StateName}),
    {stop, Reason, connection_closed(Data, gun_crashed)};
handle_common(info, {gun_up, Pid, http} = Info, StateName,
              #data{gun_pid = GunPid}) ->
    ?GRISP_DEBUG("Ignoring unexpected gun_up message"
                 " from pid ~p, current pid is ~p", [Pid, GunPid],
                 #{event => unexpected_gun_message, message => Info,
                   state => StateName}),
    keep_state_and_data;
handle_common({call, From}, Call, StateName, _Data) ->
    ?GRISP_ERROR("Unexpected call from ~p to ~s: ~p", [From, ?MODULE, Call],
                 #{event => unexpected_call, from => From, message => Call,
                   state => StateName}),
    {stop_and_reply, {unexpected_call, Call},
     [{reply, From, {error, unexpected_call}}]};
handle_common(cast, Cast, StateName, _Data) ->
    Reason = {unexpected_cast, Cast},
    ?GRISP_ERROR("Unexpected cast to ~s: ~p", [?MODULE, Cast],
                 #{event => unexpected_cast, message => Cast,
                   state => StateName}),
    {stop, Reason};
handle_common(info, Info, StateName, _Data) ->
    ?GRISP_WARN("Unexpected info message to ~s: ~p",
                [?MODULE, Info],
                #{event => unexpected_info, message => Info,
                  state => StateName}),
    keep_state_and_data.


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

schedule_ping_timeout(Data = #data{ping_timeout = Timeout}) ->
    Data2 = cancel_ping_timeout(Data),
    TRef = send_after(Timeout, ping_timeout),
    Data2#data{ping_tref = TRef}.

cancel_ping_timeout(Data = #data{ping_tref = undefined}) ->
    Data;
cancel_ping_timeout(Data = #data{ping_tref = TRef}) ->
    cancel_timer(TRef),
    Data#data{ping_tref = undefined}.

% Returns either the method of the outbound request or its reference
% as a binary if not found. Only mean for logging.
outbound_req_tag(#data{outbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> as_bin(ReqRef);
        {ok, #outbound_req{method = Method}} -> format_method(Method)
    end.

outbound_method(#data{outbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> undefined;
        {ok, #outbound_req{method = Method}} -> Method
    end.

% Returns either the method of the inbound request or its reference
% as a binary if not found. Only mean for logging.
inbound_req_tag(#data{inbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> as_bin(ReqRef);
        {ok, #inbound_req{method = Method}} -> format_method(Method)
    end.

inbound_method(#data{inbound = ReqMap}, ReqRef) ->
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
    % The error list is put in a persistent term to not add noise to the Data.
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

connection_start(Data = #data{uri = Uri, domain = Domain, port = Port},
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
            {ok, Data#data{gun_pid = GunPid, gun_ref = GunRef}};
        {error, Reason} ->
            ?GRISP_ERROR("Failed to open connection to ~s: ~p", [Uri, Reason],
                         #{event => connection_failure, uri => Uri,
                           reason => Reason}),
            {error, Reason}
    end.

connection_upgrade(Data = #data{path = Path, gun_pid = GunPid}) ->
    WsStream = gun:ws_upgrade(GunPid, Path,[], #{silence_pings => false}),
    Data#data{ws_stream = WsStream}.

connection_established(Data = #data{handler = Handler}) ->
    Handler ! {conn, self(), connected},
    schedule_ping_timeout(Data).

connection_close(Data = #data{gun_pid = GunPid, gun_ref = GunRef})
  when GunPid =/= undefined, GunRef =/= undefined  ->
    demonitor(GunRef),
    gun:shutdown(GunPid),
    Data2 = cancel_ping_timeout(Data),
    Data3 = requests_error(Data2, not_connected),
    Data3#data{gun_pid = undefined, gun_ref = undefined, ws_stream = undefined};
connection_close(Data) ->
    requests_error(Data, not_connected).

connection_closed(Data, Reason) ->
    Data2 = cancel_ping_timeout(Data),
    Data3 = requests_error(Data2, Reason),
    Data3#data{gun_pid = undefined, gun_ref = undefined, ws_stream = undefined}.

send_request(Data, Method, Params, From, Ctx) ->
    {ReqRef, Data2} = outbound_add(Data, Method, From, Ctx),
    Msg = {request, format_method(Method), Params, ReqRef},
    send_packet(Data2, Msg).

send_notification(Data, Method, Params) ->
    Msg = {notification, format_method(Method), Params},
    send_packet(Data, Msg).

send_response(Data, Result, ReqRef) ->
    Msg = {result, Result, ReqRef},
    inbound_response(Data, Msg, ReqRef).

send_error(Data, Code, Message, ErData, ReqRef) ->
    {Code2, Message2} = encode_error(Code, Message),
    Msg = {error, Code2, Message2, ErData, ReqRef},
    case ReqRef of
        undefined -> send_packet(Data, Msg);
        _ -> inbound_response(Data, Msg, ReqRef)
    end.

send_packet(Data = #data{gun_pid = GunPid, ws_stream = Stream}, Packet) ->
    Payload = grisp_connect_jsonrpc:encode(Packet),
    ?TRACE_OUTPUT(Payload),
    gun:ws_send(GunPid, Stream, {text, Payload}),
    Data.

process_text(Data = #data{batches = BatchMap}, Text) ->
    ?TRACE_INPUT(Data),
    DecodedData = grisp_connect_jsonrpc:decode(Text),
    case DecodedData of
        Messages when is_list(Messages) ->
            BatchRef = make_ref(),
            case process_messages(Data, BatchRef, 0, [], Messages) of
                {ReqCount, Replies, Data2} when ReqCount > 0 ->
                    Batch = #batch{bref = BatchRef, refcount = ReqCount,
                                   responses = Replies},
                    Data2#data{batches = BatchMap#{BatchRef => Batch}};
                {_ReqCount, [], Data2} ->
                    Data2;
                {_ReqCount, [_|_] = Replies, Data2} ->
                    % All the requests got a reply right away
                    send_packet(Data2, Replies)
            end;
        Message when is_tuple(Message) ->
            case process_messages(Data, undefined, 0, [], [Message]) of
                {_, [Reply], Data2} ->
                    send_packet(Data2, Reply);
                {_, [], Data2} ->
                    Data2
            end
    end.

process_messages(Data, _BatchRef, ReqCount, Replies, []) ->
    {ReqCount, lists:reverse(Replies), Data};
process_messages(Data, BatchRef, ReqCount, Replies,
                 [{decoding_error, _, _, _, _} = Error | Rest]) ->
    process_messages(Data, BatchRef, ReqCount, [Error | Replies], Rest);
process_messages(Data, BatchRef, ReqCount, Replies,
                 [{request, RawMethod, Params, ReqRef} | Rest]) ->
    Method = parse_method(RawMethod),
    Data2 = process_request(Data, BatchRef, Method, Params, ReqRef),
    process_messages(Data2, BatchRef, ReqCount + 1, Replies, Rest);
process_messages(Data, BatchRef, ReqCount, Replies,
                 [{notification, RawMethod, Params} | Rest]) ->
    Method = parse_method(RawMethod),
    Data2 = process_notification(Data, Method, Params),
    process_messages(Data2, BatchRef, ReqCount, Replies, Rest);
process_messages(Data, BatchRef, ReqCount, Replies,
                 [{result, Result, ReqRef} | Rest]) ->
    {Data2, Replies2} = process_response(Data, Replies, Result, ReqRef),
    process_messages(Data2, BatchRef, ReqCount, Replies2, Rest);
process_messages(Data, BatchRef, ReqCount, Replies,
                 [{error, Code, Message, ErData, ReqRef} | Rest]) ->
    {Data2, Replies2} = process_error(Data, Replies, Code,
                                       Message, ErData, ReqRef),
    process_messages(Data2, BatchRef, ReqCount, Replies2, Rest).

process_request(Data = #data{handler = Handler},
                BatchRef, Method, Params, ReqRef) ->
    Handler ! {conn, self(), {request, Method, Params, ReqRef}},
    inbound_add(Data, BatchRef, Method, ReqRef).

process_notification(Data = #data{handler = Handler}, Method, Params) ->
    Handler ! {conn, self(), {notification, Method, Params}},
    Data.

process_response(Data = #data{handler = Handler}, Replies, Result, ReqRef) ->
    case outbound_del(Data, ReqRef) of
        {error, not_found} ->
            %FIXME: Not sure what error should be returned...
            Error = {invalid_request, -32600, <<"Result for unknown request">>},
            {Data, [Error | Replies]};
        {ok, _Method, undefined, Ctx, Data2} ->
            Handler ! {conn, self(), {response, Result, Ctx}},
            {Data2, Replies};
        {ok, _, From, _, Data2} ->
            gen_statem:reply(From, {ok, Result}),
            {Data2, Replies}
    end.

process_error(Data = #data{handler = Handler},
              Replies, Code, Message, ErData, undefined) ->
    {Code2, Message2} = decode_error(Code, Message),
    Handler ! {conn, self(), {remote_error, Code2, Message2, ErData}},
    {Data, Replies};
process_error(Data = #data{handler = Handler},
              Replies, Code, Message, ErData, ReqRef) ->
    case outbound_del(Data, ReqRef) of
        {error, not_found} ->
            %FIXME: Not sure what error should be returned...
            Error = {invalid_request, -32600, <<"Error for unknown request">>},
            {Data, [Error | Replies]};
        {ok, _Method, undefined, Ctx, Data2} ->
            {Code2, Message2} = decode_error(Code, Message),
            Handler ! {conn, self(), {remote_error, Code2, Message2, ErData, Ctx}},
            {Data2, Replies};
        {ok, _, From, _, Data2} ->
            {Code2, Message2} = decode_error(Code, Message),
            gen_statem:reply(From, {remote_error, Code2, Message2, ErData}),
            {Data2, Replies}
    end.

inbound_response(Data = #data{batches = BatchMap, inbound = ReqMap},
                 Message, ReqRef) ->
    case maps:take(ReqRef, ReqMap) of
        error ->
            ?GRISP_ERROR("Ask to send a response to the unknown request ~p",
                         [ReqRef],
                         #{event => internal_error, ref => ReqRef,
                           reason => unknown_request}),
            Data;
        {#inbound_req{bref = undefined}, ReqMap2} ->
            % Not part of a batch response
            send_packet(Data#data{inbound = ReqMap2}, Message);
        {#inbound_req{bref = BatchRef}, ReqMap2} ->
            % The batch must exists
            case maps:find(BatchRef, BatchMap) of
                {ok, #batch{refcount = 1, responses = Responses}} ->
                    % This is the last message of the batch
                    BatchMap2 = maps:remove(BatchRef, BatchMap),
                    send_packet(Data#data{batches = BatchMap2,
                                              inbound = ReqMap2},
                                  [Message | Responses]);
                {ok, Batch = #batch{refcount = RefCount,
                                    responses = Responses}} ->
                    Batch2 = Batch#batch{refcount = RefCount - 1,
                                         responses = [Message | Responses]},
                    BatchMap2 = BatchMap#{BatchRef => Batch2},
                    Data#data{batches = BatchMap2, inbound = ReqMap2}
            end
    end.

inbound_add(Data = #data{inbound = ReqMap}, BatchRef, Method, ReqRef) ->
    %TODO: Should we add a timeout for inbound requests ?
    Req = #inbound_req{method = Method, id = ReqRef, bref = BatchRef},
    Data#data{inbound = ReqMap#{ReqRef => Req}}.

outbound_add(Data = #data{request_timeout = Timeout, outbound = ReqMap},
            Method, From, Ctx) ->
    ReqRef = make_reqref(),
    TRef = send_after(Timeout, {outbound_timeout, ReqRef}),
    Req = #outbound_req{id = ReqRef, method = Method, tref = TRef,
                       from = From, ctx = Ctx},
    {ReqRef, Data#data{outbound = ReqMap#{ReqRef => Req}}}.

outbound_del(Data = #data{outbound = ReqMap}, ReqRef) ->
    case maps:find(ReqRef, ReqMap) of
        error -> {error, not_found};
        {ok, #outbound_req{method = Method, tref = TRef,
                          from = From, ctx = Ctx}} ->
            cancel_timer(TRef),
            Data2 = Data#data{outbound = maps:remove(ReqRef, ReqMap)},
            {ok, Method, From, Ctx, Data2}
    end.

outbound_timeout(Data = #data{handler = Handler}, ReqRef) ->
    case outbound_del(Data, ReqRef) of
        {error, not_found} ->
            ?GRISP_WARN("Timeout for unknown request ~s", [ReqRef],
                        #{event => internal_error, ref => ReqRef,
                          reason => unknown_request}),
            Data;
        {ok, _Method, undefined, Ctx, Data2} ->
            Handler ! {conn, self(), {local_error, timeout, Ctx}},
            Data2;
        {ok, _, From, _, Data2} ->
            gen_statem:reply(From, {error, timeout}),
            Data2
    end.

requests_error(Data = #data{handler = Handler, outbound = ReqMap}, Reason) ->
    maps:foreach(fun
        (_, #outbound_req{from = undefined, ctx = Ctx}) ->
            Handler ! {conn, self(), {local_error, Reason, Ctx}};
        (_, #outbound_req{from = From, ctx = undefined}) ->
            gen_statem:reply(From, {error, Reason})
    end, ReqMap),
    Data#data{outbound = #{}}.

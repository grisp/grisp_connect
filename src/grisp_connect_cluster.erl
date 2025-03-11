-module(grisp_connect_cluster).

-behaviour(gen_server).


%--- Includes -------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").
-include_lib("grisp/include/grisp.hrl").


%--- Exports -------------------------------------------------------------------

% API Functions
-export([start_link/0]).
-export([join/2]).
-export([leave/1]).
-export([list/0]).
-export([is_allowed/1]).

% Behaviour gen_server callback functions
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

% Disable dialyzer warnings
-dialyzer({nowarn_function, clear_disp_pem_cache/0}).


%--- Types ---------------------------------------------------------------------

-record(peer, {
    node :: atom(),
    hostname :: binary(),
    address :: inet: ip4_address(),
    cookie :: atom(),
    ca :: binary(),
    fingerprint :: binary(),
    monitor :: boolean(),
    timer_ref :: undefined | reference()
}).

-record(state, {
    peers = #{} :: #{atom() => #peer{}}
}).

-type node_options() :: #{
    hostname := binary(),
    address := inet:ip4_address(),
    cookie := atom(),
    ca := binary(),
    fingerprint := binary(),
    monitor => boolean()
}.


%--- Macros --------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(RETRY_DELAY, 1000). % ms
-define(FORMAT(FMT, ARGS), iolist_to_binary(io_lib:format(FMT, ARGS))).
-define(FINGERPRINT_TABLE, grisp_connect_cluster_fingerprints).


%--- API FUNCTIONS -------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec join(Node :: atom(), Opts :: node_options()) -> true | false | error.
join(Node, Opts) when is_atom(Node), is_map(Opts) ->
    Peer = #peer{
        node = Node,
        hostname = required(binary, hostname, Opts),
        address = required(ipv4, address, Opts),
        cookie = required(atom, cookie, Opts),
        ca = required(pem, ca, Opts),
        fingerprint = required(fingerprint, fingerprint, Opts),
        monitor = optional(bool, false, monitor, Opts)
    },
    gen_server:call(?SERVER, {join, Peer}).

-spec leave(Node :: atom()) -> true | false.
leave(Node) when is_atom(Node) ->
    gen_server:call(?SERVER, {leave, Node}).

-spec list() -> [#{nodename := atom(), connected := boolean()}].
list() ->
    gen_server:call(?SERVER, list).

-spec is_allowed(CertFingerprint :: binary()) -> boolean().
is_allowed(CertFingerprint) ->
    case ets:lookup(?FINGERPRINT_TABLE, CertFingerprint) of
        [] -> false;
        [_] -> true
    end.


%--- Behaviour gen_server Callback Functions -----------------------------------

init([]) ->
    net_kernel:monitor_nodes(true, #{node_type => all, nodedown_reason => true}),
    ets:new(?FINGERPRINT_TABLE, [named_table, protected, set, {keypos, 1}]),
    net_kernel:allow([]), % Prevent connecting to any nodes by default
    {ok, store_ca_certs(store_board_certs(#state{}))}.

handle_call({join, Peer}, _From, State) ->
    case join_node(State, Peer) of
        {ok, IsConnected, State2} -> {reply, IsConnected, State2};
        {error, State2} -> {reply, error, State2}
    end;
handle_call({leave, Node}, _From, State) ->
    {WasConnected, State2} = leave_node(State, Node),
    {reply, WasConnected, State2};
handle_call(list, _From, State) ->
    {reply, list_node(State), State};
handle_call(Msg, From, _State) ->
    gen_server:reply(From, {error, unexpected_call}),
    error({unexpected_call, Msg}).

handle_cast(Msg, _State) ->
    error({unexpected_cast, Msg}).

handle_info({retry_connecting, Node}, State) ->
    case retry_node(State, Node) of
        {ok, _IsConnected, State2} -> {noreply, State2};
        {error, State2} -> {noreply, State2}
    end;
handle_info({nodedown, Node, Info}, State) ->
    {noreply, node_down(State, Node, Info)};
handle_info({nodeup, _Node, _Info}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_WARNING(#{
        description => ?FORMAT("Received unexpected message: ~w", [Info]),
        event => unexpected_message, message => Info}),
    {noreply, State}.


%--- Internal Funcitons --------------------------------------------------------

clear_disp_pem_cache() ->
    % ssl:clear_pem_cache/0 doesn't support distribution, hacking around...
    try gen_server:call(ssl_pem_cache:name(dist),
                        {unconditionally_clear_pem_cache, self()}, infinity)
    catch exit:{noproc,_} ->
        % No distribution PEM cache running
        ok
    end.

required(atom, Key, Map) ->
    case maps:find(Key, Map) of
        {ok, V} when is_atom(V) -> V;
        _ -> error(badarg)
    end;
required(binary, Key, Map) ->
    case maps:find(Key, Map) of
        {ok, V} when is_binary(V) -> V;
        _ -> error(badarg)
    end;
required(fingerprint, Key, Map) ->
    V = required(binary, Key, Map),
    case byte_size(V) of
        32 -> V;
        _ -> error(badarg)
    end;
required(pem, Key, Map) ->
    case maps:find(Key, Map) of
        {ok, V} when is_binary(V) ->
            case public_key:pem_decode(V) of
                [] -> error(badarg);
                _ -> V
            end;
        _ -> error(badarg)
    end;
required(ipv4, Key, Map) ->
    case maps:find(Key, Map) of
        {ok, {A, B, C, D} = IPv4}
          when A >= 0, A =< 256, B >= 0, B =< 256,
               C >= 0, C =< 256, D >= 0, D =< 256 ->
            IPv4;
        _ -> error(badarg)
    end.

optional(bool, Default, Key, Map) ->
    case maps:find(Key, Map) of
        {ok, V} when is_boolean(V) -> V;
        error -> Default;
    _ -> error(badarg)
    end.

store_board_certs(State) ->
    case ?IS_EMULATED of
        true -> State;
        false ->
            DerCert = grisp_cryptoauth:read_cert(primary, der),
            PemCert = der_list_to_pem([DerCert]),
            ok = file:write_file("/etc/board.pem", PemCert),
            State
    end.

store_ca_certs(State = #state{peers = Peers}) ->
    {ok, Filename} = application:get_env(grisp_connect, allowed_ca_chain),
    CAPemItems = unique([P#peer.ca || P <- maps:values(Peers)]),
    Data = lists:join("\n", CAPemItems),
    ok = file:write_file(Filename, Data),
    clear_disp_pem_cache(),
    State.

connect_node(State, Peer = #peer{node = Node}) ->
    case connect_peer(Peer) of
        {ok, Peer2} ->
            ?LOG_NOTICE(#{
                description => ?FORMAT("Joined node ~w cluster", [Node]),
                event => cluster_join, node => Node}),
            {ok, true, set_peer(State, Peer2)};
        {error, Peer2 = #peer{monitor = true}} ->
            ?LOG_DEBUG(#{
                description => ?FORMAT("Failed to join node ~w cluster, postpone connection", [Node]),
                event => cluster_join_postpone, node => Node}),
            {ok, false, set_peer(State, schedule_retry(Peer2))};
        {error, Peer2} ->
            ?LOG_DEBUG(#{
                description => ?FORMAT("Failed to join node ~w cluster", [Node]),
                event => cluster_join_failed, node => Node}),
            Peer3 = unregister_peer(State, Peer2),
            {error, store_ca_certs(del_peer(State, Peer3))}
    end.

join_node(State, Peer = #peer{node = Node}) ->
    {State2, Peer2} = case find_peer(State, Node) of
        {ok, Peer} ->
            {State, Peer};
        {ok, OldPeer} ->
            ?LOG_DEBUG(#{
                description => ?FORMAT("Update node ~w cluster configuration", [Node]),
                event => cluster_update, node => Node}),
            unregister_peer(State, disconnect_peer(OldPeer)),
            NewPeer = register_peer(Peer),
            {store_ca_certs(set_peer(State, NewPeer)), NewPeer};
        error ->
            ?LOG_DEBUG(#{
                description => ?FORMAT("Register node ~w cluster configuration", [Node]),
                event => cluster_register, node => Node}),
            NewPeer = register_peer(Peer),
            {store_ca_certs(set_peer(State, NewPeer)), NewPeer}
    end,
    connect_node(State2, Peer2).

leave_node(State, Node) ->
    case find_peer(State, Node) of
        error ->
            {false, State};
        {ok, Peer} ->
            ?LOG_NOTICE(#{
                description => ?FORMAT("Leaved node ~w cluster", [Node]),
                event => cluster_leave, node => Node}),
            WasConnected = is_peer_connected(Peer),
            Peer2 = unregister_peer(State, disconnect_peer(Peer)),
            {WasConnected, store_ca_certs(del_peer(State, Peer2))}
    end.

list_node(#state{peers = Peers}) ->
    [#{nodename => P#peer.node, connected => is_peer_connected(P)}
     || P <- maps:values(Peers)].

retry_node(State, Node) ->
    case find_peer(State, Node) of
        error ->
            {error, State};
        {ok, Peer} ->
            Peer2 = Peer#peer{timer_ref = undefined},
            connect_node(set_peer(State, Peer2), Peer2)
    end.

node_down(State, Node, Info) ->
    #{node_type := NodeType, nodedown_reason := Reason} = Info,
    case find_peer(State, Node) of
        error -> State;
        {ok, Peer} ->
            ?LOG_ERROR(#{
                description => ?FORMAT("Disconnected from node ~w cluster: ~w", [Node, Reason]),
                event => cluster_disconnected, node => Node,
                node_type => NodeType, reason => Reason}),
            case Peer#peer.monitor of
                true ->
                    set_peer(State, schedule_retry(Peer));
                false ->
                    ?LOG_NOTICE(#{
                        description => ?FORMAT("Leaved node ~w cluster", [Node]),
                        event => cluster_leaved, node => Node}),
                    Peer2 = unregister_peer(State, Peer),
                    store_ca_certs(del_peer(State, Peer2))
            end
    end.

set_peer(State = #state{peers = Peers}, Peer = #peer{node = Node}) ->
    State#state{peers = Peers#{Node => Peer}}.

del_peer(State = #state{peers = Peers}, #peer{node = Node}) ->
    State#state{peers = maps:remove(Node, Peers)}.

find_peer(#state{peers = Peers}, Node) ->
    maps:find(Node, Peers).

schedule_retry(Peer = #peer{node = Node, timer_ref = undefined}) ->
    Ref = erlang:send_after(?RETRY_DELAY, self(), {retry_connecting, Node}),
    Peer#peer{timer_ref = Ref};
schedule_retry(Peer = #peer{timer_ref = Ref}) ->
    erlang:cancel_timer(Ref),
    schedule_retry(Peer#peer{timer_ref = undefined}).

register_peer(Peer = #peer{node = Node, cookie = Cookie,
                           fingerprint = Fingerprint,
                           hostname = Hostname, address = Address}) ->
    ets:insert(?FINGERPRINT_TABLE, {Fingerprint, Node}),
    inet_db:add_host(Address, [binary_to_list(Hostname)]),
    erlang:set_cookie(Node, Cookie),
    net_kernel:allow([Node]),
    Peer.

% We need the full state to be sure to not remove an address used by another peer
unregister_peer(#state{peers = Peers},
                Peer = #peer{node = Node, fingerprint = Fingerprint,
                             address = Address}) ->
    ets:delete(?FINGERPRINT_TABLE, Fingerprint),
    SameAddr = [N || #peer{node = N, address = A} <- maps:values(Peers),
                     N =/= Node, A =:= Address],
    case SameAddr of
        [] -> inet_db:del_host(Address);
        _ -> ok
    end,
    Peer.

connect_peer(Peer = #peer{node = Node}) ->
    case net_adm:ping(Node) of
        pong -> {ok, Peer};
        pang -> {error, Peer}
    end.

disconnect_peer(Peer = #peer{node = Node}) ->
    erlang:disconnect_node(Node),
    Peer.

is_peer_connected(#peer{node = Node}) ->
    lists:member(Node, nodes()).

unique(L) ->
    maps:keys(maps:from_list([{K, true} || K <- L])).

der_list_to_pem(DerCerts) when is_list(DerCerts) ->
    lists:map(fun der_to_pem_block/1, DerCerts).

der_to_pem_block(Der) when is_binary(Der) ->
    Enc64 = base64:encode(Der),
    Wrapped = wrap_base64(Enc64, 64),
    [ "-----BEGIN CERTIFICATE-----\n",
      Wrapped,
      "-----END CERTIFICATE-----\n" ].

wrap_base64(Base64, LineLen) ->
    wrap_lines(Base64, LineLen, []).

wrap_lines(<<>>, _LineLen, Acc) ->
    lists:reverse(Acc);
wrap_lines(Data, LineLen, Acc) ->
    case Data of
        <<Line:LineLen/binary, Rest/binary>> ->
            wrap_lines(Rest, LineLen, ["\n", Line | Acc]);
        LastLine ->
            wrap_lines(<<>>, LineLen, ["\n", LastLine | Acc])
    end.

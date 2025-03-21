%% @doc Library module containing the jsonrpc API logic
-module(grisp_connect_api).

-export([handle_msg/1]).

-include_lib("kernel/include/logger.hrl").

%--- Macros --------------------------------------------------------------------
-define(method_get, get).
-define(method_post, post).


%--- API -----------------------------------------------------------------------

% @doc Handles requests and notifications from grisp.io.
-spec handle_msg(Msg) ->
    ok | {reply, Result :: term(), ReqRef :: binary() | integer()}
    | {error, Code :: integer() | atom(), Message :: binary() | undefined, ErData :: term(), ReqRef :: binary() | integer()}
  when Msg :: {request, Method :: jarl:method(), Params :: map() | list(), ReqRef :: binary() | integer()}
            | {notification, jarl:method(), Params :: map() | list()}.
handle_msg({notification, M, Params}) ->
    handle_notification(M, Params);
handle_msg({request, M, Params, ID}) ->
    try
        handle_request(M, Params, ID)
    catch
        _:R ->
            ?LOG_ERROR("Error while processing request ~s: ~p",
                       [jarl_utils:format_method(M), R]),
            ReasonBinary = iolist_to_binary(io_lib:format("~p", [R])),
            {error, internal_error, ReasonBinary, undefined, ID}
    end.


%--- Internal Funcitons --------------------------------------------------------

maybe_put(Map, []) -> Map;
maybe_put(Map, [{Key, Fun, Filter} | Rest]) ->
    maybe_put(maybe_put(Map, Key, Fun, Filter), Rest).

maybe_put(Map, Key, Fun, Filter) ->
    try Fun() of
        V when is_map(V) -> Map#{Key => maps:with(Filter, V)};
        _ -> Map
    catch
      _:_ -> Map
    end.

handle_notification([log, sync], Params) ->
    grisp_connect_log:sync(Params);
handle_notification(Method, Params) ->
    ?LOG_ERROR("Received unexpected notification ~p: ~p", [Method, Params]),
    ok.

handle_request([?method_get], #{type := <<"system_info">>} = _Params, ID) ->
    ClusterInfo = grisp_connect_cluster:system_info(),
    UpdateInfo = grisp_connect_updater:system_info(),
    Info = maybe_put(maps:merge(ClusterInfo, UpdateInfo), [
        {software, fun grisp_info:software/0,
         [id, relname, relvsn, profiles, toolchain_rev, rtems_ver, otp_ver]},
        {hardware, fun grisp_info:hardware/0,
         [platform, version, serial, batch]}
    ]),
    {reply, Info, ID};
handle_request([?method_post], #{type := <<"start_update">>} = Params, ID) ->
    try
        URL = maps:get(url, Params),
        case grisp_connect_updater:start_update(URL) of
            {error, grisp_updater_unavailable} ->
                {error, grisp_updater_unavailable, undefined, undefined, ID};
            {error, already_updating} ->
                {error, already_updating, undefined, undefined, ID};
            {error, boot_system_not_validated} ->
                {error, boot_system_not_validated, undefined, undefined, ID};
            {error, Reason} ->
                ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
                {error, internal_error, ReasonBinary, undefined, ID};
            ok ->
                {reply, ok, ID}
        end
    catch
        error:{badkey, _} ->
            {error, internal_error, <<"Invalid params">>, undefined, ID}
    end;
handle_request([?method_post], #{type := <<"validate">>}, ID) ->
    case grisp_connect_updater:validate() of
        {error, grisp_updater_unavailable} ->
            {error, grisp_updater_unavailable, undefined, undefined, ID};
        {error, {validate_from_unbooted, PartitionIndex}} ->
            {error, validate_from_unbooted, undefined, PartitionIndex, ID};
        {error, Reason} ->
            ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
            {error, internal_error, ReasonBinary, undefined, ID};
        ok ->
            {reply, ok, ID}
    end;
handle_request([?method_post], #{type := <<"reboot">>}, ID) ->
    grisp_connect_client:reboot(),
    {reply, ok, ID};
handle_request([?method_post], #{type := <<"cancel">>}, ID) ->
    case grisp_connect_updater:cancel() of
        {error, grisp_updater_unavailable} ->
            {error, grisp_updater_unavailable, undefined, undefined, ID};
        ok ->
            {reply, ok, ID}
    end;
handle_request([log, get], Params, ID) ->
    {reply, grisp_connect_log:get(Params), ID};
handle_request([cluster, join], Params, ID) ->
    #{cookie := CookieBin, ca := CAPemBin, nodename := NodeNameBin,
        fingerprint := FingerprintB64, hostname := HostnameBin,
        address := AddressBin} = Params,
    Node = binary_to_atom(NodeNameBin),
    {ok, Address} = inet:parse_ipv4_address(binary_to_list(AddressBin)),
    Opts = #{
        hostname => HostnameBin,
        address => Address,
        cookie => binary_to_atom(CookieBin),
        ca => CAPemBin,
        fingerprint => base64:decode(FingerprintB64),
        monitor => maps:get(monitor, Params, false)
    },
    case grisp_connect_cluster:join(Node, Opts) of
        error -> {reply, false, ID};
        IsConnected -> {reply, IsConnected, ID}
    end;
handle_request([cluster, leave], Params, ID) ->
    #{nodename := NodeNameBin} = Params,
    Node = binary_to_atom(NodeNameBin),
    {reply, grisp_connect_cluster:leave(Node), ID};
handle_request([cluster, list], #{}, ID) ->
    {reply, grisp_connect_cluster:list(), ID};
handle_request(Method, Params, ID) ->
    ?LOG_ERROR("Received unexpected request ~p: ~p", [Method, Params]),
    {error, method_not_found, undefined, undefined, ID}.

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
    handle_request(M, Params, ID).


%--- Internal Funcitons --------------------------------------------------------

handle_notification([log, sync], Params) ->
    grisp_connect_log:sync(Params);
handle_notification(Method, Params) ->
    ?LOG_ERROR("Received unexpected notification ~p: ~p", [Method, Params]),
    ok.

handle_request([?method_get], #{type := <<"system_info">>} = _Params, ID) ->
    Info = grisp_connect_updater:system_info(),
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
handle_request(Method, Params, ID) ->
    ?LOG_ERROR("Received unexpected request ~p: ~p", [Method, Params]),
    {error, method_not_found, undefined, undefined, ID}.

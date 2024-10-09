%% @doc Library module containing the jsonrpc API logic
-module(grisp_connect_api).

-export([request/3]).
-export([notify/3]).
-export([handle_msg/1]).

-include_lib("kernel/include/logger.hrl").

%--- Macros --------------------------------------------------------------------
-define(method_get, <<"get">>).
-define(method_post, <<"post">>).
-define(method_patch, <<"patch">>).
-define(method_delete, <<"delete">>).

%--- API -----------------------------------------------------------------------

% #doc Assembles a jsonrpc request and its uuid
-spec request(Method :: atom() | binary(),
              Type :: atom() | binary(),
              Params :: map()) -> {ID :: binary(), Encoded :: binary()}.
request(Method, Type, Params) ->
    ID = id(),
    Rpc = {request, Method, maps:put(type, Type, Params), ID},
    Encoded = grisp_connect_jsonrpc:encode(Rpc),
    {ID, Encoded}.

% #doc Assembles a jsonrpc notification
-spec notify(Method :: atom() | binary(),
              Type :: atom() | binary(),
              Params :: map()) -> Encoded :: binary().
notify(Method, Type, Params) ->
    Rpc = {notification, Method, maps:put(type, Type, Params)},
    grisp_connect_jsonrpc:encode(Rpc).

% @doc Indentifies if the message is a request or a reply to a previous request.
% In case it was a request, returns the reply to be sent to the peer.
% In case it was a response, returns the parsed ID and content to be handled by
% the caller.
-spec handle_msg(JSON :: binary()) ->
    {send_response, Response :: binary()} |
    {handle_response, ID :: binary(), {ok, Result :: map()} | {error, atom()}}.
handle_msg(JSON) ->
    JSON_RPC = grisp_connect_jsonrpc:decode(JSON),
    handle_jsonrpc(JSON_RPC).

%--- Internal Funcitons --------------------------------------------------------

handle_jsonrpc({batch, Batch}) ->
    handle_rpc_messages(Batch, []);
handle_jsonrpc({single, Rpc}) ->
    handle_rpc_messages([Rpc], []).

handle_rpc_messages([], Replies) -> lists:reverse(Replies);
handle_rpc_messages([{request, M, Params, ID} | Batch], Replies)
  when M == ?method_post;
       M == ?method_get ->
    handle_rpc_messages(Batch, [handle_request(M, Params, ID) | Replies]);
handle_rpc_messages([{result, _, _} = Res| Batch], Replies) ->
    handle_rpc_messages(Batch, [handle_response(Res)| Replies]);
handle_rpc_messages([{error, _Code, _Msg, _Data, _ID} = E | Batch], Replies) ->
    ?LOG_INFO("Received JsonRPC error: ~p",[E]),
    handle_rpc_messages(Batch, [handle_response(E)| Replies]);
handle_rpc_messages([{internal_error, _, _} = E | Batch], Replies) ->
    ?LOG_ERROR("JsonRPC: ~p",[E]),
    handle_rpc_messages(Batch,
                        [grisp_connect_jsonrpc:format_error(E)| Replies]).

handle_request(?method_get, #{type := <<"partition_state">>} = _Params, ID) ->
    Info = get_partition_info(),
    Reply = case Info of
        #{state           := _State,
          message         := _Msg,
          action_required := _ActionRequired} = Response -> 
            {result, Response, ID};
        {error, Reason} -> 
            ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
            grisp_connect_jsonrpc:format_error({internal_error, ReasonBinary, ID})
    end,
    {send_response, grisp_connect_jsonrpc:encode(Reply)};
handle_request(?method_post, #{type := <<"start_update">>} = Params, ID) ->
    try
        URL = maps:get(url, Params),
        Reply = case start_update(URL) of
            {error, grisp_updater_unavailable} ->
                {error, -10, grisp_updater_unavailable, undefined, ID};
            {error, already_updating} ->
                {error, -11, already_updating, undefined, ID};
            {error, boot_system_not_validated} ->
                {error, -12, boot_system_not_validated, undefined, ID};
            {error, Reason} ->
                ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
                grisp_connect_jsonrpc:format_error({internal_error, ReasonBinary, ID});
            ok ->
                {result, ok, ID}
        end,
        {send_response, grisp_connect_jsonrpc:encode(Reply)}
    catch
        throw:bad_key ->
            {send_response,
             grisp_connect_jsonrpc:format_error(
                {internal_error, invalid_params, ID})}
        end;
handle_request(?method_post, #{type := <<"validate">>}, ID) ->
    Reply = case grisp_updater:validate() of
        {error, {validate_from_unbooted, PartitionIndex}} ->
            {error, -13, validate_from_unbooted, PartitionIndex, ID};
        {error, Reason} ->
            ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
            grisp_connect_jsonrpc:format_error({internal_error, ReasonBinary, ID});
        ok ->
            {result, ok, ID}
    end,
    {send_response,  grisp_connect_jsonrpc:encode(Reply)};
handle_request(_, _, ID) ->
    Error = {internal_error, method_not_found, ID},
    FormattedError = grisp_connect_jsonrpc:format_error(Error),
    {send_response, grisp_connect_jsonrpc:encode(FormattedError)}.

handle_response(Response) ->
    {ID, Reply} = case Response of
        {result, Result, ID0} ->
            {ID0, {ok, Result}};
        {error, Code, _Message, _Data, ID0} ->
            {ID0, {error, error_atom(Code)}}
    end,
    {handle_response, ID, Reply}.

start_update(URL) ->
    case is_running(grisp_updater) of
        true -> grisp_updater:start(URL,
                                    grisp_connect_updater_progress,
                                    #{client => self()}, #{});
        false -> {error, grisp_updater_unavailable}
    end.

get_partition_info() ->
    case is_running(grisp_updater) of
        true -> 
            Info = grisp_updater:info(),
            #{boot := Boot, valid := Valid, next := Next} = Info,
            ActionRequired = maps:get(action_required, Info, false),
            case evaluate_partition_state(Boot, Valid, Next) of
                new_boot -> 
                    #{state           => <<"new">>, 
                      message         => <<"New partition booted, validation required">>, 
                      action_required => ActionRequired};
                update_pending -> 
                    #{state           => <<"old">>, 
                      message         => <<"Reboot required to load new partition">>, 
                      action_required => ActionRequired};
                no_update_pending -> 
                    #{state           => <<"old_no_update">>, 
                      message         => <<"No update pending, running old partition">>,
                      action_required => ActionRequired};
                _ -> 
                    #{state           => <<"unknown">>, 
                      message         => <<"Unknown partition state">>, 
                      action_required => ActionRequired}
            end;
        false -> {error, grisp_updater_unavailable}
    end.

evaluate_partition_state(BootPartition, ValidPartition, NextPartition) ->
    case {BootPartition, ValidPartition, NextPartition} of
        % Case 1: Booting from removable media, but system has a pending update
        {#{type := removable}, 
         #{type := system, id := ValidId}, 
         #{type := system, id := NextId}} 
            when ValidId =/= NextId -> update_pending;
        % Case 2: Booted from system partition, but a different system partition is pending update
        {#{type := system, id := BootId}, 
         #{type := system, id := ValidId}, 
         #{type := system, id := NextId}} 
            when BootId == ValidId, ValidId =/= NextId -> update_pending;
        % Case 3: Booted from a new partition, validation required
        {#{type := system, id := BootId}, 
         #{type := system, id := ValidId}, 
         _} 
            when BootId =/= ValidId -> new_boot;
        % Case 4: Booted from removable media, no update pending
        {#{type := removable}, 
         #{type := system, id := ValidId}, 
         #{type := system, id := NextId}}
            when ValidId == NextId -> no_update_pending;
        % Case 5: Booted from system partition, no update pending
        {#{type := system, id := BootId}, 
           _, 
         #{type := system, id := NextId}}
            when NextId == BootId -> no_update_pending;
        % Default case: Unknown partition state
        _ -> unknown_state
    end.

is_running(AppName) ->
    Apps = application:which_applications(),
    case [App || {App, _Desc, _VSN} <- Apps, App =:= AppName] of
        [] -> false;
        [_] -> true
    end.

error_atom(-1)  -> device_not_linked;
error_atom(-2)  -> token_expired;
error_atom(-3)  -> device_already_linked;
error_atom(-4)  -> invalid_token;
error_atom(-10) -> grisp_updater_unavailable;
error_atom(-11) -> already_updating;
error_atom(-12) -> boot_system_not_validated;
error_atom(-13) -> validate_from_unbooted;
error_atom(_)   -> jsonrpc_error.

id() ->
    list_to_binary(integer_to_list(erlang:unique_integer())).

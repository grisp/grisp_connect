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

format_error({internal_error, parse_error, ID}) ->
    {error, -32700, <<"Parse error">>, undefined, ID};
format_error({internal_error, invalid_request, ID}) ->
    {error, -32600, <<"Invalid request">>, undefined, ID};
format_error({internal_error, method_not_found, ID}) ->
    {error, -32601, <<"Method not found">>, undefined, ID};
format_error({internal_error, invalid_params, ID}) ->
    {error, -32602, <<"Invalid params">>, undefined, ID};
format_error({internal_error, Reason, ID}) ->
    {error, -32603, <<"Internal error">>, Reason, ID}.

handle_jsonrpc(Messages) ->
    handle_rpc_messages(Messages, []).

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
handle_rpc_messages([{decoding_error, _, _, _, _} = E | Batch], Replies) ->
    ?LOG_ERROR("JsonRPC decoding error: ~p",[E]),
    handle_rpc_messages(Batch, Replies).

handle_request(?method_get, #{type := <<"system_info">>} = _Params, ID) ->
    Info = grisp_connect_updater:system_info(),
    {send_response, grisp_connect_jsonrpc:encode({result, Info, ID})};
handle_request(?method_post, #{type := <<"start_update">>} = Params, ID) ->
    try
        URL = maps:get(url, Params),
        Reply = case grisp_connect_updater:start_update(URL) of
            {error, grisp_updater_unavailable} ->
                {error, -10, grisp_updater_unavailable, undefined, ID};
            {error, already_updating} ->
                {error, -11, already_updating, undefined, ID};
            {error, boot_system_not_validated} ->
                {error, -12, boot_system_not_validated, undefined, ID};
            {error, Reason} ->
                ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
                format_error({internal_error, ReasonBinary, ID});
            ok ->
                {result, ok, ID}
        end,
        {send_response, grisp_connect_jsonrpc:encode(Reply)}
    catch
        throw:bad_key ->
            {send_response,
             format_error({internal_error, invalid_params, ID})}
        end;
handle_request(?method_post, #{type := <<"validate">>}, ID) ->
    Reply = case grisp_connect_updater:validate() of
        {error, grisp_updater_unavailable} ->
            {error, -10, grisp_updater_unavailable, undefined, ID};
        {error, {validate_from_unbooted, PartitionIndex}} ->
            {error, -13, validate_from_unbooted, PartitionIndex, ID};
        {error, Reason} ->
            ReasonBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
            format_error({internal_error, ReasonBinary, ID});
        ok ->
            {result, ok, ID}
    end,
    {send_response,  grisp_connect_jsonrpc:encode(Reply)};
handle_request(?method_post, #{type := <<"reboot">>}, ID) ->
    grisp_connect_client:reboot(),
    {send_response,  grisp_connect_jsonrpc:encode({result, ok, ID})};
handle_request(?method_post, #{type := <<"cancel">>}, ID) ->
    Reply = case grisp_connect_updater:cancel() of
        {error, grisp_updater_unavailable} ->
            {error, -10, grisp_updater_unavailable, undefined, ID};
        ok ->
            {result, ok, ID}
    end,
    {send_response,  grisp_connect_jsonrpc:encode(Reply)};
handle_request(_T, _P, ID) ->
    Error = {internal_error, method_not_found, ID},
    FormattedError = format_error(Error),
    {send_response, grisp_connect_jsonrpc:encode(FormattedError)}.

handle_response(Response) ->
    {ID, Reply} = case Response of
        {result, Result, ID0} ->
            {ID0, {ok, Result}};
        {error, Code, _Message, _Data, ID0} ->
            {ID0, {error, error_atom(Code)}}
    end,
    {handle_response, ID, Reply}.

error_atom(-1)  -> device_not_linked;
error_atom(-2)  -> token_expired;
error_atom(-3)  -> device_already_linked;
error_atom(-4)  -> invalid_token;
error_atom(_)   -> jsonrpc_error.

id() ->
    list_to_binary(integer_to_list(erlang:unique_integer())).

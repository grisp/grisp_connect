%% @doc Library module containing the jsonrpc API logic
-module(grisp_connect_api).

-export([request/3]).
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
  when M == ?method_post ->
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

handle_request(?method_post, #{type := <<"start_update">>} = Params, ID) ->
    try
        URL = maps:get(url, Params),
        Reply = case start_update(URL) of
            {error, grisp_updater_unavailable} ->
                {error, -10, grisp_updater_unavailable, undefined, ID};
            {error, already_updating} ->
                {error, -11, already_updating, undefined, ID};
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
handle_request(?method_post, #{type := <<"flash">>} = Params, ID) ->
    Led = maps:get(led, Params, 1),
    Color = maps:get(color, Params, red),
    Reply = {result, flash(Led, Color), ID},
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

flash(Led, Color) ->
    spawn(fun() ->
        ?LOG_NOTICE("Flash from Seawater!~n"),
        grisp_led:color(Led, Color),
        timer:sleep(100),
        grisp_led:off(Led)
    end),
    ok.

start_update(URL) ->
    case is_running(grisp_updater) of
        true -> grisp_updater:start(URL,
                                    grisp_connect_updater_progress,
                                    #{client => self()}, #{});
        false -> {error, grisp_updater_unavailable}
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
error_atom(_)   -> jsonrpc_error.

id() ->
    list_to_binary(integer_to_list(erlang:unique_integer())).

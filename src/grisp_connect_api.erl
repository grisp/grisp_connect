%% @doc Librabry module containing the jsonrpc API logic
-module(grisp_connect_api).

-export([request/3]).
-export([handle_msg/1]).

-include_lib("kernel/include/logger.hrl").

%--- API -----------------------------------------------------------------------

request(Method, Type, Params) ->
    ID = id(),
    Rpc = {request, Method, maps:put(type, Type, Params), ID},
    Encoded = grisp_connect_jsonrpc:encode(Rpc),
    {ID, Encoded}.

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
when M == <<"post">> ->
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

handle_request(<<"post">>, #{type := <<"start_update">>} = Params, ID) ->
    URL = maps:get(url, Params, 1),
    case start_update(URL) of
        % TODO: start_update error cast
        {error, update_unavailable} -> {error,
                                        -564095940, 
                                        <<"grisp_updater unavailable">>, 
                                        undefined, 
                                        ID};
        {error, Reason} -> {error, 
                            -32603, 
                            iolist_to_binary(io_lib:format("~p", [Reason])), ID};
        ok -> {result, undefined, ID}
    end;
handle_request(<<"post">>, #{type := <<"flash">>} = Params, ID) ->
    Led = maps:get(led, Params, 1),
    Color = maps:get(color, Params, red),
    {result, flash(Led, Color), ID};
handle_request(_, _, ID) ->
    grisp_connect_jsonrpc:format_error({internal_error, method_not_found, ID}).

handle_response(Response) ->
    {ID, Reply} = case Response of
        {result, Result, ID0} ->
            {ID0, {ok, Result}};
        {error, Code, _Message, _Data, ID0} ->
            {ID0, {error, error_atom(Code)}}
    end,
    {response, ID, Reply}.

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
        true -> grisp_updater:start_update(URL, 
                                           grisp_conntect_updater_progress,
                                           #{client => self()}, #{});
        false -> {error, update_unavailable}
    end.

is_running(AppName) ->
    Apps = application:which_applications(),
    case [App || {App, _Desc, _VSN} <- Apps, App =:= AppName] of
        [] -> false;
        [_] -> true
    end.

error_atom(-1) -> device_not_linked;
error_atom(-2) -> token_expired;
error_atom(-3) -> device_already_linked;
error_atom(-4) -> invalid_token;
error_atom(_)  -> jsonrpc_error.

id() ->
    list_to_binary(integer_to_list(erlang:unique_integer())).

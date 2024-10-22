-module(grisp_connect_jsonrpc).

% API
-export([decode/1]).
-export([encode/1]).


%--- Types ---------------------------------------------------------------------

-type json_rpc_message() ::
    {request, Method :: binary(), Params :: map(), ReqRef :: binary()}
  | {result, Result :: term(), ReqRef :: binary()}
  | {notification, Method :: binary(), Params :: map()}
  | {error, Code :: integer(), Message :: undefined | binary(), Data :: undefined | term(), ReqRef :: undefined | binary()}
  | {decoding_error, Code :: integer(), Message :: undefined | binary(), Data :: undefined | term(), ReqRef :: undefined | binary()}.



%--- Macros --------------------------------------------------------------------

-define(V, jsonrpc => <<"2.0">>).
-define(is_valid(Message),
    (map_get(jsonrpc, Message) == <<"2.0">>)
).
-define(is_method(Method),
    (is_atom(Method) orelse is_binary(Method))
).
-define(is_params(Params),
    (is_map(Params) orelse is_list(Params))
).


%--- API -----------------------------------------------------------------------

%% @doc Decode a JSONRpc text packet and returns a list of decoded messages.
%% If some decoding errors occure while doing do, a special error message with
%% the tag `decoding_error` that can be encoded and sent back directly to the
%% JSONRpc peer.
%%
%% During JSON decoding, the `null` values are changed to `undefined` and when
%% encoding, `undefined` values are changed back to `null`.
%%
%% <p>The possible decoded messages are:
%% <ul>
%%     <li><b><c>{request, Method :: binary(), Params :: map(), ReqRef :: binary()}</c></b></li>
%%     <li><b><c>{result, Result :: term(), ReqRef :: binary()}</c></b></li>
%%     <li><b><c>{notification, Method :: binary(), Params :: map()}</c></b></li>
%%     <li><b><c>{error, Code :: integer(), Message :: undefined | binary(), Data :: undefined | term(), ReqRef :: undefined | binary()}</c></b></li>
%%     <li><b><c>{decoding_error, Code :: integer(), Message :: undefined | binary(), Data :: undefined | term(), ReqRef :: undefined | binary()}</c></b></li>
%% </ul>
-spec decode(Data :: iodata()) -> [json_rpc_message()].
decode(Data) ->
    case json_to_term(iolist_to_binary(Data)) of
        [] ->
            [{decoding_error, -32600, <<"Invalid Request">>, undefined, undefined}];
        Messages when is_list(Messages) ->
            [unpack(M) || M <- Messages];
        Message when is_map(Message) ->
            [unpack(Message)];
        {error, _Reason} ->
            [{decoding_error, -32700, <<"Parse error">>, undefined, undefined}]
    end.

%% @doc Encode a JSONRpc message or a list of JSONRpc messages to JSON text.
-spec encode(Messages :: json_rpc_message() | [json_rpc_message()]) -> iodata().
encode(Messages) when is_list(Messages) ->
    term_to_json([pack(M) || M <- Messages]);
encode(Message) ->
    encode([Message]).


%--- Internal ------------------------------------------------------------------

as_bin(undefined) -> undefined;
as_bin(Binary) when is_binary(Binary) -> Binary;
as_bin(List) when is_list(List) -> list_to_binary(List);
as_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom).

unpack(#{method := Method, params := Params, id := ID} = M)
  when ?is_valid(M), ?is_method(Method), ?is_params(Params), id =/= undefined ->
    {request, as_bin(Method), Params, as_bin(ID)};
unpack(#{method := Method, id := ID} = M)
  when ?is_valid(M), ?is_method(Method), ID =/= undefined ->
    {request, as_bin(Method), undefined, as_bin(ID)};
unpack(#{method := Method, params := Params} = M)
  when ?is_valid(M), ?is_method(Method), ?is_params(Params) ->
    {notification, as_bin(Method), Params};
unpack(#{method := Method} = M)
  when ?is_valid(M), ?is_method(Method) ->
    {notification, as_bin(Method), undefined};
unpack(#{result := Result, id := ID} = M)
  when ?is_valid(M) ->
    {result, Result, as_bin(ID)};
unpack(#{error := #{code := Code, message := Message, data := Data},
         id := ID} = M)
  when ?is_valid(M), is_integer(Code) ->
    {error, Code, as_bin(Message), Data, as_bin(ID)};
unpack(#{error := #{code := Code, message := Message}, id := ID} = M)
  when ?is_valid(M), is_integer(Code) ->
    {error, Code, as_bin(Message), undefined, as_bin(ID)};
unpack(#{id := ID}) ->
    {decoding_error, -32600, <<"Invalid Request">>, undefined, as_bin(ID)};
unpack(_M) ->
    {decoding_error, -32600, <<"Invalid Request">>, undefined, undefined}.

pack({request, Method, undefined, ID})
  when is_binary(Method), is_binary(ID) ->
    #{?V, method => Method, id => ID};
pack({request, Method, Params, ID})
  when is_binary(Method), is_binary(ID), Params =:= undefined orelse is_map(Params), is_binary(ID) ->
    #{?V, method => Method, params => Params, id => ID};
pack({notification, Method, undefined})
  when is_binary(Method) ->
    #{?V, method => Method};
pack({notification, Method, Params})
  when is_binary(Method), Params =:= undefined orelse is_map(Params) ->
    #{?V, method => Method, params => Params};
pack({result, Result, ID})
  when is_binary(ID) ->
    #{?V, result => Result, id => ID};
pack({ErrorTag, Code, Message, undefined, undefined})
  when ErrorTag =:= error, ErrorTag =:= decoding_error, is_integer(Code),
       Message =:= undefined orelse is_binary(Message) ->
    #{?V, error => #{code => Code, message => Message}, id => null};
pack({ErrorTag, Code, Message, undefined, ID})
  when ErrorTag =:= error, ErrorTag =:= decoding_error, is_integer(Code),
       Message =:= undefined orelse is_binary(Message), is_binary(ID) ->
    #{?V, error => #{code => Code, message => Message}, id => ID};
pack({ErrorTag, Code, Message, Data, undefined})
  when ErrorTag =:= error, ErrorTag =:= decoding_error, is_integer(Code),
       Message =:= undefined orelse is_binary(Message) ->
    #{?V, error => #{code => Code, message => Message, data => Data, id => null}};
pack({ErrorTag, Code, Message, Data, ID})
  when ErrorTag =:= error, ErrorTag =:= decoding_error, is_integer(Code),
       Message =:= undefined orelse is_binary(Message), is_binary(ID) ->
    #{?V, error => #{code => Code, message => Message, data => Data}, id => ID};
pack(_Message) ->
    erlang:error(badarg).

json_to_term(Bin) ->
    try jsx:decode(Bin, [{labels, attempt_atom}, return_maps]) of
        Json -> postprocess(Json)
    catch
        error:E -> {error, E}
    end.

term_to_json(Term) ->
    jsx:encode(preprocess(Term)).

postprocess(null) -> undefined;
postprocess(List) when is_list(List) ->
    [postprocess(E) || E <- List];
postprocess(Map) when is_map(Map) ->
    maps:from_list([{K, postprocess(V)} || {K, V} <- maps:to_list(Map)]).

preprocess(undefined) -> null;
preprocess(List) when is_list(List) ->
    [preprocess(E) || E <- List];
preprocess(Map) when is_map(Map) ->
    maps:from_list([{K, preprocess(V)} || {K, V} <- maps:to_list(Map)]).

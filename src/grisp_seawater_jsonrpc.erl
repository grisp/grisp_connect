-module(grisp_seawater_jsonrpc).

% API
-export([decode/1]).
-export([encode/1]).
-export([format_error/1]).

%--- Types ---------------------------------------------------------------------

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

%--- API ----------------------------------------------------------------------

decode(Term) ->
    case json_to_map(Term) of
        [] ->
            {single, {internal_error, invalid_request, null}};
        Messages when is_list(Messages) ->
            {batch, [unpack(M) || M <- Messages]};
        Message when is_map(Message) ->
            {single, unpack(Message)};
        {error, _E} ->
            {single, {internal_error, parse_error, null}}
    end.

encode([Message]) ->
    encode(Message);
encode(Messages) when is_list(Messages) ->
    map_to_json([ pack(M) || M <- Messages]);
encode(Message) ->
    map_to_json(pack(Message)).

format_error({internal_error, parse_error, ID}) ->
    {error, -32700, <<"Parse error">>, undefined, ID};
format_error({internal_error, invalid_request, ID}) ->
    {error, -32600, <<"Invalid request">>, undefined, ID};
format_error({internal_error, method_not_found, ID}) ->
    {error, -32601, <<"Method not found">>, undefined, ID};
format_error({internal_error, invalid_params, ID}) ->
    {error, -32602, <<"Invalid params">>, undefined, ID};
format_error({internal_error, internal_error, ID}) ->
    {error, -32603, <<"Internal error">>, undefined, ID}.

%--- Internal -----------------------------------------------------------------

unpack(#{method := Method, params := Params, id := ID} = M)
        when ?is_valid(M), ?is_method(Method), ?is_params(Params) ->
    {request, Method, Params, ID};
unpack(#{method := Method, id := ID} = M)
        when ?is_valid(M), ?is_method(Method) ->
    {request, Method, undefined, ID};
unpack(#{method := Method, params := Params} = M)
        when ?is_valid(M), ?is_method(Method), ?is_params(Params) ->
    {notification, Method, Params};
unpack(#{method := Method} = M)
        when ?is_valid(M), ?is_method(Method) ->
    {notification, Method, undefined};
unpack(#{method := Method, params := _Params, id := ID} = M)
        when ?is_valid(M), ?is_method(Method) ->
    {internal_error, invalid_params, ID};
unpack(#{result := Result, id := ID} = M)
        when ?is_valid(M) ->
    {result, Result, ID};
unpack(#{error := #{code := Code,
                    message := Message,
                    data := Data},
                    id := ID} = M)
        when ?is_valid(M) ->
    {error, Code, Message, Data, ID};
unpack(#{error := #{code := Code,
                    message := Message},
                    id := ID} = M)
        when ?is_valid(M) ->
    {error, Code, Message, undefined, ID};
unpack(M) ->
    {internal_error, invalid_request, id(M)}.

pack({request, Method, undefined, ID}) ->
    #{?V, method => Method, id => ID};
pack({request, Method, Params, ID}) ->
    #{?V, method => Method, params => Params, id => ID};
pack({notification, Method, undefined}) ->
    #{?V, method => Method};
pack({notification, Method, Params}) ->
    #{?V, method => Method, params => Params};
pack({result, Result, ID}) ->
    #{?V, result => Result, id => ID};
pack({error, Type, ID}) ->
    pack(format_error({internal_error, Type, ID}));
pack({error, Code, Message, undefined, undefined}) ->
    #{?V, error => #{code => Code, message => Message}, id => null};
pack({error, Code, Message, undefined, ID}) ->
    #{?V, error => #{code => Code, message => Message}, id => ID};
pack({error, Code, Message, Data, undefined}) ->
    #{?V, error => #{code => Code, message => Message, data => Data, id => null}};
pack({error, Code, Message, Data, ID}) ->
    #{?V, error => #{code => Code, message => Message, data => Data}, id => ID}.

id(Object) when is_map(Object) -> maps:get(id, Object, undefined);
id(_Object) -> undefined.


json_to_map(Bin) ->
    try jsx:decode(Bin, [{labels, attempt_atom}, return_maps])
    catch
        error:E -> {error, E}
    end.

map_to_json(Map) ->
    jsx:encode(Map).

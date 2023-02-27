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
    case json_to_term(Term) of
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
    term_to_json([pack(M) || M <- Messages]);
encode(Message) ->
    term_to_json(pack(Message)).

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

id(Object) when is_map(Object) -> maps:get(id, Object, null);
id(_Object) -> null.


json_to_term(Bin) ->
    try jsx:decode(Bin, [{labels, attempt_atom}, return_maps])
    catch
        error:E -> {error, E}
    end.

term_to_json(Map) ->
    jsx:encode(Map).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

positional_parameters_test() ->
    Term = {request, <<"subtract">>, [42,23], 1},
    Json = <<"{\"id\":1,\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23]}">>,
    ?assertMatch(Json, encode(Term)),
    ?assertMatch({single, Term}, decode(Json)).

named_parameters_test() ->
    Term = {request, <<"subtract">>, #{<<"subtrahend">> => 23, <<"minuend">> => 42}, 2},
    Json = <<"{\"id\":2,\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":{\"minuend\":42,\"subtrahend\":23}}">>,
    ?assertMatch(Json, encode(Term)),
    ?assertMatch({single, Term}, decode(Json)).

notification_test() ->
    Term = {notification, <<"update">>, [1,2,3,4,5]},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}">>,
    ?assertMatch(Json, encode(Term)),
    ?assertMatch({single, Term}, decode(Json)).

invalid_json_test() ->
    Term = {internal_error, parse_error, null},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"foobar,\"params\":\"bar\",\"baz]">>,
    ?assertMatch({single, Term}, decode(Json)),
    JsonError = <<"{\"error\":{\"code\":-32700,\"message\":\"Parse error\"},\"id\":null,\"jsonrpc\":\"2.0\"}">>,
    ?assertMatch(JsonError, encode(format_error(Term))).

invalid_request_test() ->
    Term = {internal_error, invalid_request, null},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":1,\"params\":\"bar\"}">>,
    ?assertMatch({single, Term}, decode(Json)),
    JsonError = <<"{\"error\":{\"code\":-32600,\"message\":\"Invalid request\"},\"id\":null,\"jsonrpc\":\"2.0\"}">>,
    ?assertMatch(JsonError, encode(format_error(Term))).

batch_test() ->
    Term1 = {request, <<"sum">>, [1,2,4], <<"1">>},
    Term2 = {internal_error, invalid_request, null},
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4],\"id\":\"1\"},{\"foo\":\"boo\"}]">>,
    ?assertMatch({batch, [Term1,Term2]}, decode(Json)),
    JsonError = <<"[{\"id\":\"1\",\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4]},{\"error\":{\"code\":-32600,\"message\":\"Invalid request\"},\"id\":null,\"jsonrpc\":\"2.0\"}]">>,
    ?assertMatch(JsonError, encode([Term1,format_error(Term2)])).

result_test() ->
    Term = {result, 7, 45},
    Json = <<"{\"id\":45,\"jsonrpc\":\"2.0\",\"result\":7}">>,
    ?assertMatch(Json, encode(Term)),
    ?assertMatch({single, Term}, decode(Json)).

-endif.
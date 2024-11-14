-module(grisp_connect_jsonrpc_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([positional_parameters/1,
        named_parameters/1,
        using_existing_atoms/1,
        notification/1,
        invalid_json/1,
        invalid_request/1,
        batch/1,
        result/1,
        null_values/1]).

all() -> [
    positional_parameters,
    named_parameters,
    using_existing_atoms,
    notification,
    invalid_json,
    invalid_request,
    batch,
    result,
    null_values
].

positional_parameters(_) ->
    Term = {request, <<"subtract">>, [42,23], 1},
    Json = <<"{\"id\":1,\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23]}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    Json2 = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":1">>,
                           <<"\"method\":\"subtract\"">>,
                           <<"\"params\":[42,23]">>], Json2)).

named_parameters(_) ->
    Term = {request, <<"divide">>, #{<<"dividend">> => 42, <<"divisor">> => 2}, 2},
    Json = <<"{\"id\":2,\"jsonrpc\":\"2.0\",\"method\":\"divide\",\"params\":{\"dividend\":42,\"divisor\":2}}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    Json2 = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":2">>,
                           <<"\"method\":\"divide\"">>,
                           <<"\"dividend\":42">>,
                           <<"\"divisor\":2">>], Json2)).

using_existing_atoms(_) ->
    % The ID and method are matching existing atoms, checks they are not atoms
    Term = {request, <<"notification">>, #{}, <<"request">>},
    Json = <<"{\"id\":\"request\",\"jsonrpc\":\"2.0\",\"method\":\"notification\",\"params\":{}}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    Json2 = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":\"request\"">>, <<"\"method\":\"notification\"">>], Json2)).

notification(_) ->
    Term = {notification, <<"update">>, [1,2,3,4,5]},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    Json2 = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"method\":\"update\"">>,
                           <<"\"params\":[1,2,3,4,5]">>], Json2)).

invalid_json(_) ->
    Term = {decoding_error, -32700, <<"Parse error">>, undefined, undefined},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"foobar,\"params\":\"bar\",\"baz]">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    JsonError = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"error\":{">>,
                            <<"\"code\":-32700">>,
                            <<"\"message\":\"Parse error\"">>,
                            <<"\"id\":null">>], JsonError)).

invalid_request(_) ->
    Term = {decoding_error, -32600, <<"Invalid request">>, undefined, undefined},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":1,\"params\":\"bar\"}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    JsonError = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"error\":{">>,
                           <<"\"code\":-32600">>,
                           <<"\"message\":\"Invalid request\"">>,
                           <<"\"id\":null">>], JsonError)).

batch(_) ->
    Term1 = {request, <<"sum">>, [1,2,4], <<"1">>},
    Term2 = {decoding_error, -32600, <<"Invalid request">>, undefined, undefined},
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4],\"id\":\"1\"},{\"foo\":\"boo\"}]">>,
    ?assertMatch([Term1, Term2], grisp_connect_jsonrpc:decode(Json)),
    JsonError = grisp_connect_jsonrpc:encode([Term1, Term2]),
    ?assert(jsonrpc_check([<<"\"id\":\"1\"">>,
                           <<"\"method\":\"sum\"">>,
                           <<"\"params\":[1,2,4]">>,
                           <<"\"error\":{">>,
                           <<"\"code\":-32600">>,
                           <<"\"message\":\"Invalid request\"">>,
                           <<"\"id\":null">>], JsonError)).

result(_) ->
    Term = {result, 7, 45},
    Json = <<"{\"id\":45,\"jsonrpc\":\"2.0\",\"result\":7}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    Json2 = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":45">>,
                           <<"\"result\":7">>], Json2)).

null_values(_) ->
    Term = {notification, <<"test_null">>, #{array => [undefined], object => #{foo => undefined}, value => undefined}},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test_null\",\"params\":{\"array\":[null],\"object\":{\"foo\":null},\"value\":null}}">>,
    ?assertMatch([Term], grisp_connect_jsonrpc:decode(Json)),
    Json2 = grisp_connect_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"array\":[null]">>,
                           <<"\"foo\":null">>,
                           <<"\"value\":null">>],
                          Json2)).

jsonrpc_check(Elements, JsonString) ->
    Elements2 = [<<"\"jsonrpc\":\"2.0\"">>| Elements],
    lists:all(fun(E) -> binary:match(JsonString, E) =/= nomatch end, Elements2).

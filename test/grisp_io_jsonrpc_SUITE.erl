-module(grisp_io_jsonrpc_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([positional_parameters/1,
        named_parameters/1,
        notification/1,
        invalid_json/1,
        invalid_request/1,
        batch/1,
        result/1]).

all() -> [
    positional_parameters,
    named_parameters,
    notification,
    invalid_json,
    invalid_request,
    batch,
    result
].

positional_parameters(_) ->
    Term = {request, <<"subtract">>, [42,23], 1},
    Json = <<"{\"id\":1,\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23]}">>,
    ?assertMatch({single, Term}, grisp_io_jsonrpc:decode(Json)),
    Json = grisp_io_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":1">>,
                           <<"\"method\":\"subtract\"">>,
                           <<"\"params\":[42,23]">>], Json)).

named_parameters(_) ->
    Term = {request, <<"subtract">>, #{<<"subtrahend">> => 23, <<"minuend">> => 42}, 2},
    Json = <<"{\"id\":2,\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":{\"minuend\":42,\"subtrahend\":23}}">>,
    ?assertMatch({single, Term}, grisp_io_jsonrpc:decode(Json)),
    Json = grisp_io_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":2">>,
                           <<"\"method\":\"subtract\"">>,
                           <<"\"minuend\":42">>,
                           <<"\"subtrahend\":23">>], Json)).

notification(_) ->
    Term = {notification, <<"update">>, [1,2,3,4,5]},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}">>,
    ?assertMatch({single, Term}, grisp_io_jsonrpc:decode(Json)),
    Json = grisp_io_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"method\":\"update\"">>,
                           <<"\"params\":[1,2,3,4,5]">>], Json)).

invalid_json(_) ->
    Term = {internal_error, parse_error, null},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"foobar,\"params\":\"bar\",\"baz]">>,
    ?assertMatch({single, Term}, grisp_io_jsonrpc:decode(Json)),
    JsonError = grisp_io_jsonrpc:encode(grisp_io_jsonrpc:format_error(Term)),
    ?assert(jsonrpc_check([<<"\"error\":{">>,
                            <<"\"code\":-32700">>,
                            <<"\"message\":\"Parse error\"">>,
                            <<"\"id\":null">>], JsonError)).

invalid_request(_) ->
    Term = {internal_error, invalid_request, null},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":1,\"params\":\"bar\"}">>,
    ?assertMatch({single, Term}, grisp_io_jsonrpc:decode(Json)),
    JsonError = grisp_io_jsonrpc:encode(grisp_io_jsonrpc:format_error(Term)),
    ?assert(jsonrpc_check([<<"\"error\":{">>,
                           <<"\"code\":-32600">>,
                           <<"\"message\":\"Invalid request\"">>,
                           <<"\"id\":null">>], JsonError)).

batch(_) ->
    Term1 = {request, <<"sum">>, [1,2,4], <<"1">>},
    Term2 = {internal_error, invalid_request, null},
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4],\"id\":\"1\"},{\"foo\":\"boo\"}]">>,
    ?assertMatch({batch, [Term1,Term2]}, grisp_io_jsonrpc:decode(Json)),
    JsonError = grisp_io_jsonrpc:encode([Term1, grisp_io_jsonrpc:format_error(Term2)]),
    ?assert(jsonrpc_check([<<"\"id\":\"1\"">>,
                           <<"\"method\":\"sum\"">>,
                           <<"params\":[1,2,4]">>,
                           <<"\"error\":{">>,
                           <<"\"code\":-32600">>,
                           <<"\"message\":\"Invalid request\"">>,
                           <<"\"id\":null">>], JsonError)).

result(_) ->
    Term = {result, 7, 45},
    Json = <<"{\"id\":45,\"jsonrpc\":\"2.0\",\"result\":7}">>,
    ?assertMatch({single, Term}, grisp_io_jsonrpc:decode(Json)),
    Json = grisp_io_jsonrpc:encode(Term),
    ?assert(jsonrpc_check([<<"\"id\":45">>,
                           <<"\"result\":7">>], Json)).


jsonrpc_check(Elements, JsonString) ->
    Elements2 = [<<"\"jsonrpc\":\"2.0\"">>| Elements],
    lists:all(fun(E) -> binary:match(JsonString, E) =/= nomatch end, Elements2).

-module(jsonrpc_SUITE).

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
    ?assertMatch(Json, grisp_seawater_jsonrpc:encode(Term)),
    ?assertMatch({single, Term}, grisp_seawater_jsonrpc:decode(Json)).

named_parameters(_) ->
    Term = {request, <<"subtract">>, #{<<"subtrahend">> => 23, <<"minuend">> => 42}, 2},
    Json = <<"{\"id\":2,\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":{\"minuend\":42,\"subtrahend\":23}}">>,
    ?assertMatch(Json, grisp_seawater_jsonrpc:encode(Term)),
    ?assertMatch({single, Term}, grisp_seawater_jsonrpc:decode(Json)).

notification(_) ->
    Term = {notification, <<"update">>, [1,2,3,4,5]},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2,3,4,5]}">>,
    ?assertMatch(Json, grisp_seawater_jsonrpc:encode(Term)),
    ?assertMatch({single, Term}, grisp_seawater_jsonrpc:decode(Json)).

invalid_json(_) ->
    Term = {internal_error, parse_error, null},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"foobar,\"params\":\"bar\",\"baz]">>,
    ?assertMatch({single, Term}, grisp_seawater_jsonrpc:decode(Json)),
    JsonError = <<"{\"error\":{\"code\":-32700,\"message\":\"Parse error\"},\"id\":null,\"jsonrpc\":\"2.0\"}">>,
    ?assertMatch(JsonError, grisp_seawater_jsonrpc:encode(grisp_seawater_jsonrpc:format_error(Term))).

invalid_request(_) ->
    Term = {internal_error, invalid_request, null},
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":1,\"params\":\"bar\"}">>,
    ?assertMatch({single, Term}, grisp_seawater_jsonrpc:decode(Json)),
    JsonError = <<"{\"error\":{\"code\":-32600,\"message\":\"Invalid request\"},\"id\":null,\"jsonrpc\":\"2.0\"}">>,
    ?assertMatch(JsonError, grisp_seawater_jsonrpc:encode(grisp_seawater_jsonrpc:format_error(Term))).

batch(_) ->
    Term1 = {request, <<"sum">>, [1,2,4], <<"1">>},
    Term2 = {internal_error, invalid_request, null},
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4],\"id\":\"1\"},{\"foo\":\"boo\"}]">>,
    ?assertMatch({batch, [Term1,Term2]}, grisp_seawater_jsonrpc:decode(Json)),
    JsonError = <<"[{\"id\":\"1\",\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\":[1,2,4]},{\"error\":{\"code\":-32600,\"message\":\"Invalid request\"},\"id\":null,\"jsonrpc\":\"2.0\"}]">>,
    ?assertMatch(JsonError, grisp_seawater_jsonrpc:encode([Term1,grisp_seawater_jsonrpc:format_error(Term2)])).

result(_) ->
    Term = {result, 7, 45},
    Json = <<"{\"id\":45,\"jsonrpc\":\"2.0\",\"result\":7}">>,
    ?assertMatch(Json, grisp_seawater_jsonrpc:encode(Term)),
    ?assertMatch({single, Term}, grisp_seawater_jsonrpc:decode(Json)).

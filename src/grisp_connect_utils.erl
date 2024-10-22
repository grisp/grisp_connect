-module(grisp_connect_utils).

% API Functions
-export([as_bin/1]).
-export([maybe_atom/1]).
-export([parse_method/1]).
-export([format_method/1]).


%--- API Functions -------------------------------------------------------------

as_bin(Binary) when is_binary(Binary) -> Binary;
as_bin(List) when is_list(List) -> list_to_binary(List);
as_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom).

maybe_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin)
    catch error:badarg -> Bin
    end.

parse_method(List) when is_list(List) -> check_method(List);
parse_method(Atom) when is_atom(Atom) -> [Atom];
parse_method(Binary) when is_binary(Binary) ->
    [maybe_atom(B) || B <- binary:split(Binary, <<".">>, [global])].

format_method(Binary) when is_binary(Binary) -> Binary;
format_method(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
format_method(List) when is_list(List) ->
    iolist_to_binary(lists:join(<<".">>, [as_bin(E) || E <- List])).


%--- Internal Functions --------------------------------------------------------

check_method(Method) when is_list(Method) ->
    check_method(Method, Method);
check_method(_Method) ->
    erlang:exit(badarg).

check_method(Method, []) -> Method;
check_method(Method, [Atom | Rest]) when is_atom(Atom) ->
    check_method(Method, Rest);
check_method(Method, [Bin | Rest]) when is_binary(Bin) ->
    check_method(Method, Rest);
check_method(_Method, _Rest) ->
    erlang:error(badarg).

%%%-----------------------------------------------------------------------------
%% @doc GRiSP.io High Level API.
%% @end
%%%-----------------------------------------------------------------------------

-module(grisp_connect).

-include_lib("kernel/include/logger.hrl").

%--- Exports -------------------------------------------------------------------

% API functions
-export([connect/0]).
-export([is_connected/0]).
-export([ping/0]).
-export([link_device/0]).
-export([link_device/1]).
-export([log/2]).
-export([test_log_encoding/0]).

%--- API Functions -------------------------------------------------------------

% @doc Connect to GRiSP.io.
-spec connect() -> ok.
connect() ->
    grisp_connect_client:connect().

% @doc Check if board is connected to GRiSP.io.
-spec is_connected() -> true | false.
is_connected() ->
    grisp_connect_client:is_connected().

% @doc Ping GRiSP.io.
% Returns 'pong' if the board is linked to an account, 'pang' otherwise.
-spec ping() -> {ok, binary()} | {error, atom()}.
ping() ->
    grisp_connect_client:request(post, ping, #{}).

% @doc Links the board to a GRiSP.io account.
% The token is took from the device_linking_token app env.
-spec link_device() -> {ok, binary()} | {error, token_undefined | invalid_token}.
link_device() ->
    case application:get_env(grisp_connect, device_linking_token) of
        undefined -> {error, token_undefined};
        {ok, Token} -> link_device(Token)
    end.

% @doc Links the board to the GRiSP.io account
% that generated of the specified token.
-spec link_device(Token :: binary()) ->
    {ok, binary()} | {error, invalid_token}.
link_device(Token) ->
    grisp_connect_client:request(post, device_linking_token, #{token => Token}).

% @doc Log from inside grisp_connect for testing.
log(Level, Args) -> apply(logger, Level, Args).

% @doc For manually testing the log encoding.
% Alternative to shell commands. The shell double encodes unicode strings.
test_log_encoding() ->
    String = "@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ",
    Binary = <<"@#$%^&*()_ +{}|:\"<>?-[];'./,\\`~!\néäüßóçøáîùêñÄÖÜÉÁÍÓÚàèìòùÂÊÎÔÛ"/utf8>>,
    ?LOG_NOTICE("List:\n" ++ String),
    ?LOG_NOTICE(<<"Binary: \n", Binary/binary>>),
    ?LOG_NOTICE("List:~n~tp~nBinary:~n~tp~n", [String, Binary]),
    ?LOG_NOTICE(#{list => String, binary => Binary}).

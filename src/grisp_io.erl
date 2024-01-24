%%%-----------------------------------------------------------------------------
%% @doc GRiSP.io High Level API.
%% @end
%%%-----------------------------------------------------------------------------

-module(grisp_io).

%--- Exports -------------------------------------------------------------------

% API functions
-export([ping/0]).
-export([link_device/0]).
-export([link_device/1]).

%--- API Functions -------------------------------------------------------------

% @doc Ping GRiSP.io.
% Returns 'pong' if the board is linked to an account, 'pang' otherwise.
-spec ping() -> {ok, binary()} | {error, atom()}.
ping() ->
    grisp_io_client:request(post, ping, #{}).


% @doc Links the board to a GRiSP.io account.
% The token is took from the device_linking_token app env.
-spec link_device() -> {ok, binary()} | {error, token_undefined | invalid_token}.
link_device() ->
    case application:get_env(grisp_io, device_linking_token) of
        undefined -> {error, token_undefined};
        {ok, Token} -> link_device(Token)
    end.

% @doc Links the board to a GRiSP.io account owning of the specified token.
-spec link_device(Token :: binary()) ->
    {ok, binary()} | {error, invalid_token}.
link_device(Token) ->
    grisp_io_client:request(post, device_linking_token, #{token => Token}).

%--- Internal Functions --------------------------------------------------------

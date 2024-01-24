%%%-------------------------------------------------------------------
%% @doc GRiSP.io High Level API.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_io).

%--- Exports -------------------------------------------------------------------

% API functions
-export([ping/0]).
-export([link_device/0]).
-export([link_device/1]).

%--- API Functions -------------------------------------------------------------

ping() ->
    grisp_io_client:request(post, ping, #{}).

link_device() ->
    case application:get_env(grisp_io, device_linking_token) of
        undefined -> {error, token_undefined};
        {ok, Token} -> link_device(Token)
    end.

link_device(Token) ->
    grisp_io_client:request(post, device_linking_token, #{token => Token}).

%--- Internal Functions --------------------------------------------------------

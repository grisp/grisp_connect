%%%-------------------------------------------------------------------
%% @doc grisp_connect public API
%% @end
%%%-------------------------------------------------------------------

-module(grisp_connect_app).

-behaviour(application).


%--- Exports -------------------------------------------------------------------

% Behaviour application callback functions
-export([start/2, stop/1]).


%--- Behaviour application Callback Functions ----------------------------------

start(_StartType, _StartArgs) ->
    logger:add_handlers(grisp_connect),
    grisp_connect_sup:start_link().

stop(_State) ->
    ok.

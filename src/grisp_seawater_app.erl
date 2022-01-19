%%%-------------------------------------------------------------------
%% @doc grisp_seawater public API
%% @end
%%%-------------------------------------------------------------------

-module(grisp_seawater_app).

-behaviour(application).


%--- Exports -------------------------------------------------------------------

% Behaviour application callback functions
-export([start/2, stop/1]).


%--- Behaviour application Callback Functions ----------------------------------

start(_StartType, _StartArgs) ->
    grisp_seawater_sup:start_link().

stop(_State) ->
    ok.

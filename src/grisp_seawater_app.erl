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
    grisp_rtems:clock_set({{{2022, 1, 18}, {12, 09, 42}}, 0}),
    grisp_seawater_sup:start_link().

stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%% @doc grisp_io public API
%% @end
%%%-------------------------------------------------------------------

-module(grisp_io_app).

-behaviour(application).


%--- Exports -------------------------------------------------------------------

% Behaviour application callback functions
-export([start/2, stop/1]).


%--- Behaviour application Callback Functions ----------------------------------

start(_StartType, _StartArgs) ->
    logger:add_handlers(grisp_io),
    grisp_io_sup:start_link().

stop(_State) ->
    ok.

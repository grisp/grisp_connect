%%%-----------------------------------------------------------------------------
%% @doc grisp_io app module
%% @end
%%%-----------------------------------------------------------------------------

-module(grisp_io_app).

-behaviour(application).

%--- Exports -------------------------------------------------------------------

% Behaviour application callback functions
-export([start/2, stop/1]).


%--- Behaviour application Callback Functions ----------------------------------

start(_StartType, _StartArgs) ->
    grisp_io_sup:start_link().

stop(_State) ->
    ok.

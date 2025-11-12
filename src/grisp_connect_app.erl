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
    case grisp_connect_utils:using_grisp_netman() of
        true ->
            grisp_netman:add_event_handler(grisp_connect_netman_handler, []);
        false ->
            ok
    end,
    grisp_connect_sup:start_link().

stop(_State) ->
    case grisp_connect_utils:using_grisp_netman() of
        true ->
            grisp_netman:delete_event_handler(grisp_connect_netman_handler, []);
        false ->
            ok
    end.

%--- Internal Functions --------------------------------------------------------

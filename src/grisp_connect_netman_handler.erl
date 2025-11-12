-module(grisp_connect_netman_handler).
-moduledoc """
Handler for grisp_netman events.
""".

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2]).

-include_lib("kernel/include/logger.hrl").

% Behaviour gen_event callbacks ------------------------------------------------

init(_) ->
    {ok, #{}}.

handle_event({connection_status, _IfName, Status}, State) ->
    case Status of
        S when S =:= lan orelse S =:= internet ->
            grisp_connect_client:lan_connected();
        disconnected ->
            % If one interface went down,
            % check if the global connectivity is still maintained
            % by any other interface
            case grisp_netman:connection_status() of
                S when S =:= lan orelse S =:= internet ->
                    ok;
                disconnected ->
                    grisp_connect_client:lan_disconnected()
            end
    end,
    {ok, State}.

handle_call(Request, State) ->
    ?LOG_WARNING("Unexpected ~p call: ~p", [?MODULE, Request]),
    {reply, unexpected_call, State}.

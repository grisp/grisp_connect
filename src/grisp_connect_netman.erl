-module(grisp_connect_netman).

-behaviour(gen_event).

-export([add_handler/0, remove_handler/0]).
-export([init/1, handle_event/2, handle_call/2]).

-include_lib("kernel/include/logger.hrl").

add_handler() ->
    gen_event:add_handler(grisp_netman_event, ?MODULE, []).

remove_handler() ->
    gen_event:delete_handler(grisp_netman_event, ?MODULE, []).

% Behaviour gen_event callbacks ------------------------------------------------

init([]) ->
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
    ?LOG_WARNING("Unexpected grisp_netman_event call: ~p", [Request]),
    {reply, unexpected_call, State}.

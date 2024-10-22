-module(grisp_connect_feature_system).

-behaviour(grisp_connect_feature).

-include_lib("kernel/include/logger.hrl").

% Behaviour grisp_connect_feature Callbacks
-export([namespace/0]).
-export([init/2]).
-export([on_request/5]).


%--- Types ---------------------------------------------------------------------

-record(state, {}).


%--- Behaviour grisp_connect_feature Callbacks ---------------------------------

namespace() -> system.

init(_Client, _Opts) ->
    {ok, [], #state{}}.

on_request(Client, State, ['GetInfo'], #{}, ReqRef) ->
    grisp_connect_feature:reply(Client, #{}, ReqRef),
    {ok, State};
on_request(Client, State, ['Reboot'], #{}, ReqRef) ->
    grisp_connect_feature:reply(Client, ok, ReqRef),
    {ok, State}.

-module(grisp_connect_feature_update).

-behaviour(grisp_connect_feature).

-include_lib("kernel/include/logger.hrl").

% Behaviour grisp_connect_feature Callbacks
-export([namespace/0]).
-export([init/2]).
-export([on_request/5]).


%--- Types ---------------------------------------------------------------------

-record(state, {}).


%--- Behaviour grisp_connect_feature Callbacks ---------------------------------

namespace() -> update.

init(_Client, _Opts) ->
    {ok, [], #state{}}.

on_request(Client, State, ['Start'], #{}, ReqRef) ->
    grisp_connect_feature:error(Client, not_implemented, ReqRef),
    {ok, State}.

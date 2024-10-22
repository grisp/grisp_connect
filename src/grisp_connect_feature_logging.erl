-module(grisp_connect_feature_logging).

-behaviour(grisp_connect_feature).

-include_lib("kernel/include/logger.hrl").

% Behaviour grisp_connect_feature Callbacks
-export([namespace/0]).
-export([init/2]).
-export([on_request/5]).


%--- Types ---------------------------------------------------------------------

-record(state, {}).


%--- Behaviour grisp_connect_feature Callbacks ---------------------------------

% grisp_connect_log_server:start(),
% grisp_connect_log_server:stop(),

namespace() -> logging.

init(_Client, _Opts) ->
    {ok, [], #state{}}.

on_request(Client, State, ['Start'], #{}, ReqRef) ->
    grisp_connect_feature:error(Client, not_implemented, ReqRef),
    {ok, State};
on_request(Client, State, ['Stop'], #{}, ReqRef) ->
    grisp_connect_feature:error(Client, not_implemented, ReqRef),
    {ok, State}.

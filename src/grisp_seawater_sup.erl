%%%-------------------------------------------------------------------
%% @doc grisp_seawater top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_seawater_sup).

-behaviour(supervisor).


%--- Exports -------------------------------------------------------------------

% API Functions
-export([start_link/0]).

% Behaviour supervisor Callback Functions
-export([init/1]).


%--- Macros --------------------------------------------------------------------

-define(SERVER, ?MODULE).


%--- API Functions -------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%--- Behaviour supervisor Callback Functions -----------------------------------

init([]) ->
    SupFlags = #{
        strategy => one_for_all
    },
    ChildSpecs = [
        worker(grisp_seawater_ntp, []),
        worker(grisp_seawater_client, [])],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Module, Args) ->
    #{id => Module, start => {Module, start_link, Args}, type => worker}.
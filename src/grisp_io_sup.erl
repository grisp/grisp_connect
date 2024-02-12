%%%-------------------------------------------------------------------
%% @doc grisp_io top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_io_sup).

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
    NTP = case application:get_env(grisp_io, ntp) of
        {ok, true} -> [worker(grisp_io_ntp, [])];
        {ok, false} -> []
    end,
    ChildSpecs = NTP ++ [
        worker(grisp_io_ws, []),
        worker(grisp_io_connection, [])
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Module, Args) ->
    #{id => Module, start => {Module, start_link, Args}, type => worker}.
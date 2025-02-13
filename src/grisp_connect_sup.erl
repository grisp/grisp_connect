%%%-------------------------------------------------------------------
%% @doc grisp_connect top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_connect_sup).

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
    NTP = case application:get_env(grisp_connect, ntp) of
        {ok, true} -> [worker(grisp_connect_ntp, [])];
        {ok, false} -> []
    end,
%% Notes:
%% grisp_connect_log_server is required to be running by grisp_connect_client
%% that starts and stops the logging loop in grisp_connect_log_server asynchronous.
%% Hence grisp_connect_log_server should be started before grisp_connect_client
%% and a crash in grisp_connect_log_server should crash grisp_connect_client as well.
    ChildSpecs = NTP ++ [
        worker(grisp_connect_log_server, []),
        worker(grisp_connect_client, [])
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Module, Args) ->
    #{id => Module, start => {Module, start_link, Args}, type => worker}.

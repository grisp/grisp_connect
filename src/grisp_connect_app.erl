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
    ensure_trusted_server_certs_are_set(),
    grisp_connect_sup:start_link().

stop(_State) ->
    ok.

%--- Internal Functions --------------------------------------------------------

ensure_trusted_server_certs_are_set() ->
    case application:get_env(grisp_cryptoauth, tls_server_trusted_certs_cb) of
        {ok, _} ->
            ok;
        undefined ->
            Certifi = {certifi, cacerts, []},
            application:set_env(grisp_cryptoauth,
                                tls_server_trusted_certs_cb,
                                Certifi)
    end.

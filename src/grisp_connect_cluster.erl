-module(grisp_connect_cluster).

%--- Includes -------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").


%--- Exports -------------------------------------------------------------------

% API Functions
-export([init/0]).
-export([join/6]).
-export([is_allowed/1]).

%--- Macros --------------------------------------------------------------------

-define(FINGERPRINT_TABLE, grisp_connect_cluster_fingerprints).


%--- API FUNCTIONS -------------------------------------------------------------

init() ->
    ets:new(?FINGERPRINT_TABLE, [named_table, public, set, {keypos, 1}]),
    ok.

join(Node, Cookie, CAPem, CertFingerprint, Hostname, Address) ->
    ok = cluster_setup(Node, Cookie, CAPem, Hostname, Address),
    ets:insert(?FINGERPRINT_TABLE, {CertFingerprint, Node}),
    cluster_join(Node).

is_allowed(CertFingerprint) ->
    case ets:lookup(?FINGERPRINT_TABLE, CertFingerprint) of
        [] -> false;
        [_] -> true
    end.


%--- Internal Funcitons --------------------------------------------------------

cluster_setup(Node, Cookie, CAPem, Hostname, Address) ->
    ?LOG_NOTICE("Registering cluster with node ~w", [Node]),
    % Overwrite any other CA for now
    ok = file:write_file("/etc/cluster.CA.pem", CAPem),
    ok = inet_db:add_host(Address, [binary_to_list(Hostname)]),
    true = erlang:set_cookie(Node, Cookie),
    ok.

cluster_join(Node) ->
    case net_adm:ping(Node) of
        pong ->
            ?LOG_NOTICE("Joined cluster with node ~w", [Node]),
            true;
        pang ->
            ?LOG_NOTICE("Failed to join cluster with node ~w", [Node]),
            false
    end.

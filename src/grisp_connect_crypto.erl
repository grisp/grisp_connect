%% @doc Cryptography Helper Functions
%% @end
-module(grisp_connect_crypto).

-include_lib("kernel/include/logger.hrl").


%--- Exports -------------------------------------------------------------------

% API functions
-export([verify_server/3]).
-export([skip_cert_expired/3]).


%--- API Functions -------------------------------------------------------------

-doc """
Function used as `verify_fun` when connecting to other nodes with erlang distribution.
It ensures that the peer certificate was previously allowed in the cluster.
""".
verify_server(_OtpCert, {bad_cert, _} = Reason, _State) ->
    {fail, Reason};
verify_server(_OtpCert, {extension, _}, State) ->
    {unknown, State};
verify_server(OtpCert, _Event, _State) ->
    CertBinary = public_key:pkix_encode('OTPCertificate', OtpCert, 'otp'),
    Hash = crypto:hash(sha256, CertBinary),
    case grisp_connect_cluster:is_allowed(Hash) of
        true -> {valid, Hash};
        false -> {fail, not_allowed}
    end.

-doc """
Identical to the default verify_fun, but ignores the cert_expired failure.
""".
skip_cert_expired(_, {bad_cert, cert_expired}, UserState) ->
    {valid, UserState};
skip_cert_expired(_, {bad_cert, _} = Reason, _) ->
    {fail, Reason};
skip_cert_expired(_, {extension, _}, UserState) ->
    {unknown, UserState};
skip_cert_expired(_, valid, UserState) ->
    {valid, UserState};
skip_cert_expired(_, valid_peer, UserState) ->
    {valid, UserState}.

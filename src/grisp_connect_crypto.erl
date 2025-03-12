%% @doc Cryptography Helper Functions
%% @end
-module(grisp_connect_crypto).

-include_lib("kernel/include/logger.hrl").


%--- Exports -------------------------------------------------------------------

% API functions
-export([verify_server/3]).


%--- API Functions -------------------------------------------------------------

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

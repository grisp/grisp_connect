-module(grisp_seawater_cert).

-export([grisp2_device/0,
         grisp2_device/1]).


grisp2_device() ->
    grisp2_device(undefined).


%% GRiSP2 device certificate;
%% callback for grisp_cryptoauth
grisp2_device(Context) ->
    IssuerCertFile = code:priv_dir(grisp_seawater) ++ "/client/grisp2_ca.pem",
    {ok, IssuerCertPEM} = file:read_file(IssuerCertFile),
    IssuerCert = grisp_cryptoauth_cert:decode_pem(IssuerCertPEM, plain),
    Validity = {{{2021,9,1}, {0,0,0}}, no_expiration},
    {ok, DERPubKey} = grisp_cryptoauth:public_key(Context, primary),
    {ok, GrispMeta} = grisp_hw:eeprom_read(),
    %% The device serial is also used for the certificate serial, simply
    %% because it is unique integer. If we want to use other serials in
    %% the future then that's ok, there's no requirement on such a
    %% mapping between both serials.
    Serial = maps:get(grisp_serial, GrispMeta),
    Subject = #{
        'CN' => "GRiSP2 device " ++ integer_to_list(Serial),
        'O'  => "Dipl.Phys. Peer Stritzinger GmbH",
        'OU' => "www.grisp.org",
        emailAddress => "grisp@stritzinger.com"
    },
    grisp_cryptoauth_profile:tls_client(IssuerCert, Validity, Subject, DERPubKey, GrispMeta).

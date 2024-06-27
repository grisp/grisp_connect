%%%-------------------------------------------------------------------
%% @doc Grisp Seawater Low Level HTTPS Setup.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_connect_tls).


%--- Exports -------------------------------------------------------------------

% API functions
-export([connect/2]).


%--- API Functions -------------------------------------------------------------

connect(ServerName, Port) ->
    case ssl_opts(ServerName) of
        {error, _Reason} = Error -> Error;
        {ok, TransOpts} ->
            GunOpts = #{
                    protocols => [http],
                    transport => tls,
                    tls_opts => TransOpts},
            gun:open(ServerName, Port, GunOpts)
    end.

%--- Internal Functions --------------------------------------------------------

-ifdef(TEST).

ssl_opts(_) ->
    {ok, CertDir} = application:get_env(grisp_connect, test_cert_dir),
    Priv = code:priv_dir(grisp_connect),
    {ok, [
        {verify, verify_none},
        {keyfile, filename:join(CertDir, "client.key")},
        {certfile, filename:join(CertDir, "client.crt")}
    ]}.

-else.

-if(?OTP_RELEASE >= 27).

ssl_opts(ServerName) ->
    case client_chain() of
        {error, _Reason} = Error -> Error;
        {ok, ClientChain} ->
            {ok, [
                {verify, verify_peer},
                {depth, 99},
                {cacerts, certifi:cacerts() ++ server_chain(ServerName)},
                {customize_hostname_check, [
                    {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                ]},
                {certs_keys, [#{
                    cert => ClientChain,
                    key => #{
                        algorithm => ecdsa,
                        sign_fun => fun grisp_cryptoauth:sign_fun/3
                    }
                }]}
            ]}
    end.

-else. % ?OTP_RELEASE < 27

ssl_opts(ServerName) ->
    case client_chain() of
        {error, _Reason} = Error -> Error;
        {ok, ClientChain} ->
            {ok, [
                {verify, verify_peer},
                {depth, 99},
                {cacerts, certifi:cacerts() ++ server_chain(ServerName)},
                {cert, ClientChain},
                {customize_hostname_check, [
                    {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                ]},
                {key, #{
                    algorithm => ecdsa,
                    sign_fun => {grisp_cryptoauth, sign_fun}
                }}
            ]}
    end.

-endif.

-endif.

client_chain() ->
    ClientCert = grisp_cryptoauth:read_cert(primary, der),
    {ok, IssuerId} = public_key:pkix_issuer_id(ClientCert, self),
    case client_chain_issuer(IssuerId) of
        {error, _Reason} = Error -> Error;
        {ok, Chain} -> {ok, [ClientCert | Chain]}
    end.

server_chain(ServerName) ->
    case load_cert_chain(["server", ServerName]) of
        {error, _} -> [];
        {ok, List} -> List
    end.

client_chain_issuer({Serial, _}) when Serial >= 1000 ->
    load_cert_chain(["client", "batch_001"]).

load_cert_chain(RelPath) ->
    FilePath = cert_path(RelPath),
    case file:read_file(FilePath) of
        {error, enoent} ->
            {error, {file_not_found, FilePath}};
        {ok, PemData} ->
            decode_cert_chain(PemData)
    end.

cert_path(RelPath) ->
    PrivDir = code:priv_dir(grisp_connect),
    filename:join([PrivDir | RelPath]) ++ ".pem".

decode_cert_chain(PemData) ->
    decode_cert_chain(public_key:pem_decode(PemData), []).

decode_cert_chain([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_cert_chain([{'Certificate', D, not_encrypted} | Rest], Acc) ->
    decode_cert_chain(Rest, [D | Acc]);
decode_cert_chain([Bad | _Rest], _Acc) ->
    {error, {bad_certificate, Bad}}.

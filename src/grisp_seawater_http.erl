%%%-------------------------------------------------------------------
%% @doc Grisp Seawater Low Level HTTP API.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_seawater_http).


%--- Exports -------------------------------------------------------------------

% API functions
-export([open/2]).
-export([close/1]).
-export([get/2]).


%--- API Functions -------------------------------------------------------------

open(ServerName, Port) ->
    case ssl_opts(ServerName) of
        {error, _Reason} = Error -> Error;
        {ok, TransOpts} ->
            GunOpts = #{
                    protocols => [http],
                    transport => tls,
                    tls_opts => TransOpts},
            case gun:open(ServerName, Port, GunOpts) of
                {error, _Reason} = Error -> Error;
                {ok, Conn} ->
                    erlang:link(Conn),
                    case gun:await_up(Conn) of
                        {error, _Reason} = Error -> Error;
                        {ok, _} -> {ok, Conn}
                    end
            end
    end.

close(Conn) ->
    erlang:unlink(Conn),
    gun:shutdown(Conn),
    ok.

get(Conn, Path) ->
    StreamRef = gun:get(Conn, Path),
    case gun:await(Conn, StreamRef) of
        {error, _Reason} = Error -> Error;
        {response, fin, Status, _Headers} ->
            {ok, Status, undefined};
        {response, nofin, Status, _Headers} ->
            case gun:await_body(Conn, StreamRef) of
                {error, _Reason} = Error -> Error;
                {ok, Body} -> {ok, Status, Body}
            end
    end.


%--- Internal Functions --------------------------------------------------------

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
    PrivDir = code:priv_dir(grisp_seawater),
    filename:join([PrivDir | RelPath]) ++ ".pem".

decode_cert_chain(PemData) ->
    decode_cert_chain(public_key:pem_decode(PemData), []).

decode_cert_chain([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_cert_chain([{'Certificate', D, not_encrypted} | Rest], Acc) ->
    decode_cert_chain(Rest, [D | Acc]);
decode_cert_chain([Bad | _Rest], _Acc) ->
    {error, {bad_certificate, Bad}}.
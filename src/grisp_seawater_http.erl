%%%-------------------------------------------------------------------
%% @doc Grisp Seawater Low Level HTTP API.
%% @end
%%%-------------------------------------------------------------------

-module(grisp_seawater_http).


%--- Exports -------------------------------------------------------------------

% API functions
-export([open/0, open/2]).
-export([close/1]).
-export([get/2]).


%--- Records -------------------------------------------------------------------

-record(?MODULE, {conn}).


%--- API Functions -------------------------------------------------------------

open() -> open("seawater.stritzinger.com", 443).

open(ServerName, Port) ->
    case ssl_opts(ServerName) of
        {error, _Reason} = Error -> Error;
        {ok, TransOpts} ->
            GunOpts = #{transport => tls, transport_opts => TransOpts},
            case gun:open(ServerName, Port, GunOpts) of
                {error, _Reason} = Error -> Error;
                {ok, Conn} ->
                    erlang:link(Conn),
                    case gun:await_up(Conn) of
                        {error, _Reason} = Error -> Error;
                        {ok, _} -> {ok, #?MODULE{conn = Conn}}
                    end
            end
    end.

close(#?MODULE{conn = Conn}) ->
    erlang:unlink(Conn),
    gun:shutdown(Conn),
    ok.

get(#?MODULE{conn = Conn}, Path) ->
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
            case server_chain(ServerName) of
                {error, _Reason} = Error -> Error;
                {ok, ServerChain} ->
                    {ok, [
                        {server_name_indication, ServerName},
                        {verify, verify_peer},
                        {cacerts, ServerChain},
                        {cert, ClientChain},
                        {key, #{
                            algorithm => ecdsa,
                            sign_fun => {grisp_cryptoauth, sign_fun}
                        }}
                    ]}
            end
    end.

server_chain(ServerName) ->
    load_cert_chain(["server", ServerName]).

client_chain() ->
    Templates = [{{2, 0}, {grisp_seawater_cert, grisp2_device}}],
    ClientCert = grisp_cryptoauth:read_cert(primary, Templates, der),
    {ok, IssuerId} = public_key:pkix_issuer_id(ClientCert, self),
    case client_chain_issuer(IssuerId) of
        {error, _Reason} = Error -> Error;
        {ok, Chain} -> {ok, [ClientCert | Chain]}
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

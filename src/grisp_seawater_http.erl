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
    GunOpts = #{transport => tls,
                transport_opts => ssl_opts(ServerName)},
    case gun:open(ServerName, Port, GunOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Conn} ->
            erlang:link(Conn),
            case gun:await_up(Conn) of
                {error, _Reason} = Error -> Error;
                {ok, _} -> {ok, #?MODULE{conn = Conn}}
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
    Templates = [{{2, 0}, {grisp_seawater_cert, grisp2_device}}],
    Cert = grisp_cryptoauth:read_cert(primary, Templates, der),
    [
        {server_name_indication, ServerName},
        {verify, verify_peer},
        {cacerts, ca_certs()},
        {cert, Cert},
        {key, #{
            algorithm => ecdsa,
            sign_fun  => {grisp_cryptoauth, sign_fun}
        }}
    ].

ca_certs() ->
    Priv = code:priv_dir(grisp_seawater),
    StritzingerRoot = grisp_cryptoauth_cert:decode_pem_file(filename:join([Priv, "client", "stritzinger_root.pem"]), der),
    GRiSP2CA = grisp_cryptoauth_cert:decode_pem_file(filename:join([Priv, "client", "grisp2_ca.pem"]), der),
    SeawaterCA = grisp_cryptoauth_cert:decode_pem_file(filename:join([Priv, "server", "www.seawater.local.pem"]), der),
    [StritzingerRoot, GRiSP2CA, SeawaterCA].

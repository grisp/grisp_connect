-module(grisp_connect_inet).

% API Functions
-export([ipv4_external_address/0]).


%--- API Functions -------------------------------------------------------------

ipv4_external_address() ->
    case external_ipv4_ifaces() of
        [] -> undefined;
        [Iface | _] -> iface_ipv4_address(Iface)
    end.


%--- Internl Functions ---------------------------------------------------------

iface_ipv4_address({_Name, Opts}) ->
    case proplists:get_value(addr, Opts) of
        undefined -> undefined;
        Addr ->
            case inet:is_ipv4_address(Addr) of
                true -> Addr;
                false -> undefined
            end
    end.

iface_has_ipv4_address(Iface) ->
    iface_ipv4_address(Iface) =/= undefined.

iface_has_flag({_Name, Opts}, FlagName) ->
    case proplists:get_value(flags, Opts) of
        undefined -> false;
        Flags -> lists:member(FlagName, Flags)
    end.

iface_is_active(Iface) ->
    iface_has_flag(Iface, up) andalso iface_has_flag(Iface, running).

iface_is_loopback(Iface) ->
    case iface_ipv4_address(Iface) of
        {127, _, _, _} -> true;
        _ -> iface_has_flag(Iface, loopback)
    end.

external_ipv4_ifaces() ->
    case inet:getifaddrs() of
        {error, _Reason} -> [];
        {ok, Ifaces} ->
            [I || I <- Ifaces,
             iface_is_active(I)
             andalso not iface_is_loopback(I)
             andalso iface_has_ipv4_address(I)]
    end.

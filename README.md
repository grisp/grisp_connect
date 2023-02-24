grisp_seawater
==================

Grisp2 Seawater Client Library


Build
-----

    $ rebar3 compile


Basic Low Level API Usage
-------------------------

    {ok, Conn} = grisp_seawater_http:open().
    {ok, Status, Body} = grisp_seawater_http:get(Conn, "/grisp-connect").
    grisp_seawater_http:close(Conn).

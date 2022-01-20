grisp_seawater
==============

GRiSP2 Seawater Client Library


Build
-----

    $ rebar3 compile


Prerequisites
-------------

### Time


The Grisp board time must be set and correct for the certificate expiration
validation to succeed. This can be done with the command:

    grisp_rtems:clock_set({{{YYYY, MM, DD}, {HH, MM, SS}}, MICRO}).

e.g.

    grisp_rtems:clock_set({{{2022, 1, 18}, {12, 09, 42}}, 0}).


Basic Low Level API Usage
-------------------------

    {ok, Conn} = grisp_seawater_http:open().
    {ok, Status, Body} = grisp_seawater_http:get(Conn, "/grisp-connect").
    grisp_seawater_http:close(Conn).

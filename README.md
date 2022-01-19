grisp_seawater
==================

Grisp2 Seawater Client Library


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


### Crypto Configuration

The cryptografic library need to be configured by adding this to your
application `sys.config`:

    {grisp_cryptoauth, [
        {device, #{
            type => 'ATECC608B',
            i2c_bus => 0,
            i2c_address => 16#6C
        }},
        {templates, [
            {{0, 0}, stritzinger_root},
            {{1, 0}, grisp2_intermediate},
            {{2, 0}, grisp2_device}
        ]}
    ]}


Basic Low Level API Usage
-------------------------

    {ok, Conn} = grisp_seawater_http:open().
    {ok, Status, Body} = grisp_seawater_http:get(Conn, "/grisp-connect").
    grisp_seawater_http:close(Conn).

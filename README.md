# grisp_seawater

Grisp2 Seawater Client Library

## Application env options

### ntp

An optional NTP client can be started using option `{ntp, true}`.
Such client is disabled by default (`{ntp, false}`), and is not required to authenticate with GRiSP.io. The client sets the time using `grisp_rtems:clock_set/1`


## Basic Low Level API Usage

    {ok, Conn} = grisp_seawater_http:open().
    {ok, Status, Body} = grisp_seawater_http:get(Conn, "/grisp-connect").
    grisp_seawater_http:close(Conn).

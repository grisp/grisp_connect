# grisp_seawater

Grisp2 Seawater Client Library

## Application env options

### connect

This option is set to `true` as default. Set it to `false` to prevent automatic connection to GRiSP.io on boot.
In such case the state machine that maintains the connection can be started manually using `grisp_io_connection:connect()`.

### ntp

An optional NTP client can be started using option `{ntp, true}`.
Such client is disabled by default (`{ntp, false}`), and is not required to authenticate with GRiSP.io. The client sets the time using `grisp_rtems:clock_set/1`

### ws_request_timeout

Accepts an integer that represents time in milliseconds, default value is `5_000`.
Allows to tweak the timeout of each API request going through the websocket.

## Basic Low Level API Usage

    grisp_seawater_ws:connect().
    true = grisp_seawater_ws:is_connected().
    {ok, <<pong>>} = grisp_seawater_ws:ping().

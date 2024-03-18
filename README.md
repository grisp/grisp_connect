# grisp_io

GRiSP.io Client Library for GRiSP

Add this application as a dependency in your GRiSP2 project.
Your board will connect securely through Mutual TLS to the [GRiSP.io](https://grisp.io) services.
See the [Board Registration](https://github.com/grisp/grisp_io/blob/main/Board_Registration.md) guide on how to start using your GRiSP2 board with GRiSP.io

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

### logs_interval

Accepts an integer that represents time in milliseconds, default value is `2_000`.
Sets the intervall between each log batch dispatch to grisp.io.

### logs_batch_size

Accepts an integer that represents the maximum number of logs that can be batched together, default value is `100`.

## API Usage example

    ok = grisp_io:connect().
    true = grisp_io:is_connected().
    {ok, <<pong>>} = grisp_io:ping().

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

## See all logs from boot on GRiSP.io

Once this app is started, it forwards all logs to GRiSP.io without the need of setting up anything. The only logs that we do not catch are the ones generated before `grisp_io` boots.
If you want to see ALL logs, even from applications that boot before `grisp_io`, you need to disable the default logger handler and set the grisp_io handler as the default one. This involves changing the `kernel` and `grisp_io` app configuration settings in your sys.config file.

You can copy paste these settings. Here we both swap the default logger handler with the grisp_io logger handler and also request it to print logs to stdout.
```erlang
% sys.config
[
    {kernel, [
        % Disable 'default' handler (which buffers all log events in logger).
        {logger, [{handler, default, undefined}]}
    ]},
    {grisp_io,[
        {logger, [
            % Enable the grisp_io handler as default,
            % so that it will receive all events from boot
            {handler,
             default, % name
             grisp_io_logger_bin, % module
             #{
                formatter => {grisp_io_logger_bin, #{
                    % To see logs printed on the USB serial appoint a logger
                    % formatter module of your choice and set the stdout
                    % configuration stdout => {Formatter, FormatterConfig}
                    stdout => {logger_formatter, #{}}
                }}
             }
            }
        ]}
    ]}
].
```

# grisp_connect

GRiSP.io Client Library for GRiSP

Add this application as a dependency in your GRiSP2 project.
Your board will connect securely through Mutual TLS to the [GRiSP.io](https://grisp.io) services.
See the [Board Registration](https://github.com/grisp/grisp_connect/blob/main/Board_Registration.md) guide on how to start using your GRiSP2 board with GRiSP.io

## Application env options

### connect

This option is set to `true` as default. Set it to `false` to prevent automatic connection to GRiSP.io on boot.
In such case the state machine that maintains the connection can be started manually using `grisp_connect_client:connect()`.

### ntp

An optional NTP client can be started using option `{ntp, true}`.
Such client is disabled by default (`{ntp, false}`), and is not required to authenticate with GRiSP.io. The client sets the time using `grisp_rtems:clock_set/1`

### ws_request_timeout

Accepts an integer that represents time in milliseconds, default value is `5_000`.
Allows to tweak the timeout of each API request going through the websocket.

### ws_ping_timeout
Accepts an integer that represents time in milliseconds, default value is `60_000`.
Allows to tweak the timeout between expected ping frames from the server.
If the timeout is exceeded, the socket is closed and a new connection is attempted.

### logs_interval

Accepts an integer that represents time in milliseconds, default value is `2_000`.
Sets the intervall between each log batch dispatch to grisp.io.

### logs_batch_size

Accepts an integer that represents the maximum number of logs that can be batched together, default value is `100`.

## API Usage example

    ok = grisp_connect:connect().
    true = grisp_connect:is_connected().
    {ok, <<pong>>} = grisp_connect:ping().

## See all logs from boot on GRiSP.io

Once this app is started, it forwards all logs to GRiSP.io without the need of setting up anything. The only logs that we do not catch are the ones generated before `grisp_connect` boots.
If you want to see ALL logs, even from applications that boot before `grisp_connect`, you need to disable the default logger handler and set the grisp_connect handler as the default one. This involves changing the `kernel` and `grisp_connect` app configuration settings in your sys.config file.

You can copy paste these settings. Here we both swap the default logger handler with the grisp_connect logger handler and also request it to print logs to stdout.
```erlang
% sys.config
[
    {kernel, [
        % Disable 'default' handler (which buffers all log events in logger).
        {logger, [{handler, default, undefined}]}
    ]},
    {grisp_connect,[
        {logger, [
            % Enable the grisp_connect handler as default,
            % so that it will receive all events from boot
            {handler,
             default, % name
             grisp_connect_logger_bin, % module
             #{
                formatter => {grisp_connect_logger_bin, #{
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

## Local Development

Add an entry in your local hosts file so the domain www.seawater.local points
to your local development server.

Start a local development shell:

    rebar3 as local shell

Run tests:

    rebar3 ct


## Development on GRiSP Hardware

Add an entry in the grisp hosts file so the domain www.seawater.local points
to your local development server.

e.g. using `ifconfig` command (MacOS and older linux):

    echo "$(ifconfig | grep 'inet ' | grep -v 127.0.0.1 | awk '{ print $2 }' | head -n 1) www.seawater.local" >> grisp/default/common/deploy/files/etc/hosts
]
e.g. using `ip` command (Newer linux):

    echo "$(ip addr show | grep 'inet ' | grep -v 127.0.0.1 | awk '{print $2}' | cut -d/ -f1 | head -n 1) www.seawater.local" >> grisp/default/common/deploy/files/etc/hosts

To deploy the release, configure rebar3_grisp's deploy configuration in
rebar.config and then run:

    rebar3 as dev grisp deploy


## Production on GRiSP Hardware

To deploy on GRiSP hardware for production, configure rebar3_grisp's deploy
configuration in rebar.config and then run:

    rebar3 as prod grisp deploy

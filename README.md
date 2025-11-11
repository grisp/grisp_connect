# grisp_connect

GRiSP.io Client Library for GRiSP. This library enables secure communication
 between your GRiSP2 board and the [GRiSP.io](https://grisp.io) services using
 Mutual TLS (mTLS). To get started, add this application as a dependency in your
 GRiSP2 project.

⚠️ **Note:** If you plan to use the API calls related to `grisp_updater_grisp2`,
make sure to add `grisp_updater_grisp2` as a dependency in your project as well.

## Table of content

- [grisp\_connect](#grisp_connect)
  - [Table of content](#table-of-content)
  - [Usage](#usage)
    - [Option 1. Use the `rebar3_grisp` plugin](#option-1-use-the-rebar3_grisp-plugin)
    - [Option 2. Add `grisp_connect` Manually to your Application](#option-2-add-grisp_connect-manually-to-your-application)
  - [Further Steps](#further-steps)
    - [Check whether the board is connected](#check-whether-the-board-is-connected)
    - [Link your Device to your GRiSP.io Account](#link-your-device-to-your-grispio-account)
    - [Troubleshooting:](#troubleshooting)
    - [Need to Unlink a Device?](#need-to-unlink-a-device)
  - [Environment Options](#environment-options)
    - [`connect`](#connect)
    - [`ntp`](#ntp)
    - [`ws_request_timeout`](#ws_request_timeout)
    - [`ws_ping_timeout`](#ws_ping_timeout)
    - [`logs_interval`](#logs_interval)
    - [`logs_batch_size`](#logs_batch_size)
  - [grisp\_keychain](#grisp_keychain)
    - [Use a specific backend](#use-a-specific-backend)
    - [Development options](#development-options)
      - [`allow_expired_certs`](#allow_expired_certs)
  - [See all Logs on GRiSP.io](#see-all-logs-on-grispio)
  - [Development](#development)
    - [Local Development](#local-development)
    - [Development on GRiSP Hardware](#development-on-grisp-hardware)
    - [Production on GRiSP Hardware](#production-on-grisp-hardware)

## Usage

### Option 1. Use the `rebar3_grisp` plugin

The `configure` method of the `rebar3_grisp` version >= 2.6.0 will add `grisp_connect` with the needed configurations to your GRiSP application. See [GRiSP Wiki/Use-your-GRiSP-2-board-with-GRiSP.io](https://github.com/grisp/grisp/wiki/Use-your-GRiSP-2-board-with-GRiSP.io).

### Option 2. Add `grisp_connect` Manually to your Application

Add

```erlang
{dep, [{grisp_connect, "1.0.0"}]}
```

to your `rebar.config` file.

If you still need to link your device configure the device linking token in your `sys.config`:

```erlang
[ {grisp_connect, [
    {device_linking_token, <<"...">>}
]}].
```

You can find your device linking token on [GRiSP.io/grisp_manager](https://grisp.io/grisp_manager) under "How to Link a Device/Option 2: Manual Linking (Ethernet / Wifi)".

You can also skip this configuration and insert the token manually later.

## Further Steps

### Check whether the board is connected

```erlang
> grisp_connect:is_connected().
true
> grisp_connect:ping().
{ok,<<"pong">>}
```

### Link your Device to your GRiSP.io Account

```erlang
> grisp_connect:link_device(<<"...">>).
```

Or, if your token is configured in the environment:

```erlang
> grisp_connect:link_device().
```

### Troubleshooting:
`grisp_connect:link_device` may fail with the following errors.

**Common Errors:**

- `token_expired`: regenerate one from the web page
- `invalid_token`: please double check you typed it correctly
- `token_undefined`: you called `grisp_connect:link_device/0` without setting `device_linking_token`
- `disconnected`: check that your board can connect
- `device_already_linked`: please do not steal GRiSP boards :smirk:
  if you need to unlink a GRiSP board see below...

### Need to Unlink a Device?

We currently do not expose a public API to unlink a Device. Please reach out to us for assistance.

If you encounter any problems or have questions, don't hesitate to contact [support](mailto:grisp@stritzinger.com). Happy coding!

## Environment Options

### `connect`

This option is set to `true` as default. Set it to `false` to prevent automatic connection to GRiSP.io on boot.
In such case the state machine that maintains the connection can be started manually using `grisp_connect_client:connect()`.

### `ntp`

An optional NTP client can be started using option `{ntp, true}`.
Such client is disabled by default (`{ntp, false}`), and is not required to
authenticate with GRiSP.io. The client sets the time using
`grisp_rtems:clock_set/1`.
The default NTP servers to use can be overridden by setting the `grisp_connect`
option: `{ntp_servers, ["ntp1.server.foo", "ntp2.server.foo"]}`. For every NTP
requests, a server address will be picked randomly from the list.
The default refresh period in seconds can be overridden by setting the
`grisp_connect` option: `{ntp_refresh_period, 1024}`.

### `ws_request_timeout`

Accepts an integer that represents time in milliseconds, default value is `5_000`.
Allows to tweak the timeout of each API request going through the websocket.

### `ws_ping_timeout`
Accepts an integer that represents time in milliseconds, default value is `60_000`.
Allows to tweak the timeout between expected ping frames from the server.
If the timeout is exceeded, the socket is closed and a new connection is attempted.

### `logs_interval`

Accepts an integer that represents time in milliseconds, default value is `2_000`.
Sets the intervall between each log batch dispatch to grisp.io.

### `logs_batch_size`

Accepts an integer that represents the maximum number of logs that can be batched together, default value is `100`.

## grisp_keychain

To be able to connect securely with our backend you need to correctly setup certificates and keys access through `grisp_keychain` enviroment.

This can variate greatly depending on the platfrom and usecase of your deployment.
To correctly setup a new project, please use our [rebar3_grisp plugin](https://github.com/grisp/rebar3_grisp?tab=readme-ov-file#create-new-application).

TLS settings are fetched through the [grisp_keychain app](https://github.com/grisp/grisp_keychain). This app defaults to use a simple file filesystem access and requires additional informations to locate the certificates.

In this example we use `grisp_keychain` alone. Relying on free filesystem access.
We are using certifi CAs and a device certificate and key we have in priv.

```erlang
    % Example sys.config
    [
        ...
        {grisp_keychain, [
            {api_module, grisp_keychain_filesystem}, % default
            {tls_server_trusted_certs_cb, {certifi, cacerts, []}},
            {client_certs, {priv, myapp, "certs/device.pem"}},
            {client_key, {priv, myapp, "certs/device.key"}}
        ]}
    ]
```

### Use a specific backend

For production usecases we are developing backends to securely access certificates and keys.

For example: if you are using GRiSP2 you will need `grisp_cryptoauth` to use secrets held by the secure element. See the [grisp_cryptoauth TLS options](https://github.com/grisp/grisp_cryptoauth?tab=readme-ov-file#configuring-tls-options) to customize them. A simple configuration that uses secure element certs and `certifi` as CA provider will look like this:

```erlang

    % Example sys.config
    [
        ...
        {grisp_keychain, [
            {api_module, grisp_cryptoauth}
        ]},
        {grisp_cryptoauth, [
            {tls_server_trusted_certs_cb, {certifi, cacerts, []}}
        ]}
    ]
```
Note: we do not depend on `certifi`, make sure it is added to your deps in case you want to use it.

### Development options

#### `allow_expired_certs`

`grisp_connect` will ignore `cert_expired` errors during certificate path validation. This should only be used for development. Default is `false`.

```erlang
    % Example sys.config
    [
        ...
        {grisp_keychain, [
            ...
            {allow_expired_certs, true},
            ...
        ]}
    ]
```

## See all Logs on GRiSP.io

Once this app is started, it forwards all logs to GRiSP.io without the need of setting up anything. The only logs that we do not catch are the ones generated before `grisp_connect` boots.
If you want to see ALL logs, even from applications that boot before `grisp_connect`, you need to add the log handler in the kernel configuration and disable the one defined in the `grisp_connect` application configuration. This involves changing the `kernel` and `grisp_connect` app configuration settings in your sys.config file.

You can copy paste these settings:

```erlang
% sys.config
[
    {kernel, [
        {logger_level, notice},
        {logger, [
            {handler, default, logger_std_h, #{
                level => notice,
                filter_default => log,
                filters => [
                    % Filter out supervisor progress reports so TLS certificates
                    % are not swamping the console if the level is set to info...
                    {disable_progress, {fun logger_filters:progress/2, stop}}
                ]
            }}
            {handler, grisp_connect_log_handler,  grisp_connect_logger_bin, #{
                level => notice,
                filter_default => log,
                formatter => {grisp_connect_logger_bin, #{}},
                filters => [
                    % Filter out supervisor progress reports so TLS certificates
                    % are not swamping grisp.io if level is set to info...
                    {disable_progress, {fun logger_filters:progress/2, stop}}
                ]
             }
            }
        ]}
    ]},
    {grisp_connect, [
        % Disable the log handler defined in grisp_connect application default
        % configuration, as it was explicitly started in kernel configuration
        % in order to catch the log entries before grisp_connect is started.
        {logger, []}
        ]}
    ]}
].
```

To simplify the log on the device while still sending full log reports to
griso.io, use grisp_connect_log log formatter with single-line in the default
log handler:

```
            {handler, default, logger_std_h, #{
                level => notice,
                formatter => {grisp_connect_log, #{
                    legacy_header => false,
                    single_line => true,
                    description_only => true
                }},
                filter_default => log,
                filters => [
                    % Filter out supervisor progress reports so TLS certificates
                    % are not swamping the console if the level is set to info...
                    {disable_progress, {fun logger_filters:progress/2, stop}}
                ]
            }}
```

## Development

### Local Development

Add an entry in your local hosts file so the domain www.seawater.local points
to your local development server.

Make sure you have the proper certificates under `prinv/server` and `priv/client`. Refer to the backend private docs on how to generate the certificates.

Start a local development shell:

    rebar3 as local shell

Run tests:

    rebar3 ct

To run the tests it might be necessary that you clean out the _build folder in case you compiled with another profile before:

    rm -rf _build

### Development on GRiSP Hardware

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

### Production on GRiSP Hardware

To deploy on GRiSP hardware for production, configure rebar3_grisp's deploy
configuration in rebar.config and then run:

    rebar3 as prod grisp deploy

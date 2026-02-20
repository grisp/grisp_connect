# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.0.0] - 2026-02-20

### Added

- This library is now generic and can run on any platform which supports Erlang
- Add option `allow_expired_certs` to enable custom verify_fun that skips certificate expiration errors

### Changed

- grisp_connect is now generic and not exclusively tied to run on GRiSP2 boards
- Replace `grisp` and `grisp_cryptoauth` dependencies with `grisp_keychain`
- Use `grisp_keychain` to get TLS options and manage cluster certificates
- Cleanup configuration files and test suites
- Update local profile with improved documentation

### Removed

- Removed certifi
- Removed grisp_cryptoauth
- Removed grisp

## [2.1.0] - 2025-03-12

### Changed

- Add log event metadata instead of prepending "[JSON incompatible term]" to
log string data.
- The NTP client was made a bit more robust with exponential backoff.
- The NTP client server list can be parametrised with grisp_connect's ntp_servers
environment key.
- The NTP client is now refreshing the time periodically, and the period can
be configured with grisp_connect's ntp_refresh_period environment key.

### Added

- Add the optional software and hardware information objects to the system
info API result.
- Add an API to make the the board join a Beam cluster using TLS distribution.
- Add cluster information to system_info response.

## [2.0.0] - 2025-02-26

### Changed

- The name of the grisp_connect configuration key to control the timeout of
individual JSON-RPC requests changed from ws_requests_timeout to
ws_request_timeout.
- The default log filter changed to trying to filter out only some messages to
filtering out all progress messages, as it wasn't working reliably.
- JSON-RPC logic was extracted into the jarl library.
- Jarl parses the methods into a list of atom or binaries to pave the
road for namespaces. foo.bar.Buz is parsed into [foo, bar, <<"Buz">>] (if foo
and bar are already existing atoms, but 'Buz' is not).
- Upgrade grisp dependency to 2.8.0.
- Add jittered exponential backoff for reconnection.
- Changed logging API from push to pull. Instead of the client (grisp_connect)
pushing batches of log event to the server (grisp.io), the server is now pulling
them with the request `log.get`. In order to synchronize the client ring buffer,
the server sends `log.sync` notifications.

## Fixed

- The client is now waiting 1 second before trying to reconnect when it gets
disconnected from the server.

## [1.1.0] - 2024-10-12

### Added

- Support for GRiSP.io software updates API
  - update package deployment
  - query system partition state
  - update cancellation
  - remote reboot
  - progress notifications
  - validation

### Changed

- Minimum `grisp` version set to 2.7

## [1.0.0] - 2024-09-26

#### Added
- NTP handling
- Connect to GRiSP.io
- Device Linking to User Account in GRiSP.io
- Logging towards GRiSP.io
- Cleanup prod/dev/local profiles.
- Use grisp_cryptoauth EMULATE_CRYPTOAUTH for tests and local shell.
- Use grisp_cryptoauth TLS helper to generate TLS options.
- Start integrating grisp updater.

[Unreleased]: https://github.com/grisp/grisp_connect/compare/3.0.0...HEAD
[3.0.0]: https://github.com/grisp/grisp_connect/compare/2.1.0...3.0.0
[2.1.0]: https://github.com/grisp/grisp_connect/compare/2.0.0...2.1.0
[2.0.0]: https://github.com/grisp/grisp_connect/compare/1.1.0...2.0.0
[1.1.0]: https://github.com/grisp/grisp_connect/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/grisp/grisp_connect/compare/6b59d16383b3e5154ef839bcf5c77a6b770aada5...1.0.0

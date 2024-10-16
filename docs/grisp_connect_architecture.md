# Architecture

## Client

The client process is the main state machine. Its responsabilities are:

 - Trigger connection/reconnection to the backend.
 - Ensure exponential backoff when reconnecting.
 - Handle generic high-level protocol handling.
 - Expose high-level protocol API to the application.
 - Handle handshaking, versioning and capabilities.
 - Implement generic API endpoints.
 - Instanciate and manage feature callback modules.
 - Delegate API to corresponding features.

See the [client documentation](grisp_connect_client.md).


## Connection

The connection process is abstracting the JSONRpc protocol.

It is not supervised, the process starting it must monitor it.

See the [client documentation](grisp_connect_connection.md).


## Feature Callback Module

Each features implement there own callback module that will be instanciated by
the client process. These callback module are free to spawn processes if needed.


### System Feature

This feature implement system-related API like retrieving system information and
rebooting the device.


### Logging Feature

This feature provide support for pushing the device log to the server.


### Software Update Feature

The software update feature provide the API to start and manage a software
update of the device.

See the [software update feature documentation](grisp_connect_update_feature.md).
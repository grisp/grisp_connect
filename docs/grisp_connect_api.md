# Websocket API

**Table Of Contents**
- [Websocket API](#websocket-api)
  - [Backend API](#backend-api)
    - [Requests](#requests)
    - [Notifications](#notifications)
  - [Error Codes](#error-codes)
    - [Default error codes](#default-error-codes)
    - [Custom error codes](#custom-error-codes)

We use [jsonrpc](https://www.jsonrpc.org) 2.0 between frontend and backend.

⚠️ **Note:** If you plan to use the API calls related to `grisp_updater`, make
 sure to add `grisp_updater_grisp2` as a dependency in your project as well.

## Protocol Requests

### Generic

#### Server Notifications

</p>
</details>
<details><summary><i>Unlink Device</i></summary>
<p>

Inform the client the device has been unlinked.

**method**: `"DeviceUnlinked"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---

</p>
</details>


#### Client Requests

<details><summary><i>Perform Protocol Handshake</i></summary>
<p>

Agree on the version and capabilities of the protocol.

**method**: `"Handshake"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"version"` * | string | e.g. `"1.0.0"` | grisp_connect's protocol version
| `"platform"` * | string | `"grisp2"` | The device platform identifier
| `"platform_version"` * | string | `"2"` | The device platform version
| `"serial"` * | string | e.g. `"1305"` | The serial number of the device
| `"capabilities"` * | list of string | e.g. `["system", "logging", "update"]` | Fetures supported by the client

The supported capabilities are:

| Value | Description
| --- | ---
| `"system"` | The system feature, expose system information and actions like rebooting the device
| `"logging"` | The logging feature, allows the client to push logging entries to the server
| `"update"` | The software update feature, allows the server to trigger a device software update 

**result**: object

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"linked"` * | boolean | `true`, `false` | Tells if the device is already linked
| `"capabilities"` * | list of string | e.g. `["system"]` | Capabilities the server needs

**error**:

| Error Code | Message example | Description
| --- | --- | ---
| -42001 | `"Client not supported"` | Client protocol version is not supported by the server

</p>
</details>

<details><summary><i>Link Device</i></summary>
<p>

Agree on the version and capabilities of the protocol.

**method**: `"LinkDevice"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"token"` * | string | e.g. `"C8PwywzJ...4cxjrz3"` | Linking token given by the server UI

**result**: string

| Value | Description
| --- | ---
| `"ok"` | Device linked successfully

**error**:

| Error Code | Message example | Description
| --- | --- | ---
| -42004 | `"Invalid token"` | the token is invalid, e.g. not properly encoded     
| -42005 | `"Token expired"`| The token is expired
| -42006 | `"Device already linked"` | The device needs to be unlinked first via UI

</p>
</details>


### Logging Feature

#### Server Requests

</p>
</details>
<details><summary><i>Start Logging</i></summary>
<p>

Ask the client to start sending logging notifications to the server.

**method**: `"logging.Start"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---

**result**: string

| Value | Description
| --- | ---
| `"ok"` | The server will start receviving `"logging.Push"` notitifcations

**error**:

| Error Code | Message example | Description
| --- | --- | ---

</p>
</details>

</p>
</details>
<details><summary><i>Stop Logging</i></summary>
<p>

Ask the client to stop sending logging notifications to the server.

**method**: `"logging.Stop"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---

**result**: string

| Value | Description
| --- | ---
| `"ok"` | The server will stop to send `"logging.Push"` notitifcations

**error**:

| Error Code | Message example | Description
| --- | --- | ---

</p>
</details>


#### Client Requests

<details><summary><i>Push Logs</i></summary>
<p>

Push a chunk of log entries to the server.

**method**: `"logging.Push"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"events"` * | list | `[[1, "log entry 1"],[2, "log entry 2"]]` | List of log entries
| `"dropped"` * | integer | e.g. `0` | The number of entries that got dropped

**result**: object

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"seq"` * | integer | e.g. `123` | The highest log entry sequence number the server is aware of
| `"dropped"` * | integer | e.g. `0` | The confirmation of the number of dropped entries by the server

**error**:

| Error Code | Message example | Description
| --- | --- | ---

</p>
</details>


### System Feature

#### Server Requests

<details><summary><i>Get System Information</i></summary>
<p>

Ask the client for generic syste minformation.

**method**: `"system.GetInfo"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---


**result**: object

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"id"` * | string | e.g. `"YzZiNWY0O...RE1FLm1kCg=="` | A unique identifier of the software running on the device
| `"relname"` * | string | e.g. `"grisp_demo"` | The release name of the software running on the device
| `"relvsn"` * | string | e.g. `1.0.0` | The release version of the software running on the device
| `"profiles"` * | list of string | e.g. `["dev", "foobar"]` | The list of profiles used to build the software running on the device
| `"boot_source"` * | string | `"sdcard"`, `"system_a"`, `"system_b"` | Where the device booted from
| `"allowed"` * | list of string | e.g. `["Reboot"]` | The list of allowed system requests

**error**:

| Error Code | Message exmaple | Description
| --- | --- | ---

</p>
</details>

<details><summary><i>Reboot Device</i></summary>
<p>

Ask the client for generic syste minformation.

**method**: `"system.Reboot"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---


**result**: string

| Value | Description
| --- | ---
| `"ok"` | The device will reboot right away

**error**:

| Error Code | Message exmaple | Description
| --- | --- | ---
| 42101 | `"Reboot not allowed"` | The device do not allow reboot requests

</p>
</details>


### Software Update Feature

#### Server Requests

<details><summary><i>Get Software Update Information</i></summary>
<p>

Ask the client for information about the current software update state.

**method**: `"update.GetInfo"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---


**result**: object

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"state"` * | string | `"ready"`, `"updating"`, `"updated"` or `"validating"` | The current state of the software update on the device
| `"target"` * | string | `"system_a"` or `"system_b"` | The target system where a software is, will or has been applied
| `"actions"` * | list of string | e.g. `["Cancel", "Validate"]` | The list of possible action at the current stage of the software update process
| `"progress"` | integer | e.g. `32` | If `"state"` is `"updating"`, this field give the update progress as a percentage
| `"release"` | object | See after | Inforamtion about the software update package being installed when state is `"updating"`, `"updated"` or `"validating"`
| `"last_event"` | string | `"failed"`, `"cancelled"` or `"done"` | When in state `"ready"`, this field inform about the last event of the previous software update
| `"last_reason"` | string | The machine-readable reason of the last event if it is `"failed"`
| `"last_message"` | string | The optional human-readable message of the last event if it is `"failed"`

The release information object is:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"id"` * | string | e.g. `"YzZiNWY0O...RE1FLm1kCg=="` | A unique identifier of the software currently being installed
| `"relname"` * | string | e.g. `"grisp_demo"` | The release name of the software currently being installed
| `"relvsn"` * | string | e.g. `1.0.0` | The release version of the software currently being installed
| `"profiles"` * | list of string | e.g. `["dev", "foobar"]` | The list of profiles used to build the software currently being installed

The possible values for the list of actions are:

| value | Description
| --- | ---
| `"Start"` | Start the update process by calling `"update.Start"`
| `"Cancel"` | Cancel the update process by calling `"update.Cancel"`
| `"Reboot"` | Reboot the device by calling `"system.Reboot"`
| `"Validate"` | Validate the current software version by calling `"update.Validate"`
| `"RemoveSDCard"` | An update is detected but the device booted from SD card

**error**:

| Error Code | Message exmaple | Description
| --- | --- | ---

</p>
</details>

<details><summary><i>Start Software Update</i></summary>
<p>

Ask the client start updating the device.

**method**: `"update.Start"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"url"` * | string | The URL to root of the software update package to use to update the device

**result**: string

| Value | Description
| --- | ---
| "ok" | The device update process has been started successfully

**error**:

| Error Code | Message exmaple | Description
| --- | --- | ---
| -42301 | `"Already updating"` | The device is currently 
| -42302 | `"Validation, cancellation or reboot required"` | The current software requires either to be validated, rebooted or cancelled
| -42303 | `"Reboot required"` | If the device do not allow remotely triggered reboot, manual reboot is required

</p>
</details>

<details><summary><i>Cancel Current Software Update</i></summary>
<p>

Ask the client to cancel the current software update process. This could trigger the device to reboot.

**method**: `"update.Cancel"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---

**result**: string

| Value | Description
| --- | ---
| `"ok"` | The update process was cancelled successfuly
| `"manual_reboot"` | The update was canceld, but a manual reboot is required to finalise the cancellation

**error**:

| Error Code | Message exmaple | Description
| --- | --- | ---
| -42304 | `"Not updating"` | There is no currently any software update session

</p>
</details>

<details><summary><i>Validate Current Sofware</i></summary>
<p>

Ask the client to validate the currently running updated software.

**method**: `"update.Validate"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---

**result**: string

| Value | Description
| --- | ---
| `"ok"` | The current software was validated and marked as so successfully

**error**:

| Error Code | Message exmaple | Description
| --- | --- | ---
| -42305 | `"Validation failed"` | The validation process failed, the device will reboot to the previouse validated version
| -42306 | `"Validation failed, manual reboot required"` | The validation process failed, the device need to be rebooted manually

</p>
</details>


#### Client Notifications

</p>
</details>
<details><summary><i>Software Update Event</i></summary>
<p>

The client notify the server of the software update progress.

**method**: `"logging.Event"`

**params**:

| Key (required *) | Type | Value / Example | Description
| --- | --- | --- | ---
| `"event"` * | string | e.g. `"progress"`  | The software update event being notified
| `"state"` * | string | `"ready"`, `"updating"`, `"updated"`, `"validating"` | The current state of the software update
| `"target"` * | string | `"system_a"` or `"system_b"` | The target system where a software is, will or has been applied
| `"actions"` * | list of string | e.g. `["Cancel", "Validate"]` | The list of possible action at the current stage of the software update process
| `"release"` | object | See after | Inforamtion about the software update package being installed when state is `"updating"`, `"updated"` or `"validating"`
| `"progress"` | integer | e.g. `42` | Update progress as a percentage for events `"progress"` and `"warning"`
| `"reason"` | string | e.g. `"internal_error"` | Machine-readable reason for events `"warning"` and `"failed"`
| `"message"` | string | e.g. `"Internal error"` | Optional human-readable message for events `"warning"` and `"failed"`

See `"update.GetInfo"` request's result for description of the release information and the possible actions.

Possible values for `"event"` field:

| Value | Description
| --- | --- | --- | ---
| `"ready"` | The device is ready to start and update; sent when connected and there is no previous event
| `"progress"` | A software update is in progress
| `"warning"` | A warning was traised during progress, the field `"reason"` must be defined, and optionaly the field `"message"`
| `"failed"` | The software update faield, the field `"reason"` must be defined, and optionaly the field `"message"`
| `"cancelled"` | The software update was cancelled
| `"done"` | The software update process was completed successfully

</p>
</details>


## Error Codes

### Standard error codes

| code | message example | meaning
| --- | --- | ---
|-32700 | `"Parse error"` | Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text.
|-32600 | `"Invalid Request"` | The JSON sent is not a valid Request object.
|-32601 | `"Method not found"` | The method does not exist or is not available.
|-32602 | `"Invalid params"` | Invalid method parameter(s).
|-32603 | `"Internal error"` | Internal JSON-RPC error.


### Custom error codes

Additionally to the error code defined in each requests specification, the following codes will be returned by any of them.

|code | message example | meaning
|---|---|---
| -42002 | `"Device not linked"`| The device can't be used without being linked to a registered user
| -42003 | `"Feature not available"` | The requested feature is not available

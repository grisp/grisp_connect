# UI Websocket API

**Table Of Contents**
- [UI Websocket API](#ui-websocket-api)
  - [Backend API](#backend-api)
    - [Requests](#requests)
  - [Error Codes](#error-codes)
    - [Default error codes](#default-error-codes)
    - [Custom error codes](#custom-error-codes)
  - [Notifications](#notifications)

We use [jsonrpc](https://www.jsonrpc.org) 2.0 between frontend and backend.

## Backend API

### Requests

<details><summary><i>Post - Start an update</i></summary>
<p>

Triggers grisp_updater to install an update from the given URL.

**`params`:**
| key (required *)  | value    | description                |
| ----------------- | -------- | -------------------------- |
| `"type"` *        | string   | `"start_update"`           |
| `"url"` *         | [string] | URL to the code repository |

**`result`**:  `"ok"`

**`error`**:

| Error Content                                       | When it Happens                  |
| ----------------------------------------------------| -------------------------------- |
| `{code: -10, message: "grisp_updater_unavailable"}` | Grisp updater app is not running |
| `{code: -11, message: "already_updating"}`         | An update is already happening   |
| `{code: -12, message: "boot_system_not_validated"}` | The board rebooted after an update and needs validation |

</p>
</details>

<details><summary><i>Post - Validate an update</i></summary>
<p>

Validates the current booted partition. This can only be done after an update was installed and a reboot occurred.
This request sets the current partition as permanent in the bootloader if it is not.
If the new partition is not validated, from the next reboot, the bootloader will load the previous one.

**`params`:**
| key (required *)  | value    | description                |
| ----------------- | -------- | -------------------------- |
| `"type"` *        | string   | `"validate"`               |

**`result`**:  `"ok"`

**`error`**:

| Error Content                                       | When it Happens                  |
| ----------------------------------------------------| -------------------------------- |
| `{code: -13, message: "validate_from_unbooted", data: 0}` | The current partition N cannot be validated |

</p>
</details>

## Error Codes

### Default error codes

|  code   |   message        | meaning                                          |
|---------|------------------|--------------------------------------------------|
|-32700   | Parse error      | Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text. |
|-32600   | Invalid Request  | The JSON sent is not a valid Request object. |
|-32601   | Method not found | The method does not exist / is not available.|
|-32602   | Invalid params   | Invalid method parameter(s). |
|-32603   | Internal error   | Internal JSON-RPC error. |

### Custom error codes

Additionally to the default jsonrpc error codes the following codes will be returned.

|code  | message            | meaning |
|---|---|---|
| -1    | `"device not linked"`     | device can't be used without being linked to a registered user    |
| -2    | `"token expired"`         | token is expired                          |
| -3    | `"device already linked"` | device needs to be unlinked first via UI  |
| -4    | `"invalid token"`         | token is e.g. not orderly encoded         |

## Notifications

<details><summary><i>Notify - Status Update </i></summary>

**`result`**:  `JSON Object`

| key            | value             | description                       |
| -------------- | ----------------- | --------------------------------- |
| `"type"`       | `"status_update"` | Type of notification              |
| `"percentage"` | integer           | Progress percentage of the update |

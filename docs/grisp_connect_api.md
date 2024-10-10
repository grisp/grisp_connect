# UI Websocket API

**Table Of Contents**
- [UI Websocket API](#ui-websocket-api)
  - [Backend API](#backend-api)
    - [Requests](#requests)
    - [Notifications](#notifications)
  - [Error Codes](#error-codes)
    - [Default error codes](#default-error-codes)
    - [Custom error codes](#custom-error-codes)

We use [jsonrpc](https://www.jsonrpc.org) 2.0 between frontend and backend.

⚠️ **Note:** If you plan to use the API calls related to `grisp_updater`, make
 sure to add `grisp_updater_grisp2` as a dependency in your project as well.

## Backend API

### Requests

</p>
</details>
<details><summary><i>Get - system_info</i></summary>
<p>

Retrieves the current state of the system. It returns the currently running
release name and version and if update is enabled.

**`params`:**
| key (required *)  | value    | description         |
| ----------------- | -------- | ------------------- |
| `"type"` *        | string   | `"system_info"` |

**`result`**:  JSON Object

| key             | value     | type     | description                                                      |
|-----------------|-----------|----------|------------------------------------------------------------------|
| relname         | string    | required | The name of the release running currently on the device          |
| relvsn          | string    | required | The version of the release running currently on the device       |
| update_enabled  | boolean   | required | If updating is enbaled on the device                             |
| boot_source     | map       | optional | {"type": "system", "id": ID} or {"type": "removable"}            |
| update_status   | string    | optional | `"ready"`, `"updating"`, `"failed"`, or `"updated"`              |
| update_progress | integer   | optional | The progress as a percentage                                     |
| update_message  | string    | optional | Message describing the current state of the system               |
| action_required | boolean   | optional | `null`, `"reboot"`, `"remove_sdcard_and_reboot"` or `"validate"` |

Meaning of the status:

| key               | description                                                                                |
|-------------------|--------------------------------------------------------------------------------------------|
| `"ready"`         | The system is ready for initiating an update                                               |
| `"updating"`      | The system is in the process of updating                                                   |
| `"failed"`        | The update failed, but a new update can be initiated                                       |
| `"updated"`       | The update succeed, but actions are required like "reboot" or "validate"                   |

</p>
</details>
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
| `{code: -11, message: "already_updating"}`          | An update is already happening   |
| `{code: -12, message: "boot_system_not_validated"}` | The board rebooted after an update and needs validation |

</p>
</details>

<details><summary><i>Post - Validate an update</i></summary>
<p>

Validates the current booted partition. This can only be done after an update was installed and a reboot occurred.
This request sets the current partition as permanent in the bootloader if it is not.
If the new partition is not validated, from the next reboot, the bootloader will load the previous one.
This should only be called if the new software is functioning as expected.

**`params`:**
| key (required *)  | value    | description                |
| ----------------- | -------- | -------------------------- |
| `"type"` *        | string   | `"validate"`               |

**`result`**:  `"ok"`

**`error`**:

| Error Content                                       | When it Happens                  |
| ----------------------------------------------------| -------------------------------- |
| `{code: -10, message: "grisp_updater_unavailable"}` | Grisp updater app is not running |
| `{code: -13, message: "validate_from_unbooted", data: 0}` | The current partition N cannot be validated |

</p>
</details>

<details><summary><i>Post - Reboot the device</i></summary>
<p>

**`params`:**
| key (required *)  | value    | description                |
| ----------------- | -------- | -------------------------- |
| `"type"` *        | string   | `"reboot"`                 |

**`result`**:  `"ok"`

</p>
</details>

### Notifications

<details><summary><code>update</code> <code>{"type":"software_update_event"}</code> - notify the current progess of grisp_updater </summary>
<p>

**`params`:**
| key           | value                                       | type     | description                          |
|---------------|---------------------------------------------|----------|--------------------------------------|
|`"type"`       | `"software_update_event"`                   | required |                                      |
|`"event_type"` | `"progress"` `"warning"` `"error"` `"done"` | required |                                      |
|`"message"`    |  integer                                    | optional | expected in case of warning or error |
|`"reason"`     |  integer                                    | optional | expected in case of warning or error |
|`"percentage"` |  integer                                    | optional | expected in case of progress or error|

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

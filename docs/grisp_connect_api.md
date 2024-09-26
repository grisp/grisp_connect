# UI Websocket API

**Table Of Contents**
- [UI Websocket API](#ui-websocket-api)
  - [Backend API](#backend-api)
    - [Requests](#requests)
  - [Error Codes](#error-codes)
    - [Default error codes](#default-error-codes)
    - [Custom error codes](#custom-error-codes)

We use [jsonrpc](https://www.jsonrpc.org) 2.0 between frontend and backend.

## Backend API

### Requests

<details><summary><i>Post - Start an update</i></summary>
<p>

**`params`:**
| key (required *)  | value    | description                |
| ----------------- | -------- | -------------------------- |
| `"type"` *        | string   | `"start_update"`           |
| `"url"` *         | [string] | URL to the code repository |

**`result`**:  `"ok"`

**`error`**:

| Error Content                                            | When it Happens                                   |
| -------------------------------------------------------- | ------------------------------------------------- |
| `{code: -10, message: "grisp_updater unavailable"}`      | Grisp updater app is not running                  |
| `{code: -11, message: "already updating "}`              | An update is already happening                    |

</p>
</details>

<details><summary><i>Post - Flash </i></summary>
<p>

**`params`:**
| key (required *)    | value    | description                       |
| ------------------- | -------- | --------------------------------- |
| `"led"` *           | integer  | Number that identifies the LED, to obtain more information about the options, you can visit [grisp_led:color/2](https://hexdocs.pm/grisp/)    |
| `"color"`           | string   | Color of the LED, by default: red. To obtain more information about the options, you can visit [grisp_led:color/2](https://hexdocs.pm/grisp/)   |

**`result`**:  `"ok"`

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

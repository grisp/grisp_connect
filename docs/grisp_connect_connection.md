# Connection

The connection process abstract the low-level JSONRpc 2.0 protocol.

The connection is not supervised, it is the responsability of the process
starting it to monitor it.

It decode the method name into atoms if they exists, and split it when
containing `.`:

  `"foo.bar.DoSomething"` => `[foo, bar, 'DoSomething']`.

If there is not atom already existing in the VM, a new one will **NOT**
be created, instead a binary will be returned:

  `"foo.bar.NotAnAtom"` => `[foo, bar, <<"NotAnAtom">>]`.

As input, the method could either be a single atom when there is no namespace,
a list of atom or binaries, or a single already encoded binary:

 - `[foo, bar, <<"NotAnAtom">>]` => `"foo.bar.NotAnAtom"`
 - `'DoSomething'` => `"DoSomething"`
 - `<<"foo.bar.DoSomethingElse">>'` => `"foo.bar.DoSomethingElse"`

The erlang type spec for the method is:

```erlang
atom() | binary() | list(atom() | binary())
```

When performing asynchronous requests, the caller can pass a context term to
keep track of any information needed to identify a response, an error or a
timeout. This way the caller is not required to keep a data structure to match
the request to the response/error/timeout.

When starting the connection, an error map can be passed to automatically
convert back and forth from atom to JSONRpc error. The map is a list of tuple:

```erlang
[{custom_error, -42042, <<"Custom Error Message">>}]
```

When a mapping is specified, the error codes are converted to atom, and atom
can be given as error code.

It exposes a higher level API to perform synchronous request, sends notification,
sends errors, or performs sasynchronous requests.


## Handler Messages

The connection is given a handler pid when started, multiple messages will be
sent to the handler:

### On Connection

When the connection is fully established, the following message is sent:

```erlang
{conn, Conn :: pid(), connected}
```

### On Server Requests

When the client receive a request from the server, it sends the message:

```erlang
{conn, Conn :: pid(), {request, Method :: method(), Params :: map(), ReqRef :: binary()}}
```

The handler will have then to call either `grisp_connect_connection:reply/3` or
`grisp_connect_connection:error/5`.

### On Asynchronous Response

When calling `grisp_connect_connection:post/4`, the handler will asynchronously
receive the following message when receiving a response from the server:

```erlang
{conn, Conn :: pid(), {response, Method :: method(), Result :: jsx:json_term(), ReqCtx :: term()}}
```

### On Asynchronous Request Error

When performing and asynchronous request, if the server respond with an error,
the following message will be sent to the handler:

```erlang
{conn, Conn :: pid(), {error, Method :: method(), Code :: atom() | integer(), Message :: undefined | binary(), Data :: undefined | jsx:json_term(), ReqCtx :: term()}}
```

If the request error is not a JSONRpc error, but in an internal error like
`not_connected` (sent when the connection is closed), the message sent to the
handler is:

```erlang
{conn, Conn :: pid(), {error, Method :: method(), Reason :: term(), ReqCtx :: term()}}
```

### On Asynchronous Request Timeout

When an asynchronous request timed-out, the handler will receive the message:

```erlang
{conn, Conn :: pid(), {timeout, Method :: binary(), ReqCtx :: term()}}
```

### On Server Notifications

When the server send a notification, the handler will receive the message:

```erlang
{conn, Conn :: pid(), {notification, Method :: method(), Params :: jsx:json_term()}}
```

### On Error Notifications

When the server send an error not related to a request, the following message
will be sent to the handler:

```erlang
{conn, Conn :: pid(), {error, Code :: atom() | integer(), Message :: undefined | binary(), Data :: undefined | jsx:json_term()}}
```


## API Examples

### Starting Connection

```erlang
{ok, Conn} = grisp_connect_connection:start(self(), "www.seawater.local", 8443),
MonRef = erlang:monitor(process, Conn),
```

### Synchronous Requests

```erlang
case grisp_connect_connection:request(Conn, [foo, bar, 'DoSomething'], #{arg => 42}) of
  {ok, Result} -> ...
  {error, timeout} -> ...
  {error, not_connected} -> ...
  {error, Code, Message, Data} -> ...
end
```

### Notifications

```erlang
grisp_connect_connection:notify(Conn, [foo, bar, 'SomethingHappend'], #{}),
```

### Error Notifications

```erlang
grisp_connect_connection:error(Conn, -123456, <<"Some Error">>, undefined, undefined),
```

### Asynchronous Requests

Example using a function in the context as a continuation when receiving the
response, error or timeout:

```erlang
grisp_connect_connection:post(Conn, [foo, bar, 'DoSomething'], #{arg => 42}, #{
  on_result => something_done/2,
  on_error => something_failed/4,
  on_timout => something_timedout/1
}),
```

For this to work, the handler should call the function from the received context
when receiving messages `response`, `error` and `timeout`.

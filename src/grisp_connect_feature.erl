-module(grisp_connect_feature).

-export([is_reboot_allowed/1]).
-export([reboot/1]).
-export([request/3]).
-export([notify/3]).
-export([post/5]).
-export([reply/3]).
-export([error/2, error/3, error/4, error/5]).


%--- Behaviour Specification ---------------------------------------------------

%% @doc Called to initialize the feature.
-callback init(Client, Opts) ->
    {ok, Namespace, ErrorList, State} | {error, Reason}
  when Client :: term(), Opts :: map(), State :: term(), Reason :: term(),
       Namespace :: atom(),
       ErrorList :: [grisp_connect_connection:error_mapping()].

%% @doc Called when the client enter a state.
-callback enter(Client, State, StateName) -> {ok, State} | {error, Reason}
  when Client :: term(), State :: term(), Reason :: term(),
       StateName :: atom().

%% @doc Called when the feature is enalbed by the handshake.
-callback enable(Client, State) ->
    {ok, State} | {error, Reason}
  when Client :: term(), State :: term(), Reason :: term().

%% @doc Called when a request with the feature namespace is received.
%% If the callback is not exported, of the call fail with `undef`,
%% the caller will receive a `method_not_found` error.
-callback on_request(Client, State, Method, Params, ReqRef) ->
    {ok, State} | {error, Reason}
  when Client :: term(), State :: term(), Reason :: term(),
       Method :: grisp_connect_connection:method(),
       Params :: map(),
       ReqRef :: binary().

%% @doc Called when a notification with the feature namespace is received.
-callback on_notification(Client, State, Method, Params) ->
    {ok, State} | {error, Reason}
  when Client :: term(), State :: term(), Reason :: term(),
       Method :: grisp_connect_connection:method(),
       Params :: map().

%% @doc Called when the client is terminating.
-callback terminate(Client, State, Reason) -> ok
    when Client :: term(), State :: term(), Reason :: term().

-optional_callbacks([
    enter/3,
    enable/2,
    on_request/5,
    on_notification/4,
    terminate/3
]).


%--- Types ---------------------------------------------------------------------

-type post_result_callback() :: fun(
    (State :: term(),
     Method :: grisp_connect_connection:method(),
     Result :: term()) ->
        {ok, State :: term()}
      | {error, Reason :: term()}).
-type post_error_callback() :: fun(
    (State :: term(),
     Method :: grisp_connect_connection:method(),
     Type :: internal | request | timeout,
     Code :: atom() | integer(),
     Message :: undefined | binary(),
     Data :: undefined | term()) ->
        {ok, State :: term()}
      | {error, Reason :: term()}).

-export_type([post_result_callback/0, post_error_callback/0]).


%--- API Functions -------------------------------------------------------------

-spec is_reboot_allowed(Client :: term()) -> boolean().
is_reboot_allowed(Client) ->
    grisp_connect_client:feature_is_reboot_allowed(Client).

-spec reboot(Client :: term()) -> ok | manual_reboot_required.
reboot(Client) ->
    grisp_connect_client:feature_reboot(Client).

-spec request(Client, Method, Params) ->
    {ok, Result} | {error, timeout} | {error, not_connected} | {error, Code, Message, Data}
  when Client :: term(),
       Method :: grisp_connect_connection:method(),
       Params :: map(),
       Result :: term(),
       Code :: atom() | integer(),
       Message :: undefined | binary(),
       Data :: undefined | term().
request(Client, Method, Params) ->
    grisp_connect_client:feature_request(Client, Method, Params).

-spec notify(Client, Method, Params) ->
    ok | {error, not_connected}
  when Client :: term(),
       Method :: grisp_connect_connection:method(),
       Params :: map().
notify(Client, Method, Params) ->
    grisp_connect_client:feature_notify(Client, Method, Params).

-spec post(Client, Method, Params, OnResult, OnError) ->
    ok | {error, not_connected}
  when Client :: term(),
       Method :: grisp_connect_connection:method(),
       Params :: map(),
       OnResult :: undefined | post_result_callback(),
       OnError :: undefined | post_error_callback().
post(Client, Method, Params, OnResult, OnError) ->
    grisp_connect_client:feature_post(Client, Method, Params, OnResult, OnError).

-spec reply(Client, Result, ReqRef) ->
    ok | {error, not_connected}
  when Client :: term(),
       Result :: term(),
       ReqRef :: binary().
reply(Client, Result, ReqRef) ->
    grisp_connect_client:feature_reply(Client, Result, ReqRef).

-spec error(Client, Code) ->
    ok | {error, not_connected}
  when Client :: term(),
       Code :: atom() | integer().
error(Client, Code) ->
    grisp_connect_client:feature_error(Client, Code, undefined, undefined, undefined).

-spec error(Client, Code, ReqRef) ->
    ok | {error, not_connected}
  when Client :: term(),
       Code :: atom() | integer(),
       ReqRef :: binary().
error(Client, Code, ReqRef) ->
    grisp_connect_client:feature_error(Client, Code, undefined, undefined, ReqRef).

-spec error(Client, Code, Message, Data) ->
    ok | {error, not_connected}
  when Client :: term(),
       Code :: atom() | integer(),
       Message :: undefined | binary(),
       Data :: undefined | term().
error(Client, Code, Message, Data) ->
    grisp_connect_client:feature_error(Client, Code, Message, Data, undefined).

-spec error(Client, Code, Message, Data, ReqRef) ->
    ok | {error, not_connected}
  when Client :: term(),
       Code :: atom() | integer(),
       Message :: undefined | binary(),
       Data :: undefined | term(),
       ReqRef :: binary().
error(Client, Code, Message, Data, ReqRef) ->
    grisp_connect_client:feature_error(Client, Code, Message, Data, ReqRef).

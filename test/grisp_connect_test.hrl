%--- JSON-RPC asserts ----------------------------------------------------------

% Receive a JSON-RPC request, pattern match method and params, return the id
-define(receiveRequest(Method, Params),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_request(),
                ?assertMatch(#{method := Method, params := Params}, Send),
                maps:get(id, Send)
        end)()).

% Receive a JSON-RPC request with explicit timeout,
% pattern match method and params, return the id
-define(receiveRequest(Timeout, Method, Params),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_request(Timeout),
                ?assertMatch(#{method := Method, params := Params}, Send),
                maps:get(id, Send)
        end)()).


% Receive a JSON-RPC notification, pattern match method and params
-define(receiveNotification(Method, Params),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_notification(),
                ?assertMatch(#{method := Method, params := Params}, Send),
                ok
        end)()).

% Receive a JSON-RPC result, pattern match value and id
-define(receiveResult(Value, Id),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_result(),
                ?assertMatch(#{result := Value, id := Id}, Send),
                ok
        end)()).

% Receive a JSON-RPC request error, pattern match code, message and id
-define(receiveError(Code, Message, Id),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_error(),
                ?assertMatch(#{error := #{code := Code, message := Message}, id := Id}, Send),
                ok
        end)()).

% Receive a JSON-RPC standalone error, pattern match code and message
-define(receiveError(Code, Message),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_error(),
                ?assertMatch(#{code := Code, message := Message}, Send),
                ok
        end)()).

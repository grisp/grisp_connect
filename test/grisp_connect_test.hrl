%--- JSON-RPC asserts ----------------------------------------------------------

% Receive a JSON-RPC request, pattern match method and params, return the id
-define(receiveRequest(Method, Params),
        (fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_request(),
                ?assertMatch(#{method := Method, params := Params}, Send),
                maps:get(id, Send)
        end)()).
% TODO when needed:
%-define(receiveNotification(Method, Params)
%-define(receiveResult(Pattern, Id)
%-define(receiveError(Code, Message, Id)

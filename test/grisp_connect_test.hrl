%--- Async asserts -------------------------------------------------------------

% Spawn a process to wait for the result and check the result using assert.
-define(asyncAssertEqual(Pattern, M, F, A),
        spawn_monitor(?asyncAssertEqualFun(Pattern, M, F, A))).
-define(asyncAssertEqualFun(Pattern, M, F, A),
        fun() -> ?assertEqual(Pattern, erlang:apply(M, F, A)) end).

% Use this macro to wait for the spawned process to end with normal status.
-define(asyncWait(Pid, MRef),
        ?assertEqual(normal, ?asyncWaitStatus(Pid, MRef))).
-define(asyncWaitStatus(Pid, MRef),
        receive {'DOWN', MRef, process, Pid, Status} -> Status
        after 1000 -> error({timeout, receiving_pong})
        end).

%--- JSON-RPC asserts ----------------------------------------------------------

% Receive a JSON-RPC request, pattern match method and params, return the id
-define(receiveJsonRpcRequest(Method, Params),
        (?receiveJsonRpcRequestFun(Method, Params))()).

-define(receiveJsonRpcRequestFun(Method, Params),
        fun() ->
                Send = grisp_connect_test_server:receive_jsonrpc_request(),
                ?assertMatch(#{method := Method, params := Params}, Send),
                maps:get(id, Send)
        end).

%--- Async asserts -------------------------------------------------------------

% Spawn a process to wait for the result and check the result using assert.
-define(asyncAssertEqual(Pattern, Term),
        spawn_link(fun() -> ?assertEqual(Pattern, Term) end)).
% TODO when needed:
% -define(asyncAssert(Term)
% -define(asyncAssertMatch(Pattern, Term) ...

% Use this macro to wait for the spawned process to end with normal status.
-define(asyncWait(Pid),
        ?assertEqual(normal,
                     receive {'EXIT', Pid, Status} -> Status
                     after 1000 -> error({timeout, waiting_exit})
                     end)).

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

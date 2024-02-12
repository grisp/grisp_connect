-module(grisp_io_manager).

-compile([export_all, nowarn_export_all]).

start(PrivDir, CertDir) ->
    application:set_env(mnesia, dir, PrivDir),

    eresu:install([node()]),
    {ok, Started1} = application:ensure_all_started(eresu),
    register_user(),

    grisp_manager:install([node()]),

    application:start(mnesia),

    {ok, Started2} = application:ensure_all_started(kraft),
    SslOpts = [
        {verify, verify_peer},
        {keyfile, filename:join(CertDir, "server.key")},
        {certfile, filename:join(CertDir, "server.crt")},
        {cacertfile, filename:join(CertDir, "CA.crt")}
    ],
    KraftOpts = #{
        port => 3030,
        ssl_opts => SslOpts,
        app => grisp_manager
    },
    KraftRoutes = [
        {"/grisp-connect/ws",
            {ws, grisp_manager_device_api}, #{}, #{type => json_rpc}}
    ],
    kraft:start(KraftOpts, KraftRoutes),

    {ok, Started3} = application:ensure_all_started(grisp_manager),
    Started1 ++ Started2 ++ Started3.

cleanup_apps(Apps) ->
    mnesia:delete_table(eresu_user),
    mnesia:delete_table(grisp_device),
    mnesia:delete_table(grisp_manager_token),
    [application:stop(App) || App <- Apps],
    application:stop(mnesia).

get_cert_dir(DataDir) ->
    SplitDataDir = filename:split(DataDir),
    JoinedParentDir = filename:join(lists:droplast(SplitDataDir)),
    CertDir = filename:join(JoinedParentDir, "certs"),
    true = filelib:is_dir(CertDir),
    CertDir.

register_user() ->
    Hash = erlpass:hash(<<"1234">>),
    WriteUser = fun() ->
                        mnesia:write({eresu_user,
                                      <<"Testuser">>,
                                      <<"foo">>,
                                      <<"a@a.a">>,
                                      erlang:system_time(),
                                      Hash,
                                      <<"Max Mustermann">>,
                                      undefined,
                                      undefined,
                                      <<"customer_id">>,
                                      [<<"subscription_id">>]})
                end,
    mnesia:activity(transaction, WriteUser).

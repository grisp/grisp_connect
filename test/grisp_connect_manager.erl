-module(grisp_connect_manager).

-compile([export_all, nowarn_export_all]).


-include_lib("common_test/include/ct.hrl").

start(Config) ->
    PrivDir = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, PrivDir),

    eresu:install([node()]),
    {ok, Started1} = application:ensure_all_started(eresu),
    register_user(),

    grisp_manager:install([node()]),

    application:start(mnesia),

    {ok, Started2} = application:ensure_all_started(kraft),

    {ok, Started3} = application:ensure_all_started(grisp_manager),
    Apps = Started1 ++ Started2 ++ Started3,
    [{apps, Apps} | Config].

kraft_start(CertDir) ->
    kraft_start(CertDir, #{}).

kraft_start(CertDir, OverrideOpts) ->
    SslOpts = [
        {verify, verify_peer},
        {keyfile, filename:join(CertDir, "server.key")},
        {certfile, filename:join(CertDir, "server.crt")},
        {cacertfile, filename:join(CertDir, "CA.crt")}
    ],
    Opts = #{
        port => 3030,
        ssl_opts => SslOpts,
        app => grisp_manager
    },
    KraftOpts = mapz:deep_merge(Opts, OverrideOpts),
    KraftRoutes = [
        {"/grisp-connect/ws",
            {ws, grisp_manager_device_api}, #{}, #{type => json_rpc}}
    ],
    kraft:start(KraftOpts, KraftRoutes).

cleanup_apps(Apps) ->
    mnesia:delete_table(eresu_user),
    mnesia:delete_table(grisp_device),
    mnesia:delete_table(grisp_manager_token),
    [application:stop(App) || App <- Apps],
    application:stop(mnesia).

link_device() ->
    grisp_manager:link_device(<<"0000">>, <<"Uuid">>).

register_user() ->
    Hash = erlpass:hash(<<"1234">>),
    WriteUser = fun() ->
                        mnesia:write({eresu_user,
                                      <<"Uuid">>,
                                      <<"Testuser">>,
                                      <<"foo">>,
                                      <<"a@a.a">>,
                                      erlang:system_time(),
                                      Hash,
                                      <<"Max Mustermann">>,
                                      undefined,
                                      undefined,
                                      <<"customer_id">>,
                                      []})
                end,
    mnesia:activity(transaction, WriteUser).

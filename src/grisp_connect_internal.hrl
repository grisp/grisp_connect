-ifndef(GRISP_CONNECT_INTERNAL_HRL).
-define(GRISP_CONNECT_INTERNAL_HRL, true).

-include_lib("kernel/include/logger.hrl").

-define(FORMAT(FMT, ARGS), iolist_to_binary(io_lib:format(FMT, ARGS))).

-define(GRISP_DEBUG(FMT, ARGS),
        ?LOG_DEBUG(FMT, ARGS)).
-define(GRISP_DEBUG(FMT, ARGS, REPORT),
        ?LOG_DEBUG(maps:put(description, ?FORMAT(FMT, ARGS), REPORT))).
-define(GRISP_INFO(FMT, ARGS),
        ?LOG_INFO(FMT, ARGS)).
-define(GRISP_INFO(FMT, ARGS, REPORT),
        ?LOG_INFO(maps:put(description, ?FORMAT(FMT, ARGS), REPORT))).
-define(GRISP_WARN(FMT, ARGS),
        ?LOG_WARNING(FMT, ARGS)).
-define(GRISP_WARN(FMT, ARGS, REPORT),
        ?LOG_WARNING(maps:put(description, ?FORMAT(FMT, ARGS), REPORT))).
-define(GRISP_ERROR(FMT, ARGS),
        ?LOG_ERROR(FMT, ARGS)).
-define(GRISP_ERROR(FMT, ARGS, REPORT),
        ?LOG_ERROR(maps:put(description, ?FORMAT(FMT, ARGS), REPORT))).

-endif. % GRISP_CONNECT_INTERNAL_HRL

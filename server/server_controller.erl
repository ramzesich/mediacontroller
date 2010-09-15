-module(server_controller).
-behaviour(supervisor).
-export([start/0, init/1]).

-include("definitions.hrl").


start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_FileName) ->
    Lsn = {lsn, {listener, start_link, []}, permanent, ?WORKER_KILL_TIME, worker, [listener]},
    Msn = {msn, {messenger, start_link, []}, permanent, ?WORKER_KILL_TIME, worker, [messenger]},
    {ok, {{one_for_all, ?RESTART_TIMES, ?RESTART_SECONDS}, [Lsn, Msn]}}.

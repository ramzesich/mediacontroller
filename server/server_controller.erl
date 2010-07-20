-module(server_controller).
-behaviour(supervisor).
-export([start/0, init/1]).

-include("definitions.hrl").


start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_FileName) ->
    Lsn = {lsn, {listener, start_link, []}, permanent, ?LISTENER_WORKER_KILL_TIME, worker, [listener]},
    {ok, {{one_for_all, ?LISTENER_RESTART_TIMES, ?LISTENER_RESTART_SECONDS}, [Lsn]}}.

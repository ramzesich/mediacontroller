-module(actions).
-export([
    ping/0,
    play/0,
    stop/0
]).
-vsn(0.1).

-include("definitions.hrl").


ping() ->
    messenger ! {ping, self()},
    gen_tcp:send(get(socket), <<"pong\n\n">>).


play() ->
    messenger ! {play, self()}.


stop() ->
    messenger ! {stop, self()}.

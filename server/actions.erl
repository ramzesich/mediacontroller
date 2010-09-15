-module(actions).
-export([
    login/0,
    logout/0,
    ping/0
]).
-vsn(0.1).

-include("definitions.hrl").


login() ->
    messenger ! {register, self()}.


logout() ->
    messenger ! {unregister, self()}.


ping() ->
    messenger ! {ping, self()},
    gen_tcp:send(get(socket), <<"pong\n\n">>).

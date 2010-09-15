-module(messenger).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2, code_change/3]).
-export([start_link/0, process/0]).
-vsn(0.1).

-include("definitions.hrl").


start_link() ->
    gen_event:start({local, messenger}),
    gen_event:add_handler(messenger, ?MODULE, []).


init([]) ->
    put(clients, []),
    put(linked, false),
    process().


process() ->
    receive
        {register, From} ->
            update(register, From);
        {unregister, From} ->
            update(unregister, From);
        {ping, From} ->
            update_time(From);
        {link, Pid} ->
            link(Pid),
            put(linked, true);
        {'EXIT', Pid, Reason} ->
            put(linked, false),
            error_logger:warning_msg("linked messenger ~p has died: ~p~n", [Pid, Reason]);
        _Unknown ->
            error_logger:warning_msg("unknown message received: ~p~n", [_Unknown])
    after (?MSN_SESSION_SECONDS * 1000) ->
        error_logger:warning_msg("no messages during last ~p seconds~n", [?MSN_SESSION_SECONDS])
    end,
    %error_logger:info_msg("cleaning the clients list~n"),
    %clean_lists(),
    error_logger:info_msg("clients: ~p~n", [get(clients)]),
    ?MODULE:process().


broadcast(Message) ->
    Send = fun({{Pid, _Id}, _}) ->
        Pid ! Message
    end,
    lists:map(Send, get(clients)).


update(register, From) ->
    put(clients, [{From, calendar:local_time()} | get(clients)]),
    error_logger:info_msg("client ~p added~n", [From]);
update(unregister, From) ->
    put(clients, proplists:delete(From)),
    error_logger:info_msg("client ~p removed~n", [From]).


update_time(Key) ->
    put(clients, [{Key, calendar:local_time()} | proplists:delete(Key, get(clients))]).


clean_lists() ->
    lists:map(fun(Type) ->
        put(Type, [{Key, DateTime} || {Key, DateTime} <- get(Type), (calendar:datetime_to_gregorian_seconds(calendar:local_time()) - calendar:datetime_to_gregorian_seconds(DateTime)) < ?MSN_SESSION_SECONDS])
    end,
    [clients]).


handle_event(_ErrorMsg, _Fd) ->
    ok.


terminate(_Args, _Fd) ->
    ok.


handle_call(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

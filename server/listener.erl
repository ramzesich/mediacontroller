-module(listener).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([start_link/0, pair_connect/1, process_messages/1]).
-vsn(0.1).

-include("definitions.hrl").


start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    case gen_tcp:listen(?TCP_PORT, [
            binary,
            {packet, raw},
            {reuseaddr, true},
            {active, true}
    ]) of
        {ok, ListenSocket} ->
            spawn(?MODULE, pair_connect, [ListenSocket]);
        {error, Message} ->
            error_logger:error_msg("Couldn't listen: ~p~n", [Message])
    end,
    {ok, null}.


pair_connect(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(?MODULE, pair_connect, [ListenSocket]),
            put(socket, Socket),
            process_messages(<<>>);
        {error, closed} ->
            error_logger:error_msg("Listening socket is closed~n");
        {error, timeout} ->
            error_logger:error_msg("Listening socket timed out~n");
        _Whatever ->
            error_logger:error_msg("The following socket error occured: ~p~n", [_Whatever])
    end.


process_messages(Acc) ->
    receive
        {tcp, _Port, Data} ->
            error_logger:info_msg("received a message: ~p~n", [Data]),
            Parts = re:split(<<Acc/binary, Data/binary>>, "\n\n"),
            if
                length(Parts) > 1 ->
                    [Head | Tail] = lists:reverse(Parts),
                    lists:map(fun(Part) -> parse({data, Part}) end, lists:reverse(Tail)),
                    ?MODULE:process_messages(Head);
                length(Parts) =:= 1 ->
                    [Part] = Parts,
                    ?MODULE:process_messages(Part);
                true ->
                    error_logger:warning_msg("something terrible happened while parsing the following message: ~p~n", [Parts]),
                    ?MODULE:process_messages(<<>>)
            end;
        _Whatever ->
            error_logger:warning_msg("some garbage arrived: ~p~n", [_Whatever]),
            ?MODULE:process_messages(<<>>)
    end.


parse({data, Bin}) ->
    case [X || X <- re:split(Bin, <<"\n">>), X =/= <<>>] of
        [] ->
            error_logger:warning_msg("empty action~n");
        List ->
            [Action | PL] = List,
            _Params = [list_to_tuple(re:split(Param, <<": ">>)) || Param <- PL],
            case Action of
                <<"login">> ->
                    error_logger:info_msg("logging in... ~n");
                _Whatever ->
                    error_logger:warning_msg("unknown action requested: ~p~n", [_Whatever])
            end
    end.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

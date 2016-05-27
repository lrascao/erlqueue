%%%-------------------------------------------------------------------
%%% File    : big.erl
%%% Author  : Rickard Green <>
%%% Description : A simple message passing benchmark
%%%
%%% Created : 30 Dec 2005 by Rickard Green <>
%%%-------------------------------------------------------------------
-module(big).

-export([bang/1]).

pinger([], [], true) ->
    receive
        {procs, Procs, ReportTo} ->
            pinger(Procs, [], ReportTo)
    end;
pinger([], [], false) ->
    receive {ping, From} -> From ! {pong, self()} end,
    pinger([],[],false);
pinger([], [], ReportTo) ->
    ReportTo ! {done, self()},
    pinger([],[],false);
pinger([],[Po|Pos] = Pongers, ReportTo) ->
    receive
        {ping, From} ->
            From ! {pong, self()},
            pinger([], Pongers, ReportTo);
        {pong, Po} ->
            pinger([], Pos, ReportTo)
    end;
pinger([Pi|Pis], Pongers, ReportTo) ->
    receive
        {ping, From} -> From ! {pong, self()}
    after 0 -> ok
    end,
    Pi ! {ping, self()},
    pinger(Pis, [Pi|Pongers], ReportTo).

spawn_procs(N) when N =:= 0 ->
    [];
spawn_procs(N) ->
    [spawn_link(fun () -> pinger([],[],true) end) | spawn_procs(N - 1)].

send_procs([], Msg) ->
    Msg;
send_procs([P|Ps], Msg) ->
    P ! Msg,
    send_procs(Ps, Msg).

receive_msgs([]) ->
    ok;
receive_msgs([M|Ms]) ->
    receive
        M ->
            receive_msgs(Ms)
    end.

bang(N) when is_integer(N) ->
    Procs = spawn_procs(N),
    RMsgs = lists:map(fun (P) ->
                        {done, P}
                      end, Procs),
    Start = now(),
    send_procs(Procs, {procs, Procs, self()}),
    receive_msgs(RMsgs),
    Stop = now(),

    lists:foreach(fun (P) -> exit(P, normal) end, Procs),
    timer:now_diff(Stop, Start).

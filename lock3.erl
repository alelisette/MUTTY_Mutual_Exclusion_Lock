-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            LamportClock = 0,
            open(MyId, Nodes, LamportClock);
        stop ->
            ok
    end.

open(MyId, Nodes, LamportClock) ->
    receive
        {take, Master, Ref} ->
            TakeTime = LamportClock,
            Refs = requests(MyId, Nodes, TakeTime),
            wait(Nodes, Master, Refs, [], Ref, MyId, TakeTime, LamportClock);
        {request, From, Ref, _, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
            From ! {ok, Ref, LamportClock2},
            open(MyId, Nodes, LamportClock2);
        stop ->
            ok
    end.

requests(MyId, Nodes, TakeTime) ->
    lists:map(
        fun(P) ->
            R = make_ref(),
            P ! {request, self(), R, MyId, TakeTime},
            {P, R}
        end,
        Nodes
    ).

wait(Nodes, Master, [], Waiting, TakeRef, MyId, _, LamportClock) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId, LamportClock);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, TakeTime, LamportClock) ->
    receive
        {request, From, Ref, ReqID, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
            if
                TakeTime < ExtClock ->
                    wait(Nodes, Master, Refs, [{From, Ref} | Waiting], TakeRef, MyId, TakeTime, LamportClock2);
                TakeTime > ExtClock ->
                    NewRefs = resendRequest(From, Refs, MyId, LamportClock2),
                    From ! {ok, Ref, LamportClock2},
                    wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, TakeTime, LamportClock2);
                TakeTime == ExtClock ->
                    if
                        ReqID < MyId ->
                            NewRefs = resendRequest(From, Refs, MyId, LamportClock2),
                            From ! {ok, Ref, LamportClock2},
                            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, TakeTime, LamportClock2);
                        true ->
                            wait(Nodes, Master, Refs, [{From, Ref} | Waiting], TakeRef, MyId, TakeTime, LamportClock2)
                    end
            end;
        {ok, Ref, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
            NewRefs = lists:keydelete(Ref, 2, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, TakeTime, LamportClock2);
        release ->
            ok(Waiting, LamportClock),
            open(MyId, Nodes, LamportClock)
    end.

ok(Waiting, LamportClock) ->
    lists:map(
        fun({F, R}) ->
            F ! {ok, R, LamportClock}
        end,
        Waiting
    ).

held(Nodes, Waiting, MyId, LamportClock) ->
    receive
        {request, From, Ref, _, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
            held(Nodes, [{From, Ref} | Waiting], MyId, LamportClock2);
        release ->
            ok(Waiting, LamportClock),
            open(MyId, Nodes, LamportClock)
    end.

resendRequest(From, Refs, MyId, LamportClock) ->
    IsPidPresent = lists:keymember(From, 1, Refs),
    IsNamePresent = lists:keymember(process_info(From), 1, Refs),
    if
        IsPidPresent or IsNamePresent ->
            Refs;
        true ->
            R = make_ref(),
            NewRefs = [R | Refs],
            From ! {request, self(), R, MyId, LamportClock},
            NewRefs
    end.

updateClock(MyClock, ExtClock) ->
    if
        MyClock >= ExtClock ->
            NewClock = MyClock + 1;
        MyClock < ExtClock ->
            NewClock = ExtClock + 1
    end,
    NewClock.

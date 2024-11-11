-module(lock2).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(MyId, Nodes);
        stop ->
            ok
    end.

open(MyId, Nodes) ->
    receive
        {take, Master, Ref} ->
            Refs = requests(MyId, Nodes),
            wait(Nodes, Master, Refs, [], Ref, MyId);
        {request, From, Ref, _} ->
            From ! {ok, Ref},
            open(MyId, Nodes);
        stop ->
            ok
    end.

requests(MyId, Nodes) ->
    lists:map(
        fun(P) ->
            R = make_ref(),
            P ! {request, self(), R, MyId},
            R
        end,
        Nodes
    ).

wait(Nodes, Master, [], Waiting, TakeRef, MyId) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId) ->
    receive
        {request, From, Ref, ReqID} ->
            if
                ReqID < MyId ->
                    R = make_ref(),
                    NewRefs = [R | Refs],
                    From ! {request, self(), R, MyId},
                    From ! {ok, Ref},
                    wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId);
                true ->
                    wait(Nodes, Master, Refs, [{From, Ref} | Waiting], TakeRef, MyId)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId);
        release ->
            ok(Waiting),
            open(MyId, Nodes)
    end.

ok(Waiting) ->
    lists:map(
        fun({F, R}) ->
            F ! {ok, R}
        end,
        Waiting
    ).

held(Nodes, Waiting, MyId) ->
    receive
        {request, From, Ref, _} ->
            held(Nodes, [{From, Ref} | Waiting], MyId);
        release ->
            ok(Waiting),
            open(MyId, Nodes)
    end.

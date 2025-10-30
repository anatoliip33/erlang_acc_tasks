-module(counter_proc).
-export([start/0, start/1, stop/1]).

start() ->
    start(0).

start(Initial) when is_integer(Initial)->
    spawn(fun() -> loop(Initial) end);

start(_Initial) ->
    {error, badarg}.

stop(Pid) ->
    Pid ! stop,
    ok.

loop(Count) ->
    receive
        {From, ping} ->
            From ! pong,
            loop(Count);

        {From, {inc, N}} when is_integer(N) ->
            NewCount = Count + N,
            From ! ok,
            loop(NewCount);
        
        {From, get} ->
            From ! {ok, Count},
            loop(Count); 
            
        {From, stop} ->
            From ! stopped,
            exit(normal);
        
        {From, _Other} ->
            From ! {error, unsupported},
            loop(Count)
    end.
    
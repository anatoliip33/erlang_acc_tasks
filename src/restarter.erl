-module(restarter).
-export([start_link/1, status/1, stop/1]).

-record(state, {
    mfa,
    child,
    status
}).

start_link(MFA) ->
    spawn_link(fun() -> init(MFA) end).

status(Pid) ->
    Pid ! {self(), status},
    receive
        {ok, ChildPid} ->
            {ok, ChildPid};

        {restarting, Reason} ->
            {restarting, Reason}

    after 10000 ->
        {error, timeout}
    end.

stop(Pid) ->
    Pid ! {self(), stop},
    receive
        ok ->
            ok
    after 5000 ->
        {error, timeout}
    end.

init({Module, Function, Args} = MFA) ->
    io:format("Parent ~p spawning child~n", [self()]),

    %% Spawn process and monitor them
    {ChildPid, Ref} = spawn_monitor(Module, Function, Args),
    
    io:format("Monitoring child ~p with ref ~p~n", [ChildPid, Ref]),

    State = #state{mfa = MFA, child = {ChildPid, Ref}, status = {ok, nil}},

    watcher_loop(State).

watcher_loop(State = #state{mfa = {Module, Function, Args}, child = {ChildPid, RefMon}}) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Worker ~p (ref: ~p) died: ~p~n", [Pid, Ref, Reason]),

            NewState = State#state{status = {restarting, Reason}},

            erlang:send_after(5000, self(), spawn_child),
            watcher_loop(NewState);
        
        spawn_child ->    
            {NewChildPid, NewRef} = spawn_monitor(Module, Function, Args),
            NewState = State#state{child = {NewChildPid, NewRef}, status = {ok, nil}},
            watcher_loop(NewState);

        {From, status} ->
            io:format("Current Child PID: ~p~n", [ChildPid]),
            case is_process_alive(ChildPid) of
                true ->
                    From ! {ok, ChildPid};

                false ->
                    #state{status = {restarting, Reason}} = State,
                    From ! {restarting, Reason}           
            end,

            watcher_loop(State); 

        {From, stop} ->
            io:format("Received stop request from ~p~n", [From]),
            %% Stop the child and demonitor our monitor, then exit cleanly.
            exit(ChildPid, normal),
            %% ensure we remove the monitor to avoid dangling refs
            demonitor(RefMon, [flush]),
            From ! ok,
            %% terminate the watcher process
            exit(normal)    
    end.
-module(loop_recursion).

-export([loop_tail/1, loop_not_tail/1, loop_try/1, run/1]).

loop_tail(N) ->
    loop_tail(N, 0).

%% A normal tail-recursive loop (should be optimized, no stack growth).
loop_tail(0, Acc) ->
    Acc;
loop_tail(N, Accu) ->
    loop_tail(N - 1, Accu + 1).

%% A normal non tail-recursive loop (should not be optimized).
loop_not_tail(0) ->
    0;
loop_not_tail(N) ->
    1 + loop_not_tail(N - 1).

loop_try(N) ->
    loop_try(N, 0).
%% The same logic as loop_tail, but the recursive tail call is placed inside a `try`.
%% This prevents tail-call optimisation on BEAM (stack grows / will eventually crash).
loop_try(0, Accu) ->
    Accu;
loop_try(N, Accu) ->
    try
        loop_try(N - 1, Accu + 1)
    catch
        Class:Reason ->
            %% re-raise so run/0 can print it
            erlang:error(Class, Reason)
    end.

format_tc_result(Message, {Time, Result}) ->
    io:format("~s: ~p in ~p microseconds~n", [
        Message,
        Result,
        Time
    ]).

%% Run all functions and print what happens.
run(N) ->
    lists:foreach(
        fun(Idx) ->
            io:format("~n~nAttempt ~p ...~n", [Idx]),

            io:format("Running loop_tail(~p) ...~n", [N]),
            format_tc_result(
                "Result",
                timer:tc(fun() ->
                    loop_tail(N, 0)
                end)
            ),

            io:format("~n", []),

            io:format("Running loop_try(~p) ...~n", [N]),
            format_tc_result(
                "Result",
                timer:tc(fun() ->
                    loop_try(N, 0)
                end)
            ),

            io:format("~n", []),

            io:format("Running loop_not_tail(~p) ...~n", [N]),
            format_tc_result(
                "Result",
                timer:tc(fun() ->
                    loop_not_tail(N)
                end)
            )
        end,
        lists:seq(1, 3)
    ).
-module(learn_recursion).

-export([
    length_head/1,
    length_tail/1
]).

length_tail(Xs) ->
    length_tail(Xs, 0).

length_tail([], Len) ->
    Len;

length_tail([_ | Xs], Len) ->
    length_tail(Xs, Len + 1).


length_head([]) ->
    0;

length_head([_ | Xs]) ->
    1 + length_head(Xs).

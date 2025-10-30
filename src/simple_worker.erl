%% A minimal long-running worker used to test the restarter.
-module(simple_worker).
-export([worker/1]).

worker(Count) ->
    timer:sleep(1000),
    worker(Count + 1).
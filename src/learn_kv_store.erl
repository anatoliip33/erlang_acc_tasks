-module(learn_kv_store).

-export([new/0,
         add/3,
         update/3,
         remove/2,
         fetch/2,
         list_keys_sorted/1,
         ops_count/1,
         reset_counter/1]).

new() ->
    Ref = atomics:new(1, []),
    {#{}, Ref}.

add({Map, Ref}, Key, Value) ->
    NewMap = maps:put(Key, Value, Map),
    inc_ops(Ref),
    {{NewMap, Ref}, ok}.

update({Map, Ref} = State, Key, Value) ->
    case maps:is_key(Key, Map) of
        true ->
            NewMap = maps:put(Key, Value, Map),
            inc_ops(Ref),
            {{NewMap, Ref}, ok};
        false ->
            {State, {error, not_found}}
    end.    

remove({Map, Ref} = State, Key) ->
    case maps:is_key(Key, Map) of
        true ->
            NewMap = maps:remove(Key, Map),
            inc_ops(Ref),
            {{NewMap, Ref}, ok};
        false ->
            {State, {error, not_found}}
    end.

fetch({Map, Ref}, Key) ->
    inc_ops(Ref),
    case Map of
       #{Key := Value} ->
           {Value, ok};
       #{} ->
           {error, not_found}
   end.
  
list_keys_sorted({Map, Ref} = State) ->
    Keys = maps:keys(Map),
    Sorted = lists:sort(Keys),
    inc_ops(Ref),
    {State, Sorted}.

ops_count({_Map, Ref}) ->
    atomics:get(Ref, 1).
  
reset_counter({_Map, Ref}) ->
    atomics:put(Ref, 1, 0),
    ok.    

inc_ops(Ref) ->
  atomics:add(Ref, 1, 1),
  ok.   
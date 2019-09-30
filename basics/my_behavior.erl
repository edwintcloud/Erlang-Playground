-module(my_behavior).

-export([behavior_info/1]).

behavior_info(callbacks) ->
    [{init, 1}, {a_function, 0}, {other, 3}];
behavior_info(_) -> undefined.

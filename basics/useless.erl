%%% native uses hipe to compile platform specific code
%%% which should not be used unless the ~20% performance
%%% is absolutely needed
-compile([debug_info, native]).

-module(useless).

-author("Edwin Cloud").

-export([add/2, greet_and_add_two/1, hello/0]).

%% define macros below
-define(HELLO, "Hello world!~n").

-define(print(V), io:format(V)).

add(A, B) -> A + B.

%% Shows greeting.
%%
%% usage:
%%    hello()
hello() -> ?print((?HELLO)).

greet_and_add_two(X) -> hello(), add(X, 2).

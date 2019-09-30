%%% functions contains methods
%%% to do blah blah blah
-module(functions).

-author("Edwin Cloud").

-compile([debug_info]).

-export([head/1, same/2, second/1, valid_datetime/1]).

%% Return the head of a list.
%%
%% usage:
%%    head([1,2]) -> 1
head([H | _]) -> H.

%% Return the second value in a list.
%%
%% usage:
%%    second([1,2,3,4]) -> 2
second([_, S | _]) -> S.

%% Return true if two values are the same
%% and false otherwise.
%%
%% usage:
%%    same(b, a) -> false
same(V, V) -> true;
same(_, _) -> false.

%% Print the provided date and time if formatted
%% correctly.
%%
%% usage:
%%    valid_datetime({2019, 09, 23}, {15, 45, 30}) ->
%%  15:45:30 on 09/23/2019
valid_datetime({{Y, M, D}, {H, Mn, S}}) ->
    io:format("~p:~p:~p on ~p/~p/~p~n",
	      [H, Mn, S, M, D, Y]);
valid_datetime(_) -> io:format("go f() yourself ").

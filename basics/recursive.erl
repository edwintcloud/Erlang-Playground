-module(recursive).

-author("Edwin Cloud").

-compile([debug_info]).

-export([duplicate/2, factorial/1, len/1, quicksort/1,
	 reverse/1, sublist/2, wee_quicksort/1, zip/2]).

%% Return N!.
%%
factorial(N) -> factorial(N, 1).

factorial(N, Acc) when N == 0 -> Acc;
factorial(N, Acc) when N > 0 ->
    factorial(N - 1, N * Acc).

%% Return length of L.
%%
len(L) -> len(L, 0).

len([], Acc) -> Acc;
len([_ | T], Acc) -> len(T, 1 + Acc).

%% Reverse a list.
%%
reverse(L) -> reverse(L, []).

reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).

%% Return a list with N copies of T.
%%
duplicate(N, T) -> duplicate(N, T, []).

duplicate(0, _, Acc) -> Acc;
duplicate(N, T, Acc) when N > 0 ->
    duplicate(N - 1, T, [T | Acc]).

%% Return N elements from L.
%%
sublist(L, N) -> sublist(L, N, []).

sublist(_, 0, Acc) -> Acc;
sublist([], _, Acc) -> Acc;
sublist([H | T], N, Acc) when N > 0 ->
    reverse(sublist(T, N - 1, [H | Acc])).

%% Zip two lists together into a list of tuples.
%%
zip(A, B) -> reverse(zip(A, B, [])).

zip([], [], Acc) -> Acc;
zip([X | Xs], [Y | Ys], Acc) ->
    zip(Xs, Ys, [{X, Y} | Acc]).

%% Naive quicksort.
%%
quicksort([]) -> [];
quicksort([H | T]) ->
    {S, L} = partition(H, T, [], []),
    quicksort(S) ++ [H] ++ quicksort(L).

partition(_, [], S, L) -> {S, L};
partition(P, [H | T], S, L) ->
    if H =< P -> partition(P, T, [H | S], L);
       H > P -> partition(P, T, S, [H | L])
    end.

%% More concise, naive quicksort. We can use list
%% comprehension instead of a partition func.
%%
wee_quicksort([]) -> [];
wee_quicksort([H | T]) ->
    wee_quicksort([S || S <- T, S =< H]) ++
      [H] ++ wee_quicksort([L || L <- T, L > H]).

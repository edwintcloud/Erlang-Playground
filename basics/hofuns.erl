%%% Higher Order Functions
-module(hofuns).

-author("Edwin Cloud").

-compile([debug_info]).

-export([filter/2, map/2, reduce/3]).

%% Reduce a list using the given function.
reduce(_, Start, []) -> Start;
reduce(F, Start, [H | T]) -> reduce(F, F(H, Start), T).

%% Filter a list using the given predicate.
filter(Pred, L) ->
    F = fun (X, Acc) ->
		case Pred(X) of
		  true -> [X | Acc];
		  false -> Acc
		end
	end,
    lists:reverse(reduce(F, [], L)).

%% Map over a list, applying the given function
%% to each term.
map(F, L) ->
    lists:reverse(reduce(fun (X, Acc) -> [F(X) | Acc] end,
			 [], L)).

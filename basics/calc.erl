%%% Reverse Polish Notation Calculator
%%%

-module(calc).

-author("Edwin Cloud").

-compile([debug_info]).

-export([rpn/1, rpn_test/0]).

%% Main function to calculate the result.
%%
rpn(L) when is_list(L) ->
    [Result] = lists:foldl(fun rpn/2, [],
			   string:tokens(L, " ")),
    Result.

rpn("+", [N1, N2 | Rest]) ->
    [N2 + N1 | Rest]; % addition
rpn("-", [N1, N2 | Rest]) ->
    [N2 - N1 | Rest]; % subtraction
rpn("/", [N1, N2 | Rest]) ->
    [N2 / N1 | Rest]; % division
rpn("*", [N1, N2 | Rest]) ->
    [N2 * N1 | Rest]; % multiplication
rpn("^", [N1, N2 | Rest]) ->
    [math:pow(N2, N1) | Rest]; % power of
rpn("ln", [N | Rest]) -> [math:log(N) | Rest]; % log n
rpn("log10", [N | Rest]) ->
    [math:log10(N) | Rest]; % log 10
rpn(X, Stack) -> [read(X) | Stack]. % add X to stack

%% Convert S to an integer or float.
read(S) ->
    case string:to_float(S) of
      {error, _} -> list_to_integer(S);
      {F, _} -> F % catch-all
    end.

%%% Tests

rpn_test() ->
    10 = rpn("3 7 +"),
    88 = rpn("90 1 - 1 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try rpn("22 1 + *") catch error:_ -> ok end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(3.2) == rpn("3.2 log10"),
    ok.

-module(tree).

-author("Edwin Cloud").

-compile([debug_info]).

-export([empty/0, insert/3, lookup/2]).

%% Return an empty tree.
%%
empty() -> {node, nil}.

%% Insert a node into the tree.
%%
insert(K, V, {node, nil}) ->
    {node, {K, V, {node, nil}, {node, nil}}};
insert(Nk, Nv, {node, {K, V, S, L}}) when Nk < K ->
    {node, {K, V, insert(Nk, Nv, S), L}};
insert(Nk, Nv, {node, {K, V, S, L}}) when Nk > K ->
    {node, {K, V, S, insert(Nk, Nv, L)}};
insert(K, V, {node, {K, _, S, L}}) ->
    {node, {K, V, S, L}}.

%% Lookup a node in the tree.
%%
lookup(_, {node, nil}) -> undefined;
lookup(K, {node, {K, V, _, _}}) -> {ok, V};
lookup(K, {node, {Nk, _, S, _}}) when K < Nk ->
    lookup(K, S);
lookup(K, {node, {_, _, _, L}}) -> lookup(K, L).

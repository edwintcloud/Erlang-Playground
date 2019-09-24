-module(cases).

-author("Edwin Cloud").

-compile([debug_info]).

-export([insert/2]).

%% Naive set.add using lists
insert(X, []) -> [X]; % empty list? just return [X]
insert(X, Set) ->
    %% is X a member of the set?
    case lists:member(X, Set) of
      true -> Set; % just return the set
      false -> [X | Set] % add X to the head of the list
    end.

%%% guards
%%%
-module(guards).

-author("Edwin Cloud").

-compile([debug_info]).

-export([old_enough/1]).

%% Return true if input is greater than or equal to 16
%% andalso less that or equal to 104.
%%
old_enough(X) when X >= 16, X =< 104 -> true;
old_enough(_) -> false.

%%% A module demonstrating message passing.
%%%
-module(dolphin).

-author("Edwin Cloud").

-compile([debug_info]).

-export([read/0, reply/0]).

read() ->
    receive
      do_a_flip -> io:format("no free flips~n");
      fish -> io:format("yum my fav~n");
      _ -> io:format("Heh, we're smarter than you humans.~n")
    end.

reply() ->
    receive
      {From, do_a_flip} -> From ! "Naaaaa", reply();
      {From, fish} -> From ! "Aww shucks";
      _ ->
	  io:format("Hehe, we're smarter than you human. ~n"),
	  reply()
    end.

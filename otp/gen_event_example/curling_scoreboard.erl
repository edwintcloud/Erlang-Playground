-module(curling_scoreboard).

-behavior(gen_event).

-export([code_change/3, handle_call/2, handle_event/2,
	 handle_info/2, init/1, terminate/2]).

init([]) -> {ok, []}.

handle_event({set_teams, A, B}, State) ->
    curling_scoreboard_hw:set_teams(A, B), {ok, State};
handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard_hw:add_point(Team)
     || _ <- lists:seq(1, N)],
    {ok, State};
handle_event(next_round, State) ->
    curling_scoreboard_hw:next_round(), {ok, State};
handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVer, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

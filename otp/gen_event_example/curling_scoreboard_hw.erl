-module(curling_scoreboard_hw).

-export([add_point/1, next_round/0, reset_board/0,
	 set_teams/2]).

set_teams(A, B) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [A, B]).

next_round() -> io:format("Scoreboard: round over~n").

add_point(Team) ->
    io:format("Scoreboard: increase score of team ~s "
	      "by 1~n",
	      [Team]).

reset_board() ->
    io:format("Scoreboard: All teams are undefined "
	      "and all scores are 0~n").

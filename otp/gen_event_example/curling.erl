-module(curling).

-export([add_points/3, next_round/1, set_teams/3,
	 start_link/2]).

-export([join_feed/2, leave_feed/2]).

-export([game_info/1]).

start_link(A, B) ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, curling_scoreboard, []),
    gen_event:add_handler(Pid, curling_accumulator, []),
    set_teams(Pid, A, B),
    {ok, Pid}.

set_teams(Pid, A, B) ->
    gen_event:notify(Pid, {set_teams, A, B}).

add_points(Pid, Team, N) ->
    gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) -> gen_event:notify(Pid, next_round).

join_feed(Pid, ToPid) ->
    HandlerId = {curling_feed, make_ref()},
    gen_event:add_handler(Pid, HandlerId, [ToPid]),
    HandlerId.

leave_feed(Pid, HandlerId) ->
    gen_event:delete_handler(Pid, HandlerId, leave_feed).

game_info(Pid) ->
    gen_event:call(Pid, curling_accumulator, game_data).

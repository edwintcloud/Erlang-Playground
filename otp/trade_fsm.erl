%%% A Finite State Machine example of an
%%% in-game trade system.

-module(trade_fsm).

-behavior(gen_fsm).

%% public API
-export([accept_trade/1, cancel/1, make_offer/2,
	 ready/1, retract_offer/2, start/1, start_link/1,
	 trade/2]).

%% gen_fsm callbacks - standard
-export([code_change/4, handle_event/3, handle_info/3,
	 handle_sync_event/4, init/1, terminate/3]).

%% gen_fsm callbacks - custom state names
-export([idle/2, idle/3, idle_wait/2, idle_wait/3,
	 negotiate/2, negotiate/3, ready/2, ready/3, wait/2]).

%%% RECORDS

-record(state,
	{name = "", other, ownitems = [], otheritems = [],
	 monitor, from}).

%%% PUBLIC API

start(Name) -> gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% Ask OtherPid to initiate a trade. Return if/when accepted.
trade(Pid, OtherPid) ->
    gen_fsm:sync_send_event(Pid, {negotiate, OtherPid},
			    30000). % 30 sec timeout

%% Accept a trade offer.
accept_trade(Pid) ->
    gen_fsm:sync_send_event(Pid, accept_negotiate).

%% Send an Item on the table to be traded.
make_offer(Pid, Item) ->
    gen_fsm:send_event(Pid, {make_offer, Item}).

%% Cancel pending offer for Item.
retract_offer(Pid, Item) ->
    gen_fsm:send_event(Pid, {retract_offer, Item}).

%% Confirm your offers in preparation for a trade. Once the
%% other player has also declared ready, the trade will be
%% complete.
ready(Pid) ->
    gen_fsm:sync_send_event(Pid, ready,
			    infinity). % change default timeout of 5 secs to inf

%% Cancel the entire transaction.
cancel(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, cancel).

%%% FSM to FSM Functions

%% Ask OtherPid if they are available for a trade.
ask_negotiate(OtherPid, Pid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, Pid}).

%% Foward the client message, accepting the request to trade.
accept_negotiate(OtherPid, Pid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, Pid}).

%% Foward a client's offer.
do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

% Foward a client's offer cancellation.
undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% Ask OtherPid if ready to trade.
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

%% Reply not ready (i.e. not in 'wait' state).
not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

%% Reply ready (signals other FSM, state should
%% transition to 'ready!').
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

%% Acknowledge that FSM is in ready state.
ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

%% Ask if ready to commit.
ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

%% Begin synchronous commit.
do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

%% Notify other FSM that the trade was cancelled.
notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).

%%% GEN_FSM CALLBACKS - STANDARD

init(Name) -> {ok, idle, #state{name = Name}}.

handle_event(cancel, _StateName, S = #state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName,
		  S = #state{}) ->
    notify_cancel(S#state.other),
    notice(S, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _,
	    S = #state{other = Pid, monitor = Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVer, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

terminate(normal, ready, S = #state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) -> ok.

%%% GEN_FSM CALLBACKS - CUSTOM STATE NAMES

%% IDLE
idle({ask_negotiate, OtherPid}, S = #state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked from a trade negotiation",
	   [OtherPid]),
    {next_state, idle_wait,
     S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
    unexpected(Event, idle), {next_state, idle, Data}.

%% IDLE FROM
idle({negotiate, OtherPid}, From, S = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait,
     S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle), {next_state, idle, Data}.

%% IDLE_WAIT
idle_wait({ask_negotiate, OtherPid},
	  S = #state{other = OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid},
	  S = #state{other = OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

%% IDLE_WAIT FROM
idle_wait(accept_negotiate, _From,
	  S = #state{other = OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "accepting negotiation", []),
    {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

%% NEGOTIATE
negotiate({make_offer, Item},
	  S = #state{ownitems = OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate,
     S#state{ownitems = add(Item, OwnItems)}};
negotiate({retract_offer, Item},
	  S = #state{ownitems = OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate,
     S#state{ownitems = remove(Item, OwnItems)}};
negotiate({do_offer, Item},
	  S = #state{otheritems = OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate,
     S#state{otheritems = add(Item, OtherItems)}};
negotiate({undo_offer, Item},
	  S = #state{otheritems = OtherItems}) ->
    notice(S, "other player cancelling offer on ~p",
	   [Item]),
    {next_state, negotiate,
     S#state{otheritems = remove(Item, OtherItems)}};
negotiate(are_you_ready,
	  S = #state{other = OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(S,
	   "Other user ready to transfer goods: "
	   "~nYou get ~p, The other side gets ~p ",
	   [S#state.otheritems, S#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

%% NEGOTIATE FROM
negotiate(ready, From, S = #state{other = OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from = From}};
negotiate(Event, _From, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

%% WAIT
wait({do_offer, Item},
     S = #state{otheritems = OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate,
     S#state{otheritems = add(Item, OtherItems)}};
wait({undo_offer, Item},
     S = #state{otheritems = OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "Other side cancelling offer of ~p", [Item]),
    {next_state, negotiate,
     S#state{otheritems = remove(Item, OtherItems)}};
wait(are_you_ready, S = #state{}) ->
    am_ready(S#state.other),
    notice(S,
	   "asked if ready, and I am. Waiting for "
	   "same reply",
	   []),
    {next_state, wait, S};
wait(not_yet, S = #state{}) ->
    notice(S, "Other not ready yet", []),
    {next_state, wait, S};
wait('ready!', S = #state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S,
	   "other side is ready. Moving to ready "
	   "state",
	   []),
    {next_state, ready, S};
wait(Event, Data) ->
    unexpected(Event, wait), {next_state, wait, Data}.

%% READY
ready(ack, S = #state{}) ->
    case priority(self(), S#state.other) of
      true ->
	  try notice(S, "asking for commit", []),
	      ready_commit = ask_commit(S#state.other),
	      notice(S, "ordering commit", []),
	      ok = do_commit(S#state.other),
	      notice(S, "committing...", []),
	      commit(S),
	      {stop, normal, S}
	  catch
	    Class:Reason ->
		notice(S, "commit failed", []),
		{stop, {Class, Reason}, S}
	  end;
      false -> {next_state, ready, S}
    end;
ready(Event, Data) ->
    unexpected(Event, ready), {next_state, ready, Data}.

%% READY FROM
ready(ask_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "committing...", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _From, Data) ->
    unexpected(Event, ready), {next_state, ready, Data}.

%%% PRIVATE FUNCTIONS

%% Send players a notice.
notice(#state{name = N}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [N | Args]).

%% Log unexpected messages.
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in "
	      "state ~p~n",
	      [self(), Msg, State]).

%% Add an Item to an Items list.
add(Item, Items) -> [Item | Items].

%% Remove an Item from an Items list.
remove(Item, Items) -> Items -- [Item].

%% Rank Pid by priority (to establish a commit order).
priority(Pid, OtherPid) when Pid > OtherPid -> true;
priority(Pid, OtherPid) when Pid < OtherPid -> false.

%% Commit the transaction (completing the trade). Realistically
%% a third-party (other process) should validate the exchange is
%% valid at this point and if not - rollback everything.
commit(S = #state{}) ->
    io:format("Transaction completed for ~s.Items sent "
	      "are: ~n~p, ~n received are: ~n~p. ~nThis "
	      "operation should have some atomic save "
	      "ina database.~n",
	      [S#state.name, S#state.ownitems, S#state.otheritems]).

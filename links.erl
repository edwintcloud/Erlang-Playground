-module(links).

-compile(export_all).

chain(0) ->
    receive
      _ -> ok after 2000 -> exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun () -> chain(N - 1) end),
    link(Pid),
    receive _ -> ok end.

start_critic() -> spawn(?MODULE, critic, []).

start_critic2() -> spawn(?MODULE, restarter, []).

%% supervisor process
restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid), % give the process a name
    receive
      {'EXIT', Pid, normal} -> ok; % normal exit
      {'EXIT', Pid, shutdown} -> ok; % manual termination
      {'EXIT', Pid, _} ->
	  restarter() % he's dead jim, restart em'
    end.

critic() ->
    receive
      {From, Ref,
       {"Rage Against the Turing Machine", "Unit Testify"}} ->
	  From ! {Ref, "Now you do what they told you"};
      {From, Ref, {"System of a Downtime", "Memoize"}} ->
	  From !
	    {Ref,
	     "When you loose small mind, you free "
	     "your life"};
      {From, Ref,
       {"Breaking Bandwith", "Break My Internet"}} ->
	  From ! {Ref, "And we'll grow, till we hit the ceiling"};
      {From, Ref, {_, _}} ->
	  From ! {Ref, "New song, who dis?"}
    end,
    critic().

judge(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
      {Ref, Criticism} -> Criticism after 2000 -> timeout
    end.

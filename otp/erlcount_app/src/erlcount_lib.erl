-module(erlcount_lib).

-export([find_erl/1, regex_count/2]).

-include_lib("kernel/include/file.hrl").

%% CSP (continuation style programming)
find_erl(Directory) -> find_erl(Directory, queue:new()).

find_erl(Name, Queue) ->
    {ok, F = #file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
      directory -> handle_directory(Name, Queue);
      regular -> handle_regular_file(Name, Queue);
      _ -> dequeue_and_run(Queue)
    end.

%% Open a directory and enqueue it's files.
handle_directory(Dir, Queue) ->
    case file:list_dir(Dir) of
      {ok, []} -> dequeue_and_run(Queue);
      {ok, Files} ->
	  dequeue_and_run(enqueue_many(Dir, Files, Queue))
    end.

%% Dequeue file from Queue and run it.
dequeue_and_run(Queue) ->
    case queue:out(Queue) of
      {empty, _} -> done;
      {{value, File}, NewQueue} -> find_erl(File, NewQueue)
    end.

%% Add list of files to Queue.
enqueue_many(Path, Files, Queue) ->
    F = fun (File, Q) ->
		queue:in(filename:join(Path, File), Q)
	end,
    lists:foldl(F, Queue, Files).

%% Check that a file has .erl extension.
handle_regular_file(Name, Queue) ->
    case filename:extension(Name) of
      ".erl" ->
	  {continue, Name, fun () -> dequeue_and_run(Queue) end};
      _ -> dequeue_and_run(Queue)
    end.

regex_count(Re, Str) ->
    case re:run(Str, Re, [global]) of
      nomatch -> 0;
      {match, List} -> length(List)
    end.

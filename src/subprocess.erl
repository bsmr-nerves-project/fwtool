-module(subprocess).

-export([run/1, run/2]).

% Run the specified executable and return ok on success
run(Executable) ->
    run(Executable, []).

% Run an executable with the arguments passed in as a list
run(Executable, Args) ->
    case os:find_executable(Executable) of
	false -> 
	    exit(enoent);
	FoundExecutable ->
	    Port = open_port({spawn_executable, FoundExecutable}, 
			     [exit_status, {args, Args}, stderr_to_stdout]),
	    loop_till_done(Port)
    end.

loop_till_done(Port) ->
    receive
	{Port, {data, _Data}} ->
	    % Throw out anything coming in from stdin
	    loop_till_done(Port);
	{Port, {exit_status, 0}} ->
	    ok;
	{Port, {exit_status, ExitStatus}} ->
	    {error, ExitStatus}
    end.

%% The MIT License (MIT)
%%
%% Copyright (c) 2013 Frank Hunleth
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

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

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

-module(fwprogram).

-export([program/3]).

-export([main/1]).

-include_lib("kernel/include/file.hrl").

main(Options) ->
    DestinationPath = proplists:get_value(destination, Options),
    UpdateType = proplists:get_value(update_type, Options),
    FirmwarePath = proplists:get_value(firmware, Options),
    program(FirmwarePath, UpdateType, DestinationPath).

program(FirmwarePath, UpdateType, DestinationPath) ->
    {ok, Writer} = open_destination(DestinationPath),
    {ok, ZipHandle} = zip:zip_open(FirmwarePath, [memory]),
    {ok, {_, InstructionsBin}} = zip:zip_get("instructions.json", ZipHandle),
    Instructions = jsx:decode(InstructionsBin, [{labels, atom}]),

    case proplists:get_value(UpdateType, Instructions) of
	undefined -> exit(unsupported_upgrade);
	Update -> run_upgrade(Update, ZipHandle, Writer)
    end,

    close_destination(Writer),
    ok = zip:zip_close(ZipHandle).

run_command([<<"pwrite">>, Path, Location, Size], ZipHandle, Writer) ->
    {ok, {_, Data}} = zip:zip_get(binary_to_list(Path), ZipHandle),
    % Verify the size
    Size = byte_size(Data),
    ok = pwrite(Writer, Location, Data).

run_upgrade([], _ZipHandle, _Writer) -> ok;
run_upgrade([Command|T], ZipHandle, Writer) ->
    run_command(Command, ZipHandle, Writer),
    run_upgrade(T, ZipHandle, Writer).

open_destination(DestinationPath) ->
    case file:read_file_info(DestinationPath) of
	{error, enoent} ->
	    % File doesn't exist, so create it.
	    {ok, Handle} = file:open(DestinationPath, [write]),
	    {ok, {file, Handle}};
	{error, Reason} ->
	    % Something's wrong
	    io:format("Error: Problem with ~p~n", [DestinationPath]),
	    {error, Reason};
	{ok, #file_info{access = read}} ->
	    % If the file is readonly, then it's also no good
	    io:format("Error: ~p is readonly~n", [DestinationPath]),
	    {error, readonly};
	{ok, #file_info{type = directory}} ->
	    % A directory doesn't work either
	    io:format("Error: ~p is directory. Expecting a file or device.~n", [DestinationPath]),
	    {error, eisdir};
	{ok, #file_info{type = regular}} ->
	    % If a regular file, then just use Erlang
	    {ok, Handle} = file:open(DestinationPath, [write]),
	    {ok, {file, Handle}};
	{ok, #file_info{type = device}} ->
	    % Device file. Erlang's file module won't allow writes
	    % to devices, so check for helpers
	    open_destination_helper(DestinationPath)
    end.

open_destination_helper(DestinationPath) ->
    case os:find_executable("mmccopy") of
	false ->
	    case os:find_executable("dd") of
		false ->
		    {error, nohelper};
		Dd ->
		    io:format("WARNING: Didn't find mmccopy so going to use dd~n"),
		    {ok, {dd, Dd, DestinationPath}}
	    end;
	Mmccopy ->
	    {ok, {mmccopy, Mmccopy, DestinationPath}}
    end.

pwrite({file, Handle}, Location, Data) ->
    file:pwrite(Handle, Location, Data);
pwrite({dd, Dd, DestinationPath}, Location, Data) ->
    LocationBlocks = Location div 512,
    Args = ["of=" ++ DestinationPath, "seek=" ++ integer_to_list(LocationBlocks)],
    subprocess:run(Dd, Args, Data);
pwrite({mmccopy, Mmccopy, DestinationPath}, Location, Data) ->
    DataSize = byte_size(Data),
    Args = ["-d", DestinationPath,
	    "-s", integer_to_list(DataSize),
	    "-o", integer_to_list(Location),
	    "-"],
    subprocess:run(Mmccopy, Args, Data).

close_destination({file, Handle}) ->
    file:close(Handle);
close_destination({mmccopy, _Mmccopy, _DestinationPath}) ->
    ok;
close_destination({dd, _Dd, _DestinationPath}) ->
    ok.

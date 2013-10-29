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

main(Options) ->
    DestinationPath = proplists:get_value(destination, Options),
    UpdateType = proplists:get_value(update_type, Options),
    FirmwarePath = proplists:get_value(firmware, Options),
    program(FirmwarePath, UpdateType, DestinationPath).

program(FirmwarePath, UpdateType, DestinationPath) ->
    {ok, Destination} = file:open(DestinationPath, [write]),
    {ok, ZipHandle} = zip:zip_open(FirmwarePath, [memory]),
    {ok, {_, InstructionsBin}} = zip:zip_get("instructions.json", ZipHandle),
    Instructions = jsx:decode(InstructionsBin, [{labels, atom}]),

    case proplists:get_value(UpdateType, Instructions) of
	undefined -> exit(unsupported_upgrade);
	Update -> run_upgrade(Update, ZipHandle, Destination)
    end,

    ok = file:close(Destination),
    ok = zip:zip_close(ZipHandle).

run_command([<<"pwrite">>, Path, Location, Size], ZipHandle, Destination) ->
    {ok, {_, Data}} = zip:zip_get(binary_to_list(Path), ZipHandle),
    % Verify the size
    Size = byte_size(Data),
    ok = file:pwrite(Destination, Location, Data).

run_upgrade([], _ZipHandle, _Destination) -> ok;
run_upgrade([Command|T], ZipHandle, Destination) ->
    run_command(Command, ZipHandle, Destination),
    run_upgrade(T, ZipHandle, Destination).

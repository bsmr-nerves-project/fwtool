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

-module(fwtool).

-export([main/1]).

main(CmdLine) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CmdLine) of
	{ok, {Options, _NonOptArgs}} ->
	    Command = proplists:get_value(command, Options),
	    Config = load_config(proplists:get_value(config, Options)),
	    CombinedOptions = merge_proplists(Options, Config),
	    help_or_run_command(Command, CombinedOptions);
	{error, {Reason, Data}} ->
	    io:format("Error: ~s ~p~n~n", [Reason, Data]),
	    usage()
    end.

merge_proplists(P1, P2) ->
    P3 = lists:append(P1, P2),
    [{K, proplists:get_value(K, P3)} || K <- proplists:get_keys(P3)].

load_config(Filename) ->
    case file:consult(Filename) of
	{ok, Config} ->
	    Config;
	{error, _} ->
	    []
    end.

help_or_run_command(Command, Options) ->
    case proplists:get_bool(help, Options) of
	true -> usage();
	false -> run_command(Command, Options)
    end.

run_command("create", Options) ->
    fwbuilder:main(Options);
run_command("run", Options) ->
    DestinationPath = proplists:get_value(destination, Options),
    UpdateType = proplists:get_value(update_type, Options),
    FirmwarePath = proplists:get_value(firmware, Options),
    fwprogrammer:start_link(),
    fwprogrammer:program(FirmwarePath, UpdateType, DestinationPath);
run_command(undefined, _Options) ->
    io:format("Error: specify a command~n"),
    usage();
run_command(Command, _Options) ->
    io:format("Error: unknown command ~p~n", [Command]),
    usage().

usage() ->
    getopt:usage(option_spec_list(), "fwtool"),
    io:format("Commands:~n"),
    io:format("  create   Create a firmware file~n"),
    io:format("  run      Run the firmware update~n"),
    io:format("~n"),
    io:format("Examples:~n"),
    io:format("  sudo env PATH=$PATH fwtool -d /dev/sdc -t complete run bbb.fw~n").

option_spec_list() ->
    [
     %% {Name,     ShortOpt,    LongOpt,    ArgSpec,    HelpMsg}
     {help,        $?,          "help",     undefined,  "Show program options"},
     {config,      $c,          "config",   {string, "fwtool.config"},     "Config file (create, run)"},
     {destination, $d,          "destination", string,  "Destination (run) (e.g. /dev/mmcblk0, /dev/sdc, firmware.img)"},
     {update_type, $t,          "type",     atom,       "Update type (run)"},
     {boot_partition_start, undefined, "boot_partition_start", string, "Boot partition start in blocks (create)"},
     {boot_partition_count, undefined, "boot_partition_count", string, "Boot partition size in blocks (create)"},
     {rootfs_a_partition_start, undefined, "rootfs_a_partition_start", string, "Rootfs A partition start in blocks (create)"},
     {rootfs_a_partition_count, undefined, "rootfs_a_partition_count", string, "Rootfs A partition size in blocks (create)"},

     {rootfs_b_partition_start, undefined, "rootfs_b_partition_start", string, "Rootfs B partition start in blocks (create)"},
     {rootfs_b_partition_count, undefined, "rootfs_b_partition_count", string, "Rootfs B partition size in blocks (create)"},

     {application_partition_start, undefined, "application_partition_start", string, "App partition start in blocks (create)"},
     {application_partition_count, undefined, "application_partition_count", string, "App partition size in blocks (create)"},

     {mlo_path, undefined, "mlo_path", string, "Path to MLO bootloader (create)"},
     {uboot_path, undefined, "uboot_path", string, "Path to U-Boot bootloader (create)"},
     {uenv_txt, undefined, "uenv_txt", string, "Contents of the uEnv.txt file (create)"},
     {rootfs_path, undefined, "rootfs_path", string, "Path to RootFS image (create)"},
     {command,     undefined,   undefined,  string,     "Command"},
     {firmware,    undefined,   undefined,  string,     "Firmware file name"}
    ].

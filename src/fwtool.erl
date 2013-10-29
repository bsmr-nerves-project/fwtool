-module(fwtool).

-export([main/1]).

main(CmdLine) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CmdLine) of
	{ok, {Options, _NonOptArgs}} ->
	    Command = proplists:get_value(command, Options),
	    Config = load_config(proplists:get_value(config, Options)),
	    CombinedOptions = merge_proplists(Options, Config),
	    run_command(Command, CombinedOptions);
	{error, {Reason, Data}} ->
	    io:format("Error: ~s ~p~n~n", [Reason, Data]),
	    usage()
    end.

merge_proplists(P1, P2) ->
    P3 = lists:append(P1, P2),
    [{K, proplists:get_value(K, P3)} || K <- proplists:get_keys(P3)].

load_config(undefined)->
    load_config("fwtool.config");
load_config(Filename) ->
    case file:consult(Filename) of
	{ok, Config} ->
	    Config;
	{error, _} ->
	    []
    end.

run_command("create", Options) ->
    fwbuilder:main(Options);
run_command("run", Options) ->
    fwprogram:main(Options);
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
    io:format("  run      Run the firmware update~n").
    
option_spec_list() ->
    [
     %% {Name,     ShortOpt,    LongOpt,    ArgSpec,    HelpMsg}
     {help,        $?,          "help",     undefined,  "Show program options"},
     {config,      $c,          "config",   string,     "Config file"},
     {destination, $d,          "destination", string,  "Destination (/dev/mmcblk0, /dev/sdc, firmware.img)"},
     {update_type, $t,          "type",     atom,       "Update type"},
     {command,     undefined,   undefined,  string,     "Command"},
     {firmware,    undefined,   undefined,  string,     "Firmware file name"}
    ].
    
    

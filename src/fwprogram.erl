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

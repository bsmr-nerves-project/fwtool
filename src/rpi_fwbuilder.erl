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

-module(rpi_fwbuilder).

-export([build/1]).

-spec build([{atom(), term()}]) -> ok.
build(Options) ->
    BootFs = boot_fs(Options),
    RootFsPath = get_path(rootfs_path, Options),
    {ok, RootFs} = file:read_file(RootFsPath),
    KernelPath = get_path(kernel_path, Options),
    {ok, Kernel} = file:read_file(KernelPath),
    build_fw_file(Options, BootFs, Kernel, RootFs).

-spec in_blocks(atom(), [{atom(), term()}]) -> non_neg_integer().
in_blocks(What, Options) ->
    proplists:get_value(What, Options).
-spec in_bytes(atom(), [{atom(), term()}]) -> non_neg_integer().
in_bytes(What, Options) ->
    512 * in_blocks(What, Options).

-spec build_fw_file([{atom(), term()}], binary(), binary(), binary()) -> ok.
build_fw_file(Options, BootFs, Kernel, RootFs) ->
    MbrA = mbr_a(Options),
    MbrB = mbr_b(Options),
    Instructions = [{bootloader,
		     [[<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"fat_write_file">>, <<"data/zImage">>, in_bytes(boot_partition_start, Options), <<"zImage">>]]},
		    {update_a,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)],
		      [<<"fat_rm_file">>, in_bytes(boot_partition_start,Options), <<"zImage.previous">>],
		      [<<"fat_write_file">>, <<"data/zImage">>, in_bytes(boot_partition_start, Options), <<"zImage.next">>],
		      [<<"fat_mv_file">>, in_bytes(boot_partition_start,Options), <<"zImage">>, <<"zImage.previous">>],
		      [<<"fat_mv_file">>, in_bytes(boot_partition_start,Options), <<"zImage.next">>, <<"zImage">>],
		      [<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)]]},
		    {update_b,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)],
		      [<<"fat_rm_file">>, in_bytes(boot_partition_start,Options), <<"zImage.previous">>],
		      [<<"fat_write_file">>, <<"data/zImage">>, in_bytes(boot_partition_start, Options), <<"zImage.next">>],
		      [<<"fat_mv_file">>, in_bytes(boot_partition_start,Options), <<"zImage">>, <<"zImage.previous">>],
		      [<<"fat_mv_file">>, in_bytes(boot_partition_start,Options), <<"zImage.next">>, <<"zImage">>],
		      [<<"pwrite">>, <<"data/mbr-b.img">>, 0, byte_size(MbrB)]]},
		    {autoupdate,
		     [[<<"compare_and_run">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA), <<"update_b">>],
		      [<<"compare_and_run">>, <<"data/mbr-b.img">>, 0, byte_size(MbrB), <<"update_a">>],
		      [<<"fail">>, <<"Unexpected config.txt contents so not updating">>]]},
		    {complete,
		     [[<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"fat_write_file">>, <<"data/zImage">>, in_bytes(boot_partition_start, Options), <<"zImage">>],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)]]}
		   ],
    InstructionsBin = jsx:encode(Instructions, [space, indent]),
    FileList = [{"instructions.json", InstructionsBin},
		{"data/mbr-a.img", MbrA},
		{"data/mbr-b.img", MbrB},
		{"data/boot.img", BootFs},
		{"data/zImage", Kernel},
	        {"data/rootfs.img", RootFs}],
    OutputFile = proplists:get_value(firmware, Options),
    {ok, _} = zip:create(OutputFile, FileList),
    ok.

% Build the MBR
-spec mbr_a([{atom(),term()}]) -> binary().
mbr_a(Options) ->
    mbr:create(<<0:3520>>, % Bootstrap code unused.
	       [{boot, fat32, in_blocks(boot_partition_start, Options),
		 in_blocks(boot_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_a_partition_start, Options),
		 in_blocks(rootfs_a_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
		 in_blocks(rootfs_b_partition_count, Options)},
		{normal, linux, in_blocks(application_partition_start, Options),
		 in_blocks(application_partition_count, Options)}]).

-spec mbr_b([{atom(),term()}]) -> binary().
mbr_b(Options) ->
    mbr:create(<<0:3520>>, % Bootstrap code unused.
	       [{boot, fat32, in_blocks(boot_partition_start, Options),
		 in_blocks(boot_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
		 in_blocks(rootfs_a_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_a_partition_start, Options),
		 in_blocks(rootfs_b_partition_count, Options)},
		{normal, linux, in_blocks(application_partition_start, Options),
		 in_blocks(application_partition_count, Options)}]).

-spec tmpdir() -> string().
tmpdir() ->
    case os:getenv("TMPDIR") of
	false -> "/tmp";
	Dir -> Dir
    end.

abspath(PossibleRelativePath, Options) ->
    case hd(PossibleRelativePath) of
	$/ ->
	    PossibleRelativePath;
	_ ->
	    BasePath = proplists:get_value(base_path, Options),
	    BasePath ++ "/" ++ PossibleRelativePath
    end.

get_path(Key, Options) ->
    abspath(proplists:get_value(Key, Options), Options).

check_existance(Path) ->
    case file:open(Path, [read]) of
	{ok, IoDevice} ->
	    file:close(IoDevice);
	{error, Reason} ->
	    exit({open_error, Path, Reason})
    end.

-spec copy_to_fat(string(), string(), string()) -> ok.
copy_to_fat(BootFilename, SourceFile, DestFile) ->
    check_existance(SourceFile),
    ok = subprocess:vrun("mcopy", ["-i", BootFilename, SourceFile, "::" ++ DestFile]),
    ok.

% Build the boot file system
-spec boot_fs([{atom(),term()}]) -> binary().
boot_fs(Options) ->
    BootFilename = tmpdir() ++ "/boot.vfat",
    {ok,_} = subprocess:run("dd", ["if=/dev/zero", "of=" ++ BootFilename, "count=0",
				   "seek=" ++ integer_to_list(in_blocks(boot_partition_count, Options))]),
    {ok,_} = subprocess:run("mkfs.vfat", ["-F", "32", "-n", "BOOT", BootFilename]),

    OtherFiles = proplists:get_value(boot_partition_extra_files, Options),
    [ copy_to_fat(BootFilename, abspath(Src, Options), Dst) || {Src, Dst} <- OtherFiles ],

    {ok, Contents} = file:read_file(BootFilename),
    ok = file:delete(BootFilename),
    Contents.

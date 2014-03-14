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

-module(uboot_fwbuilder).

-export([build/1]).

-spec build([{atom(), term()}]) -> ok.
build(Options) ->
    BootFs = boot_fs(Options),
    MbrA = mbr_a(Options),
    MbrB = mbr_b(Options),
    RootFsPath = get_path(rootfs_path, Options),
    {ok, RootFs} = file:read_file(RootFsPath),
    build_fw_file(Options, BootFs, MbrA, MbrB, RootFs).

-spec in_blocks(atom(), [{atom(), term()}]) -> non_neg_integer().
in_blocks(What, Options) ->
    proplists:get_value(What, Options).
-spec in_bytes(atom(), [{atom(), term()}]) -> non_neg_integer().
in_bytes(What, Options) ->
    512 * in_blocks(What, Options).

-spec build_fw_file([{atom(), term()}], binary(), binary(), binary(), binary()) -> ok.
build_fw_file(Options, BootFs, MbrA, MbrB, RootFs) ->
    Instructions = [{bootloader,
		     [[<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)]]},
		    {update_a,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)],
		      [<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)]]},
		    {update_b,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)],
		      [<<"pwrite">>, <<"data/mbr-b.img">>, 0, byte_size(MbrB)]]},
		    {autoupdate,
		     [[<<"compare_and_run">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA), <<"update_b">>],
		      [<<"compare_and_run">>, <<"data/mbr-b.img">>, 0, byte_size(MbrB), <<"update_a">>],
		      [<<"fail">>, <<"Unexpected MBR contents so not updating">>]]},
		    {complete,
		     [[<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)]]}
		   ],
    InstructionsBin = jsx:encode(Instructions, [space, indent]),
    FileList = [{"instructions.json", InstructionsBin},
		{"data/boot.img", BootFs},
		{"data/mbr-a.img", MbrA},
		{"data/mbr-b.img", MbrB},
	        {"data/rootfs.img", RootFs}],
    OutputFile = proplists:get_value(firmware, Options),
    {ok, _} = zip:create(OutputFile, FileList),
    ok.

% Build the MBR for booting off partition A
-spec mbr_a([{atom(),term()}]) -> binary().
mbr_a(Options) ->
    mbr:create(<<0:3520>>, % Bootstrap code unused.
	       [{boot, fat12, in_blocks(boot_partition_start, Options),
		 in_blocks(boot_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_a_partition_start, Options),
		 in_blocks(rootfs_a_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
		 in_blocks(rootfs_b_partition_count, Options)},
		{normal, linux, in_blocks(application_partition_start, Options),
		 in_blocks(application_partition_count, Options)}]).

% Build the MBR for booting off partition B
-spec mbr_b([{atom(),term()}]) -> binary().
mbr_b(Options) ->
    mbr:create(<<0:3520>>, % Bootstrap code unused
	       [{boot, fat12, in_blocks(boot_partition_start, Options),
		 in_blocks(boot_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
		 in_blocks(rootfs_b_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_a_partition_start, Options),
		 in_blocks(rootfs_a_partition_count, Options)},
		{normal, linux, in_blocks(application_partition_start, Options),
		 in_blocks(application_partition_count, Options)}]).

-spec tmpdir() -> string().
tmpdir() ->
    case os:getenv("TMPDIR") of
	false -> "/tmp";
	Dir -> Dir
    end.

get_path(Key, Options) ->
    PossibleRelativePath = proplists:get_value(Key, Options),
    case hd(PossibleRelativePath) of
	$/ ->
	    PossibleRelativePath;
	_ ->
	    BasePath = proplists:get_value(base_path, Options),
	    BasePath ++ "/" ++ PossibleRelativePath
    end.

% Build the boot file system
-spec boot_fs([{atom(),term()}]) -> binary().
boot_fs(Options) ->
    BootFilename = tmpdir() ++ "/boot.vfat",
    {ok,_} = subprocess:run("dd", ["if=/dev/zero", "of=" ++ BootFilename, "count=0",
				   "seek=" ++ integer_to_list(in_blocks(boot_partition_count, Options))]),
    {ok,_} = subprocess:run("mkfs.vfat", ["-F", "12", "-n", "boot", BootFilename]),
    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, get_path(mlo_path, Options), "::MLO"]),
    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, get_path(uboot_path, Options), "::U-BOOT.IMG"]),
    case proplists:get_value(uenv_txt, Options) of
	UEnvTxtContents when is_list(UEnvTxtContents) ->
	    TmpFilename = tmpdir() ++ "/uEnv.txt",
	    ok = file:write_file(TmpFilename, list_to_binary(UEnvTxtContents)),
	    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, TmpFilename, "::UENV.TXT"]),
	    ok = file:delete(TmpFilename);
	undefined ->
	    ok
    end,
    {ok, Contents} = file:read_file(BootFilename),
    ok = file:delete(BootFilename),
    Contents.

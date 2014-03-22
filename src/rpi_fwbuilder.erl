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
    Mbr = mbr(Options),
    RootFsPath = get_path(rootfs_path, Options),
    {ok, RootFs} = file:read_file(RootFsPath),
    KernelPath = get_path(kernel_path, Options),
    {ok, Kernel} = file:read_file(KernelPath),
    build_fw_file(Options, BootFs, Mbr, Kernel, RootFs).

-spec in_blocks(atom(), [{atom(), term()}]) -> non_neg_integer().
in_blocks(What, Options) ->
    proplists:get_value(What, Options).
-spec in_bytes(atom(), [{atom(), term()}]) -> non_neg_integer().
in_bytes(What, Options) ->
    512 * in_blocks(What, Options).

% Build the config.txt file contents that are used
% to determine which Linux kernel to boot.
-spec build_config_file(string(), integer()) -> binary().
build_config_file(Kernel, Partition) ->
    ConfigTxt =
	"# DO NOT EDIT. USED BY NERVES FOR FIRMWARE UPDATE\n"
	"cmdline=\"dwc_otg.fiq_fix_enable=1 sdhci-bcm2708.sync_after_dma=0 dwc_otg.lpm_enable=0 console=tty1 root=/dev/mmcblk0p" ++ integer_to_list(Partition) ++ " rootwait -u\"\n"
	"arm_freq=700\n"
	"core_freq=250\n"
	"kernel=" ++ Kernel ++ "\n"
	"disable_overscan=1\n"
	"gpu_mem_256=100\n"
	"gpu_mem_512=100\n"
	"sdram_freq=400\n"
	"over_voltage=0\n",
    list_to_binary(ConfigTxt).

-spec build_fw_file([{atom(), term()}], binary(), binary(), binary(), binary()) -> ok.
build_fw_file(Options, BootFs, Mbr, Kernel, RootFs) ->
    Instructions = [{bootloader,
		     [[<<"pwrite">>, <<"data/mbr.img">>, 0, byte_size(Mbr)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)]]},
		    {update_a,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)],
		      [<<"fat_write_file">>, <<"data/zImage">>, in_bytes(boot_partition_start, Options), <<"zImage.a">>],
		      [<<"fat_write_file">>, <<"data/config.a.txt">>, in_bytes(boot_partition_start, Options), <<"config.txt">>]]},
		    {update_b,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)],
		      [<<"fat_write_file">>, <<"data/zImage">>, in_bytes(boot_partition_start, Options), <<"zImage.b">>],
		      [<<"fat_write_file">>, <<"data/config.b.txt">>, in_bytes(boot_partition_start, Options), <<"config.txt">>]]},
		    {autoupdate,
		     [[<<"fat_compare_and_run">>, <<"data/config.a.txt">>, in_bytes(boot_partition_start, Options), <<"config.txt">>, <<"update_b">>],
		      [<<"fat_compare_and_run">>, <<"data/config.b.txt">>, in_bytes(boot_partition_start, Options), <<"config.txt">>, <<"update_a">>],
		      [<<"fail">>, <<"Unexpected config.txt contents so not updating">>]]},
		    {complete,
		     [[<<"pwrite">>, <<"data/mbr.img">>, 0, byte_size(Mbr)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)]]}
		   ],
    InstructionsBin = jsx:encode(Instructions, [space, indent]),
    ConfigA = build_config_file("zImage.a", 2),
    ConfigB = build_config_file("zImage.b", 3),
    FileList = [{"instructions.json", InstructionsBin},
		{"data/mbr.img", Mbr},
		{"data/boot.img", BootFs},
		{"data/config.a.txt", ConfigA},
		{"data/config.b.txt", ConfigB},
		{"data/zImage", Kernel},
	        {"data/rootfs.img", RootFs}],
    OutputFile = proplists:get_value(firmware, Options),
    {ok, _} = zip:create(OutputFile, FileList),
    ok.

% Build the MBR
-spec mbr([{atom(),term()}]) -> binary().
mbr(Options) ->
    mbr:create(<<0:3520>>, % Bootstrap code unused.
	       [{boot, fat32, in_blocks(boot_partition_start, Options),
		 in_blocks(boot_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_a_partition_start, Options),
		 in_blocks(rootfs_a_partition_count, Options)},
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
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

    KernelPath = get_path(kernel_path, Options),
    copy_to_fat(BootFilename, KernelPath, "zImage.a"),

    DefaultCfgFilename = tmpdir() ++ "/fwtool-config.txt",
    ok = file:write_file(DefaultCfgFilename, build_config_file("zImage.a", 2)),
    copy_to_fat(BootFilename, DefaultCfgFilename, "config.txt"),

    {ok, Contents} = file:read_file(BootFilename),
    ok = file:delete(BootFilename),
    ok = file:delete(DefaultCfgFilename),
    Contents.

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

-module(syslinux_fwbuilder).

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

% Build the default.cfg file contents that are used by the syslinux.cfg
% to determine which Linux kernel to boot.
-spec build_default_file(string()) -> binary().
build_default_file(Which) ->
    list_to_binary("DEFAULT Linux-" ++ Which ++ "\n").

-spec build_fw_file([{atom(), term()}], binary(), binary(), binary(), binary()) -> ok.
build_fw_file(Options, BootFs, Mbr, Kernel, RootFs) ->
    Instructions = [{bootloader,
		     [[<<"pwrite">>, <<"data/mbr.img">>, 0, byte_size(Mbr)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)]]},
		    {update_a,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)],
		      [<<"fat_write_file">>, <<"data/bzImage">>, in_bytes(boot_partition_start, Options), <<"bzImage.a">>],
		      [<<"fat_write_file">>, <<"data/default.a.cfg">>, in_bytes(boot_partition_start, Options), <<"default.cfg">>]]},
		    {update_b,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)],
		      [<<"fat_write_file">>, <<"data/bzImage">>, in_bytes(boot_partition_start, Options), <<"bzImage.b">>],
		      [<<"fat_write_file">>, <<"data/default.b.cfg">>, in_bytes(boot_partition_start, Options), <<"default.cfg">>]]},
		    {autoupdate,
		     [[<<"fat_compare_and_run">>, <<"data/default.a.cfg">>, in_bytes(boot_partition_start, Options), <<"default.cfg">>, <<"update_b">>],
		      [<<"fat_compare_and_run">>, <<"data/default.b.cfg">>, in_bytes(boot_partition_start, Options), <<"default.cfg">>, <<"update_a">>],
		      [<<"fail">>, <<"Unexpected default.cfg contents so not updating">>]]},
		    {complete,
		     [[<<"pwrite">>, <<"data/mbr.img">>, 0, byte_size(Mbr)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)]]}
		   ],
    InstructionsBin = jsx:encode(Instructions, [space, indent]),
    FileList = [{"instructions.json", InstructionsBin},
		{"data/boot.img", BootFs},
		{"data/mbr.img", Mbr},
		{"data/default.a.cfg", build_default_file("A")},
		{"data/default.b.cfg", build_default_file("B")},
		{"data/bzImage", Kernel},
	        {"data/rootfs.img", RootFs}],
    OutputFile = proplists:get_value(firmware, Options),
    {ok, _} = zip:create(OutputFile, FileList),
    ok.

% Build the MBR
-spec mbr([{atom(),term()}]) -> binary().
mbr(Options) ->
    BootstrapCodePath = get_path(mbr_bootstrap_path, Options),
    {ok, BootstrapCode} = file:read_file(BootstrapCodePath),
    mbr:create(BootstrapCode,
	       [{boot, fat12, in_blocks(boot_partition_start, Options),
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

get_path(Key, Options) ->
    PossibleRelativePath = proplists:get_value(Key, Options),
    case hd(PossibleRelativePath) of
	$/ ->
	    PossibleRelativePath;
	_ ->
	    BasePath = proplists:get_value(base_path, Options),
	    BasePath ++ "/" ++ PossibleRelativePath
    end.

check_existance(Path) ->
    case file:open(Path, [read]) of
	{ok, IoDevice} ->
	    file:close(IoDevice);
	{error, Reason} ->
	    exit({open_error, Path, Reason})
    end.

% Build the boot file system
-spec boot_fs([{atom(),term()}]) -> binary().
boot_fs(Options) ->
    BootFilename = tmpdir() ++ "/boot.vfat",
    {ok,_} = subprocess:run("dd", ["if=/dev/zero", "of=" ++ BootFilename, "count=0",
				   "seek=" ++ integer_to_list(in_blocks(boot_partition_count, Options))]),
    {ok,_} = subprocess:run("mkfs.vfat", ["-F", "12", "-n", "boot", BootFilename]),

    SyslinuxCfg = get_path(syslinuxcfg_path, Options),
    check_existance(SyslinuxCfg),

    DefaultCfgFilename = tmpdir() ++ "/default.cfg",
    ok = file:write_file(DefaultCfgFilename, build_default_file("A")),

    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, SyslinuxCfg, "::syslinux.cfg"]),
    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, DefaultCfgFilename, "::default.cfg"]),

    KernelPath = get_path(kernel_path, Options),
    check_existance(KernelPath),
    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, KernelPath, "::bzImage.a"]),
    {ok,_} = subprocess:run("syslinux", ["-i", BootFilename]),
    {ok, Contents} = file:read_file(BootFilename),
    ok = file:delete(BootFilename),
    Contents.

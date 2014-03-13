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
    RootFsPath = proplists:get_value(rootfs_path, Options),
    {ok, RootFs} = file:read_file(RootFsPath),
    KernelPath = proplists:get_value(kernel_path, Options),
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
		      [<<"copy_file">>, <<"data/bzImage">>, <<"/mnt/bzImage-a">>],
		      [<<"copy_file">>, <<"data/default-a.cfg">>, <<"/mnt/default.cfg">>]]},
		    {update_b,
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)],
		      [<<"copy_file">>, <<"data/bzImage">>, <<"/mnt/bzImage-b">>],
		      [<<"copy_file">>, <<"data/default-b.cfg">>, <<"/mnt/default.cfg">>]]},
		    {autoupdate,
		     %% FIXME
		     [[<<"compare_and_run">>, <<"data/mbr.img">>, 0, byte_size(Mbr), <<"update_b">>],
		      [<<"compare_and_run">>, <<"data/mbr.img">>, 0, byte_size(Mbr), <<"update_a">>],
		      [<<"fail">>, <<"Unexpected MBR contents so not updating">>]]},
		    {complete,
		     [[<<"pwrite">>, <<"data/mbr.img">>, 0, byte_size(Mbr)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)]]}
		   ],
    InstructionsBin = jsx:encode(Instructions, [space, indent]),
    FileList = [{"instructions.json", InstructionsBin},
		{"data/boot.img", BootFs},
		{"data/mbr.img", Mbr},
		{"data/default-a.cfg", build_default_file("A")},
		{"data/default-b.cfg", build_default_file("B")},
		{"data/bzImage", Kernel},
	        {"data/rootfs.img", RootFs}],
    OutputFile = proplists:get_value(firmware, Options),
    {ok, _} = zip:create(OutputFile, FileList),
    ok.

% Build the MBR
-spec mbr([{atom(),term()}]) -> binary().
mbr(Options) ->
    BootstrapCodePath = proplists:get_value(mbr_bootstrap_path, Options),
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

% Build the boot file system
-spec boot_fs([{atom(),term()}]) -> binary().
boot_fs(Options) ->
    BootFilename = tmpdir() ++ "/boot.vfat",
    {ok,_} = subprocess:run("dd", ["if=/dev/zero", "of=" ++ BootFilename, "count=0",
				   "seek=" ++ integer_to_list(in_blocks(boot_partition_count, Options))]),
    {ok,_} = subprocess:run("mkfs.vfat", ["-F", "12", "-n", "boot", BootFilename]),

    SyslinuxCfg = proplists:get_value(syslinuxcfg_path, Options),
    DefaultCfgFilename = tmpdir() ++ "/default.cfg",
    ok = file:write_file(DefaultCfgFilename, build_default_file("A")),

    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, SyslinuxCfg, "::syslinux.cfg"]),
    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, DefaultCfgFilename, "::default.cfg"]),
    {ok,_} = subprocess:run("mcopy", ["-i", BootFilename, proplists:get_value(kernel_path, Options), "::bzImage-a"]),
    {ok,_} = subprocess:run("syslinux", ["-i", BootFilename]),
    {ok, Contents} = file:read_file(BootFilename),
    ok = file:delete(BootFilename),
    Contents.

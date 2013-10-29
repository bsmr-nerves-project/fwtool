-module(fwbuilder).

-export([main/1]).

main(Options) ->
    BootFs = boot_fs(Options),
    MbrA = mbr_a(Options),
    MbrB = mbr_b(Options),
    RootFsPath = proplists:get_value(rootfs_path, Options),
    {ok, RootFs} = file:read_file(RootFsPath),
    build_fw_file(Options, BootFs, MbrA, MbrB, RootFs).

in_blocks(What, Options) ->
    proplists:get_value(What, Options).
in_bytes(What, Options) ->
    512 * in_blocks(What, Options).

build_fw_file(Options, BootFs, MbrA, MbrB, RootFs) ->
    Instructions = [{bootloader, 
		     [[<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)]]},
		    {update_a, 
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)],
		      [<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)]]},
		    {update_b, 
		     [[<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)],
		      [<<"pwrite">>, <<"data/mbr-b.img">>, 0, byte_size(MbrB)]]},
		    {complete, 
		     [[<<"pwrite">>, <<"data/mbr-a.img">>, 0, byte_size(MbrA)],
		      [<<"pwrite">>, <<"data/boot.img">>, in_bytes(boot_partition_start, Options), byte_size(BootFs)],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_a_partition_start, Options), byte_size(RootFs)],
		      [<<"pwrite">>, <<"data/rootfs.img">>, in_bytes(rootfs_b_partition_start, Options), byte_size(RootFs)]]}
		   ],
    InstructionsBin = jsx:encode(Instructions, [space, indent]),
    FileList = [{"instructions.json", InstructionsBin},
		{"data/boot.img", BootFs},
		{"data/mbr-a.img", MbrA},
		{"data/mbr-b.img", MbrB},
	        {"data/rootfs.img", RootFs}],
    OutputFile = proplists:get_value(firmware, Options),
    {ok, _} = zip:create(OutputFile, FileList).   

% Build the MBR for booting off partition A
mbr_a(Options) ->
    mbr:create([{boot, fat12, in_blocks(boot_partition_start, Options), 
		 in_blocks(boot_partition_count, Options)}, 
		{normal, linux, in_blocks(rootfs_a_partition_start, Options), 
		 in_blocks(rootfs_a_partition_count, Options)}, 
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
		 in_blocks(rootfs_b_partition_count, Options)}, 
		{normal, linux, in_blocks(application_partition_start, Options),
		 in_blocks(application_partition_count, Options)}]).

% Build the MBR for booting off partition B
mbr_b(Options) ->
    mbr:create([{boot, fat12, in_blocks(boot_partition_start, Options), 
		 in_blocks(boot_partition_count, Options)}, 
		{normal, linux, in_blocks(rootfs_b_partition_start, Options),
		 in_blocks(rootfs_b_partition_count, Options)}, 
		{normal, linux, in_blocks(rootfs_a_partition_start, Options), 
		 in_blocks(rootfs_a_partition_count, Options)}, 
		{normal, linux, in_blocks(application_partition_start, Options),
		 in_blocks(application_partition_count, Options)}]).

% Build the boot file system
boot_fs(Options) ->
    TmpFilename = "/tmp/boot.vfat",
    ok = subprocess:run("dd", ["if=/dev/zero", "of=" ++ TmpFilename, "count=0", 
			  "seek=" ++ integer_to_list(in_blocks(boot_partition_count, Options))]),
    ok = subprocess:run("mkfs.vfat", ["-F", "12", "-n", "boot", TmpFilename]),
    ok = subprocess:run("mcopy", ["-i", TmpFilename, in_blocks(mlo_path, Options), "::MLO"]),
    ok = subprocess:run("mcopy", ["-i", TmpFilename, in_blocks(uboot_path, Options), "::U-BOOT.IMG"]),
    {ok, Contents} = file:read_file(TmpFilename),
    ok = file:delete(TmpFilename),
    Contents.
    
    

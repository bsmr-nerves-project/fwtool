-module(fwbuilder).

-export([main/0]).

main() ->
    {ok, Options} = file:consult("firmware.config"),
    BootFs = boot_fs(Options),
    MbrA = mbr_a(Options),
    MbrB = mbr_b(Options),
    RootFsPath = proplists:get_value(rootfs_path, Options),
    {ok, RootFs} = file:read_file(RootFsPath),
    Manifest = [{"data/boot.img", proplists:get_value(boot_partition_start, Options)},
		{"data/mbr-a.img", 0},
		{"data/mbr-b.img", 0},
		{"data/rootfs.img", proplists:get_value(rootfs_a_partition_start, Options)},
		{"data/rootfs.img", proplists:get_value(rootfs_b_partition_start, Options)}],
    ManifestBin = list_to_binary(lists:flatten(io_lib:print(Manifest))),

    FileList = [{"manifest", ManifestBin},
		{"data/boot.img", BootFs},
		{"data/mbr-a.img", MbrA},
		{"data/mbr-b.img", MbrB},
	        {"data/rootfs.img", RootFs}],
    {ok, _} = zip:create("test.fw", FileList).

% Build the MBR for booting off partition A
mbr_a(Options) ->
    mbr:create([{boot, fat12, proplists:get_value(boot_partition_start, Options), 
		 proplists:get_value(boot_partition_count, Options)}, 
		{normal, linux, proplists:get_value(rootfs_a_partition_start, Options), 
		 proplists:get_value(rootfs_a_partition_count, Options)}, 
		{normal, linux, proplists:get_value(rootfs_b_partition_start, Options),
		 proplists:get_value(rootfs_b_partition_count, Options)}, 
		{normal, linux, proplists:get_value(application_partition_start, Options),
		 proplists:get_value(application_partition_count, Options)}]).

% Build the MBR for booting off partition B
mbr_b(Options) ->
    mbr:create([{boot, fat12, proplists:get_value(boot_partition_start, Options), 
		 proplists:get_value(boot_partition_count, Options)}, 
		{normal, linux, proplists:get_value(rootfs_b_partition_start, Options),
		 proplists:get_value(rootfs_b_partition_count, Options)}, 
		{normal, linux, proplists:get_value(rootfs_a_partition_start, Options), 
		 proplists:get_value(rootfs_a_partition_count, Options)}, 
		{normal, linux, proplists:get_value(application_partition_start, Options),
		 proplists:get_value(application_partition_count, Options)}]).

% Build the boot file system
boot_fs(Options) ->
    TmpFilename = "/tmp/boot.vfat",
    ok = subprocess:run("dd", ["if=/dev/zero", "of=" ++ TmpFilename, "count=0", 
			  "seek=" ++ integer_to_list(proplists:get_value(boot_partition_count, Options))]),
    ok = subprocess:run("mkfs.vfat", ["-F", "12", "-n", "boot", TmpFilename]),
    ok = subprocess:run("mcopy", ["-i", TmpFilename, proplists:get_value(mlo_path, Options), "::MLO"]),
    ok = subprocess:run("mcopy", ["-i", TmpFilename, proplists:get_value(uboot_path, Options), "::U-BOOT.IMG"]),
    {ok, Contents} = file:read_file(TmpFilename),
    ok = file:delete(TmpFilename),
    Contents.
    
    

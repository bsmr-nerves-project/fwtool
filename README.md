# fwtool

This is an Erlang version that's has similar features to some firmware
image builders I've written or used in other environments. It
currently creates images for the Beaglebone.

The fwtool application can be invoked both at the command line and
from within Erlang. Its purpose is to create firmware files (.fw) and
apply them to SDCards, eMMC, or raw image files suitable for bulk
programming.  Firmware files contain instructions so that only parts
of the non-volatile storage that need to be updated are programmed. It
can also be used to perform in-place upgrades, selectively program
parts, and enable other features such as progress reporting, digitally
signed updates, etc.  Firmware files are compressed zip files
internally so they can be exchanged over the Internet easily and have
some protection against corruption.

## Firmware image format

As previously mentioned, firmware files are just zip archives. They must
contain at least one file called `instructions.json`. In the common
case, the `instructions.json` file references other files in the
archive.

The `instructions.json` file contains one JSON object. The keys are
update types and the value is a list of commands. For example:

    {
     "bootloader": [
                    ["pwrite", "data/boot.img", 32256, 1032192]
                   ],
     "update_a": [
                  ["pwrite", "data/rootfs.img", 2097152, 19586048],
                  ["pwrite", "data/mbr-a.img", 0, 512]
                 ],
     "update_b": [
                  ["pwrite", "data/rootfs.img", 136314880, 19586048],
                  ["pwrite", "data/mbr-b.img", 0, 512]
                 ],
     "complete": [
                  ["pwrite", "data/mbr-a.img", 0, 512]
                  ["pwrite", "data/boot.img", 32256, 1032192]
                  ["pwrite", "data/rootfs.img", 2097152, 19586048],
                  ["pwrite", "data/rootfs.img", 136314880, 19586048]
    }

This firmware update contains 4 update types: "bootloader",
"update_a", "update_b", and "complete". The format does not specify
naming or semantics, but for the sake of clarity, the above example
expects a device with a Flash part containing a bootloader and two
firmware locations. The firmware flasher alternates between firmware
locations (e.g. A and B) and then programs the device's Master Boot
Record (MBR) so that the device boots to the most recently programmed
location. Note that the MBR is programmed last for both the "update_a"
and "update_b" types. Since the programming of the MBR switches which
firmware location is active, performing that the end makes the update
process more robust against a power failure while programming the
rootfs.

The command list contains zero or more commands. The following
commands are defined:

### pwrite

The `pwrite` command takes file in the firmware archive and writes it
to the specified offset. It has the following form:

    ["pwrite", Filename, DestinationOffset, FileSize]

The DestinationOffset should be specified in bytes. FileSize can be
used by the programmer to provide update progress information.

## Invoking from the command line

The fwtool program takes options from both the command line and a
config file (`fwtool.config` if unspecified). Most options are
available both places for convenience. Usage is:

    fwtool [options] <command> <firmware filename>

Commands are `create` and `run` for creating firmware images and
running them respectively. Options are summarized below:

--config (-c)
: A config file [default: fwtool.config]

--destination (-d)
: The destination device or file that will receive the update (run)

--type (-t)
: The update type. This must match an available update type in the
firmware archive. (run)

--boot_partition_start
: Boot partition start in blocks (create)

--boot_partition_count
: Boot partition size in blocks (create)

--rootfs_a_partition_start
: Rootfs A partition start in blocks (create)

--rootfs_a_partition_count
: Rootfs A partition size in blocks (create)

--rootfs_b_partition_start
: Rootfs B partition start in blocks (create)

--rootfs_b_partition_count
: Rootfs B partition size in blocks (create)

--application_partition_start
: App partition start in blocks (create)

--application_partition_count
: App partition size in blocks (create)

--mlo_path
: Path to MLO bootloader (create)

--uboot_path
: Path to U-Boot bootloader (create)

--rootfs_path
: Path to RootFS image (create)

### Examples

Create a firmware image (assumes most options are in fwtool.config):

    ./fwtool create myfirmware.fw

Create a raw image for a bulk memory programmer from the myfirmware.fw
file:

    ./fwtool -d myimage.img -t complete run myfirmware.fw

## Invoking from Erlang

## Todo

In no particular order:

 1. Add digital signature support
 2. Add ability to report progress
 3. Add commands for checking target compatibility (e.g. compatible MBR offsets)
 4. Add check that destination is unmounted to avoid accidents
 5. Add firmware metadata to the fw archive and helper functions to read it

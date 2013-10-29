# fwbuilder 

This is an Erlang port of firmware image builder and
programmers I've written or used on other embedded systems. It
currently creates images for the Beaglebone.

The fwbuilder application can create firmware files (.fw) and raw
images (.img). Raw images are intended for bulk programming of Flash
parts, since it is usually easiest (or only possible) to write one
large image to a part. The firmware files include binary data and
offset information so they can minimize the amount of data written to
perform an update and therefore can be run more quickly. Firmware
files are compressed zip files internally and can be exchanged over
the Internet more easily than raw images.

## Firmware image format

Firmware files are zip archives that must contain at least an
`instructions.json` file. In the common case that the
`instructions.json` file references other files in the archive, those
files must be present as well.

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
locations and then programs the device's Master Boot Record so that
the device boots to the most recently programmed location.

The command list contains zero or more commands. The following
commands are defined:

### pwrite

The `pwrite` command takes file in the firmware archive and writes it
to the specified offset. It has the following form:

    ["pwrite", Filename, DestinationOffset, FileSize]

The DestinationOffset should be specified in bytes. FileSize can be
used by the programmer to provide update progress information.

## Invoking


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
-module(mbr).

%% Interface
-export([create/2]).

-include_lib("eunit/include/eunit.hrl").

%% Hardcode the cylinder/head/sector geometry, since it's not relevant
%% to the types of memory that we use.
-define(SECTORS_PER_HEAD, 63).
-define(HEADS_PER_CYLINDER, 255).

%% MBR partition type to id
partition_type_to_id(linux) -> 16#83;
partition_type_to_id(fat32) -> 16#0c;
partition_type_to_id(fat16) -> 16#04;
partition_type_to_id(fat12) -> 16#01.

%% Bootable partition flag values
boottype_to_flag(boot) -> 16#80;
boottype_to_flag(normal) -> 0.

%% If you want a bootstrap section, just pass in a 440 byte binary.
%% If not, pass in <<0:3520>>.
%% Pass in a list of partitions like this:
%% [{boot, linux, LbaStartPart1, LbaCountPart1},
%%  {normal, linux, LbaStartPart2, LbaCountPart2},
%%  {normal, linux, LbaStartPart3, LbaCountPart3},
%%  {normal, linux, LbaStartPart4, LbaCountPart4}]
%%
-spec create(binary(), [{atom(), atom(), integer(), integer()}]) -> binary().
create(BootstrapCode, [Partition1, Partition2, Partition3, Partition4]) ->
    DiskId = 0, % Should be unique, but unused for firmware images
    Signature = 16#aa55,

    Partition1Bits = partition(Partition1),
    Partition2Bits = partition(Partition2),
    Partition3Bits = partition(Partition3),
    Partition4Bits = partition(Partition4),

    440 = byte_size(BootstrapCode),
    <<BootstrapCode/binary, % Bootstrap code (440 bytes)
      DiskId:32/little,
      0:16, % Padding
      Partition1Bits/binary,
      Partition2Bits/binary,
      Partition3Bits/binary,
      Partition4Bits/binary,
      Signature:16/little
      >>.

%% Convert the logical block address to cylinder/head/sector
%% form
lba_to_chs(Lba) ->
    Cylinder = Lba div (?SECTORS_PER_HEAD * ?HEADS_PER_CYLINDER),
    Head = (Lba div ?SECTORS_PER_HEAD) rem ?HEADS_PER_CYLINDER,
    Sector = (Lba rem ?SECTORS_PER_HEAD) + 1,
    <<0:6, CylinderHigh:2, CylinderLow:8>> = <<Cylinder:16>>,
    <<Head:8,
      CylinderHigh:2, Sector:6,
      CylinderLow:8>>.

partition({BootType, PartitionType, LbaFirst, SectorCount}) ->
    BootFlag = boottype_to_flag(BootType),
    ChsFirst = lba_to_chs(LbaFirst),
    ChsLast = lba_to_chs(LbaFirst + SectorCount - 1),
    PartitionId = partition_type_to_id(PartitionType),
    <<BootFlag:8,
      ChsFirst/binary,
      PartitionId:8,
      ChsLast/binary,
      LbaFirst:32/little,
      SectorCount:32/little>>.

create_test() ->
    Expected = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,1,1,0,1,32,63,0,
		 63,0,0,0,224,7,0,0,0,65,2,0,131,209,33,16,0,16,0,0,160,15,4,0,0,146,3,16,131,
		 35,34,33,0,16,4,0,160,15,4,0,0,227,4,32,131,12,35,41,0,16,8,0,0,0,2,0,85,170>>,
        ?assertEqual(Expected, mbr:create(<<0:3520>>, [{boot, fat12, 63, 2016}, {normal, linux, 4096, 266144}, {normal, linux, 266240, 266144}, {normal, linux, 528384, 131072}])).

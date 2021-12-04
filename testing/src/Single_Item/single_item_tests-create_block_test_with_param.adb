with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

with BRBON;
with BRBON.Block.Static_Unprotected;
with BRBON.Block;
with BRBON.Configure;
with BRBON.Types; use BRBON.Types;

with Serializable;

separate (Single_Item_Tests)

function Create_Block_Test_With_Param (Count: in out Integer) return Test_Result is

   T_Object: BRBON.Block.Static_Unprotected.Instance;
   T_Serializer: Serializable.Instance;
   Expected_Bytes: Array_Of_Unsigned_8_Ptr;
   S: Serializable.Instance;
   Cursor: Unsigned_32;

   Type_1_Block_No_Parameters: Array_Of_Unsigned_8 :=
     (
      -- 4 Sync bytes, 16#96, 16#7F, 16#81, little endian => 16#5A, big endian => 16#A5
      -- Offset 16#00
      16#96#, 16#7F#, 16#81#, 16#5A#,

      -- Type, Single Item File => 1
      -- Offset 16#04
      16#01#, 16#00#,

      -- Options, none => 0
      -- Offset 16#06
      16#00#, 16#00#,

      -- Block Byte Count, minimum length => 160 bytes (16#A0#)
      -- Offset 16#08
      16#A0#, 16#00#, 16#00#, 16#00#,

      -- Block header byte count, 17 * 8 = 136 bytes (16#88#)
      -- Offset 16#0C
      16#88#, 16#00#,

      -- Encrypted header byte count, unused => 0
      -- Offset 16#0E
      16#00#, 16#00#,

      -- Block Origin CRC16, unused => 0
      -- Offset 16#10
      16#79#, 16#7E#,

      -- Block Identifier CRC16, unused => 0
      -- Offset 16#12
      16#A9#, 16#7E#,

      -- Block Extension CRC16, unused => 0
      -- Offset 16#14
      16#62#, 16#3D#,

      -- Block Path Prefix CRC16, unused => 0
      -- Offset 16#16
      16#BF#, 16#60#,

      -- Block Origin byte count, unused => 0
      -- Offset 16#18
      16#09#,

      -- Block identifier byte count, unused => 0
      -- Offset 16#19
      16#08#,

      -- Block extension byte count, unused => 0
      -- Offset 16#1A
      16#03#,

      -- Block path prefix byte count, unused => 0
      -- Offset 16#1B
      16#0A#,

      -- Block Origin offset, unused => 0
      -- Offset 16#1C
      16#48#, 16#00#,

      -- Block identifier offset, unused => 0
      -- Offset 16#1E
      16#51#, 16#00#,

      -- Block extension offset, unused => 0
      -- Offset 16#20
      16#59#, 16#00#,

      -- Block path prefix offset, unused => 0
      -- Offset 16#22
      16#5C#, 16#00#,

      -- Block acquisition url byte count, unused => 0
      -- Offset 16#24
      16#09#, 16#00#,

      -- Block acquisition url offset, unused => 0
      -- Offset 16#26
      16#66#, 16#00#,

      -- Block Target List Byte Offset, unused => 0
      -- Offset 16#028
      16#06#, 16#00#,

      -- Block Target List Offset, unused => 0
      -- Offset 16#02A
      16#6F#, 16#00#,

      -- Block public key url byte count, unused => 0
      -- Offset 16#2C
      16#04#, 16#00#,

      -- Block public key url offset, unused => 0
      -- Offset 16#2E
      16#75#, 16#00#,

      -- Block Creation Time Stamp
      -- Offset 16#30
      16#21#, 16#43#, 16#65#, 16#87#,  16#78#, 16#56#, 16#34#, 16#12#,

      -- Block Modification Time Stamp
      -- Offset 16#38
      16#21#, 16#43#, 16#65#, 16#87#,  16#78#, 16#56#, 16#34#, 16#12#,

      -- Block Expiry Time Stamp
      -- Offset 16#40
      16#88#, 16#99#, 16#AA#, 16#BB#,  16#CC#, 16#DD#, 16#EE#, 16#FF#,

      -- Block Type dependent header (not used)

      -- Block Header Field Storage (56 bytes)
      -- Offset 16#48
      16#6C#, 16#6F#, 16#63#, 16#61#,  16#6C#, 16#68#, 16#6F#, 16#73#,
      16#74#, 16#54#, 16#65#, 16#73#,  16#74#, 16#66#, 16#69#, 16#6C#,
      16#65#, 16#74#, 16#78#, 16#74#,  16#4F#, 16#70#, 16#65#, 16#6E#,
      16#41#, 16#63#, 16#63#, 16#65#,  16#73#, 16#73#, 16#6C#, 16#6F#,
      16#63#, 16#61#, 16#6C#, 16#68#,  16#6F#, 16#73#, 16#74#, 16#46#,
      16#6F#, 16#72#, 16#41#, 16#6C#,  16#6C#, 16#4E#, 16#6F#, 16#6E#,
      16#65#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,

      -- Reserved

      -- Offset 16#80
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#,

      -- Block Header CRC16
      -- Offset 16#86
      16#FF#, 16#5E#,

      -- Item storage (16 bytes)
      -- Offset 16#88
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,

      -- Block Footer for Block Type 1
      -- Reserved
      16#00#, 16#00#, 16#00#, 16#00#,

      -- Block Content Crc 32
      16#00#, 16#00#, 16#00#, 16#00#
     );


   Type_1_Block_Dont_Care: Array_Of_Boolean :=
     (
      -- 4 Sync bytes, 16#96, 16#7F, 16#81, little endian => 16#5A, big endian => 16#A5
      -- Offset 16#00
      false, false, false, false,

      -- Type, Single Item File => 1
      -- Offset 16#04
      false, false,

      -- Options, none => 0
      -- Offset 16#06
      false, false,

      -- Block Byte Count, minimum length => 96 bytes (16#60)
      -- Offset 16#08
      false, false, false, false,

      -- Block header byte count, 80 bytes (16#50#)
      -- Offset 16#0C
      false, false,

      -- Encrypted header byte count, unused => 0
      -- Offset 16#0E
      false, false,

      -- Block Origin CRC16, unused => 0
      -- Offset 16#10
      false, false,

      -- Block Identifier CRC16, unused => 0
      -- Offset 16#12
      false, false,

      -- Block Extension CRC16, unused => 0
      -- Offset 16#14
      false, false,

      -- Block Path Prefix CRC16, unused => 0
      -- Offset 16#16
      false, false,

      -- Block Origin byte count, unused => 0
      -- Offset 16#18
      false,

      -- Block identifier byte count, unused => 0
      -- Offset 16#19
      false,

      -- Block extension byte count, unused => 0
      -- Offset 16#1A
      false,

      -- Block path prefix byte count, unused => 0
      -- Offset 16#1B
      false,

      -- Block Origin offset, unused => 0
      -- Offset 16#1C
      false, false,

      -- Block identifier offset, unused => 0
      -- Offset 16#1E
      false, false,

      -- Block extension offset, unused => 0
      -- Offset 16#20
      false, false,

      -- Block path prefix offset, unused => 0
      -- Offset 16#22
      false, false,

      -- Block acquisition url byte count, unused => 0
      -- Offset 16#24
      false, false,

      -- Block acquisition url offset, unused => 0
      -- Offset 16#26
      false, false,

      -- Block Target List Byte Offset, unused => 0
      -- Offset 16#028
      false, false,

      -- Block Target List Offset, unused => 0
      -- Offset 16#02A
      false, false,

      -- Block public key url byte count, unused => 0
      -- Offset 16#2C
      false, false,

      -- Block public key url offset, unused => 0
      -- Offset 16#2E
      false, false,

      -- Block Creation Time Stamp
      -- Offset 16#30
      false, false, false, false,  false, false, false, false,

      -- Block Modification Time Stamp
      -- Offset 16#38
      false, false, false, false,  false, false, false, false,

      -- Block Expiry Time Stamp
      -- Offset 16#40
      false, false, false, false, false, false, false, false,

      -- Block Type dependent header (not used)

      -- Block Header Field Storage (56 bytes)
      -- Offset 16#48
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,

      -- Reserved

      -- Offset 16#80
      false, false, false, false,  false, false,

      -- Block Header CRC16
      -- Offset 16#86
      false, false,

      -- Item storage (16 bytes)
      -- Offset 16#88
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,

      -- Block Footer for Block Type 1
      -- Reserved
      false, false, false, false,

      -- Block Content Crc 32
      false, false, false, false
     );

begin

   -- Setup

   Expected_Bytes := new Array_Of_Unsigned_8 (0 .. Type_1_Block_No_Parameters'Length - 1);
   Expected_Bytes.all := Type_1_Block_No_Parameters;


   T_Object := BRBON.Block.Static_Unprotected.Factory
     (Type_Of_Block                   => BRBON.Types.Single_Item,
      Minimum_Byte_Count              => 16,
      Header_Field_Storage_Byte_Count => 0,
      Options                         => BRBON.Types.No_Block_Options,
      Using_Endianness                => BRBON.Configure.Machine_Endianness,
      Origin                          => "localhost",
      Identifier                      => "Testfile",
      Extension                       => "txt",
      Path_Prefix                     => "OpenAccess",
      Acquisition_URL                 => "localhost",
      Target_List                     => "ForAll",
      Public_Key_URL                  => "None",
      Creation_Timestamp              => 16#1234_5678_8765_4321#,
      Expiry_Timestamp                => 16#FFEE_DDCC_BBAA_9988#);

   T_Serializer := T_Object.Test_Serializer;


   -- Test

   if T_Serializer.Remaining_Bytes /= Expected_Bytes'Length then

      New_Line (2);
      Put_Line ("Compare failed due to size mismatch");
      Put_Line ("Expected:" & Type_1_Block_No_Parameters'Length'Image & " bytes, found:" & T_Serializer.Remaining_Bytes'Image & " bytes.");

      return Failed;

   elsif not T_Serializer.Compare (Type_1_Block_No_Parameters, Type_1_Block_Dont_Care) then

      Cursor := T_Serializer.Index_Of_Last_Byte;

      New_Line (2);
      Put_Line ("Block verification failed");

      New_Line;
      Put_Line ("Expected:");
      S := Serializable.Create_With_Copy (Type_1_Block_No_Parameters);
      S.Dump_2_Lines (Around => Cursor);

      New_Line (2);
      Put_Line ("Found:");
      T_Serializer.Dump_2_Lines (Around => Cursor, Show_Cursor => True);

      return Failed;

   else

      return Passed;

   end if;

end Create_Block_Test_With_Param;

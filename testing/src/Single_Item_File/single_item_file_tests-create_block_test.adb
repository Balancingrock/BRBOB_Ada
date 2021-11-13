with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

with BRBON;
with BRBON.Static_Unprotected;
with BRBON.Block;
with BRBON.Configure;
with BRBON.Types; use BRBON.Types;

with Serializable;

separate (Single_Item_File_Tests)

function Create_Block_Test (Count: in out Integer) return Test_Result is

   T_Object: BRBON.Static_Unprotected.Instance;
   T_Serializer: Serializable.Instance;
   Expected_Bytes: Array_Of_Unsigned_8_Ptr;
   S: Serializable.Instance;
   Cursor: Unsigned_32;

   Type_1_Block: Array_Of_Unsigned_8 :=
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

      -- Block Byte Count, minimum length => 96 bytes (16#50)
      -- Offset 16#08
      16#60#, 16#00#, 16#00#, 16#00#,

      -- Block header byte count, 10 * 8 = 80 => 16#50# bytes
      -- Offset 16#0C
      16#50#, 16#00#,

      -- Encrypted header byte count, unused => 0
      -- Offset 16#0E
      16#00#, 16#00#,

      -- Block Origin CRC16, unused => 0
      -- Offset 16#10
      16#00#, 16#00#,

      -- Block Identifier CRC16, unused => 0
      -- Offset 16#12
      16#00#, 16#00#,

      -- Block Extension CRC16, unused => 0
      -- Offset 16#14
      16#00#, 16#00#,

      -- Block Path Prefix CRC16, unused => 0
      -- Offset 16#16
      16#00#, 16#00#,

      -- Block Origin byte count, unused => 0
      -- Offset 16#18
      16#00#,

      -- Block identifier byte count, unused => 0
      -- Offset 16#19
      16#00#,

      -- Block extension byte count, unused => 0
      -- Offset 16#1A
      16#00#,

      -- Block path prefix byte count, unused => 0
      -- Offset 16#1B
      16#00#,

      -- Block Origin offset, unused => 0
      -- Offset 16#1C
      16#00#, 16#00#,

      -- Block identifier offset, unused => 0
      -- Offset 16#1E
      16#00#, 16#00#,

      -- Block extension offset, unused => 0
      -- Offset 16#20
      16#00#, 16#00#,

      -- Block path prefix offset, unused => 0
      -- Offset 16#22
      16#00#, 16#00#,

      -- Block acquisition url byte count, unused => 0
      -- Offset 16#24
      16#00#, 16#00#,

      -- Block acquisition url offset, unused => 0
      -- Offset 16#26
      16#00#, 16#00#,

      -- Block Target List Byte Offset, unused => 0
      -- Offset 16#028
      16#00#, 16#00#,

      -- Block Target List Offset, unused => 0
      -- Offset 16#02A
      16#00#, 16#00#,

      -- Block public key url byte count, unused => 0
      -- Offset 16#2C
      16#00#, 16#00#,

      -- Block public key url offset, unused => 0
      -- Offset 16#2E
      16#00#, 16#00#,

      -- Block Creation Time Stamp
      -- Offset 16#30
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,

      -- Block Modification Time Stamp
      -- Offset 16#38
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,

      -- Block Expiry Time Stamp
      -- Offset 16#40
      16#7F#, 16#FF#, 16#FF#, 16#FF#,  16#FF#, 16#FF#, 16#FF#, 16#FF#,

      -- Block Type dependent header (not used)

      -- Block Header Field Storage (not used)

      -- Reserved

      -- Offset 16#48
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#,

      -- Block Header CRC16
      -- Offset 16#4E
      16#00#, 16#00#,

      -- Item storage (8 bytes)
      -- Offset 16#50
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

      -- Block Byte Count, minimum length => 96 bytes (16#50)
      -- Offset 16#08
      false, false, false, false,

      -- Block header byte count, ?
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
      true, true, true, true,  true, true, true, true,

      -- Block Modification Time Stamp
      -- Offset 16#38
      true, true, true, true,  true, true, true, true,

      -- Block Expiry Time Stamp
      -- Offset 16#40
      false, false, false, false, false, false, false, false,

      -- Block Type dependent header (not used)

      -- Block Header Field Storage (not used)

      -- Reserved

      -- Offset 16#48
      false, false, false, false,  false, false,

      -- Block Header CRC16
      -- Offset 16#4E
      false, false,

      -- Item storage (8 bytes)
      -- Offset 16#50
      false, false, false, false,  false, false, false, false,


      -- Block Footer for Block Type 1
      -- Reserved
      false, false, false, false,

      -- Block Content Crc 32
      false, false, false, false
     );

begin

   Expected_Bytes := new Array_Of_Unsigned_8 (1 .. Type_1_Block'Length);
   Expected_Bytes.all := Type_1_Block;

   T_Object := BRBON.Static_Unprotected.Factory
     (Block_Type         => BRBON.Block.Single_Item_File,
      Minimum_Byte_Count => 8,
      Using_Endianness   => BRBON.Configure.Machine_Endianness);

   T_Serializer := T_Object.Create_Serializable_Instance;


--   T_Serializer.Put_All;


   if not T_Serializer.Compare (Type_1_Block, Type_1_Block_Dont_Care) then

      Cursor := T_Serializer.Index_Of_Last_Byte;

      New_Line (2);
      Put_Line ("Block verification failed");

      New_Line;
      Put_Line ("Expected:");
      S := Serializable.Create_With_Copy (Type_1_Block);
      S.Dump_2_Lines (Around => Cursor);

--
      New_Line (2);
      Put_Line ("Found:");
      T_Serializer.Dump_2_Lines (Around => Cursor, Show_Cursor => True);

      return Failed;

   else
      return Passed;
   end if;

end Create_Block_Test;

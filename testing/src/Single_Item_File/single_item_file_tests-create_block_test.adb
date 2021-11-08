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
   E_Serializer: Serializable.Instance;
   Expected_Bytes: Array_Of_Unsigned_8_Ptr;

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

      -- Block Byte Count, minimum length => 80 bytes (16#50)
      -- Offset 16#08
      16#50#, 16#00#, 16#00#, 16#00#,

      -- Block header byte count, ?
      -- Offset 16#0C
      16#00#, 16#00#,

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
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,

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


begin

   Expected_Bytes := new Array_Of_Unsigned_8 (1 .. Type_1_Block'Length);
   Expected_Bytes.all := Type_1_Block;

   T_Object := BRBON.Static_Unprotected.Factory
     (Block_Type         => BRBON.Block.Single_Item_File,
      Minimum_Byte_Count => 8,
      Using_Endianness   => BRBON.Configure.Machine_Endianness);

   T_Serializer := T_Object.Create_In_Place_Serializable_Instance;

   E_Serializer := Serializable.New_Instance
     (Use_In_Place => Expected_Bytes,
      First        => Expected_Bytes.all'First,
      Last         => Expected_Bytes.all'Last);

   E_Serializer.Put_All;

   T_Serializer.Put_All;

   if not T_Serializer.Compare (E_Serializer) then

      New_Line (2);
      Put_Line ("Expected:");
      E_Serializer.Hex_Dump_With_Cursor;
      New_Line (2);
      Put_Line ("Found:");
      T_Serializer.Hex_Dump_With_Cursor;

      return Failed;

   else
      return Passed;
   end if;

end Create_Block_Test;

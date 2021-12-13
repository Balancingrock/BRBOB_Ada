with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

with BRBON;
with BRBON.Block.Static_Unprotected;
with BRBON.Block;
with BRBON.Configure;
with BRBON.Types; use BRBON.Types;

with Serializable;

separate (Single_Item_Tests)

function Create_Blocks_With_Types (Count: in out Integer) return Test_Result is

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

      -- Block Byte Count, minimum length => - bytes (16#D8#)
      -- Offset 16#08
      16#D8#, 16#00#, 16#00#, 16#00#,

      -- Block header byte count, 16#50#
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
      16#21#, 16#43#, 16#65#, 16#87#,  16#78#, 16#56#, 16#34#, 16#12#,

      -- Block Modification Time Stamp
      -- Offset 16#38
      16#21#, 16#43#, 16#65#, 16#87#,  16#78#, 16#56#, 16#34#, 16#12#,

      -- Block Expiry Time Stamp
      -- Offset 16#40
      16#88#, 16#99#, 16#AA#, 16#BB#,  16#CC#, 16#DD#, 16#EE#, 16#FF#,

      -- Block Type dependent header (not used)

      -- Block Header Field Storage (not used)
      -- Offset 16#48


      -- Reserved

      -- Offset 16#48
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#,

      -- Block Header CRC16
      -- Offset 16#4E
      16#38#, 16#B9#,

      -- Item storage (16 bytes)
      -- Offset 16#50
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --50
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --58
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --60
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --68
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --70
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --78
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --80
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --88
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --90
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --98
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --A0
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --A8
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --B0
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --B8
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --C0
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, --C8

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

      -- Block Byte Count, minimum length => - bytes (16#D8)
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

      -- Block Header Field Storage (not used)
      -- Offset 16#48

      -- Reserved

      -- Offset 16#48
      false, false, false, false,  false, false,

      -- Block Header CRC16
      -- Offset 16#4E
      false, false,

      -- Item storage (16 bytes)
      -- Offset 16#50
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,
      false, false, false, false,  false, false, false, false,

      -- Block Footer for Block Type 1
      -- Reserved
      false, false, false, false,

      -- Block Content Crc 32
      false, false, false, false
     );

   function Evaluate (Serializer: in out Serializable.Instance; Expected_Bytes: Array_Of_Unsigned_8_Ptr) return Test_Result is

      Cursor: Unsigned_32;
      S: Serializable.Instance;

   begin
      if Serializer.Remaining_Bytes /= Expected_Bytes.all'Length then

         New_Line (2);
         Put_Line ("Compare failed due to size mismatch");
         Put_Line ("Expected:" & Expected_Bytes'Length'Image & " bytes, found:" & Serializer.Remaining_Bytes'Image & " bytes.");

         return Failed;

      elsif not Serializer.Compare (Expected_Bytes.all, Type_1_Block_Dont_Care) then

         Cursor := Serializer.Index_Of_Last_Byte;

         New_Line (2);
         Put_Line ("Block verification failed");

         New_Line;
         Put_Line ("Expected:");
         S := Serializable.Create_With_Copy (Expected_Bytes.all);
         S.Dump_2_Lines (Around => Cursor);

         New_Line (2);
         Put_Line ("Found:");
         Serializer.Dump_2_Lines (Around => Cursor, Show_Cursor => True);

         return Failed;

      else

         return Passed;

      end if;

   end Evaluate;

begin

   -- Null Type

   declare

      T_Object: BRBON.Block.Static_Unprotected.Instance;
      T_Serializer: Serializable.Instance;
      Expected_Bytes: Array_Of_Unsigned_8_Ptr;
      --S: Serializable.Instance;
      --Cursor: Unsigned_32;

   begin

      Expected_Bytes := new Array_Of_Unsigned_8 (0 .. Type_1_Block_No_Parameters'Length - 1);
      Expected_Bytes.all := Type_1_Block_No_Parameters;
      Expected_Bytes.all (16#50# .. 16#5F#) :=
        (16#01#, 16#00#, 16#00#, 16#00#,  16#10#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#);

      T_Object := BRBON.Block.Static_Unprotected.Factory
        (Type_Of_Block                   => BRBON.Types.Single_Item,
         Minimum_Byte_Count              => 128,
         Header_Field_Storage_Byte_Count => 0,
         Options                         => BRBON.Types.No_Block_Options,
         Using_Endianness                => BRBON.Configure.Machine_Endianness,
         Origin                          => "",
         Identifier                      => "",
         Extension                       => "",
         Path_Prefix                     => "",
         Acquisition_URL                 => "",
         Target_List                     => "",
         Public_Key_URL                  => "",
         Creation_Timestamp              => 16#1234_5678_8765_4321#,
         Expiry_Timestamp                => 16#FFEE_DDCC_BBAA_9988#);

      T_Object.Add_Root_Item (Of_Type         => BRBON.Types.Null_Type,
                              With_Byte_Count => 0,
                              With_Name       => "");

      T_Serializer := T_Object.Test_Serializer;


      if Evaluate (T_Serializer, Expected_Bytes) /= Passed then
         return Failed;
      end if;

   end; -- Null check


   -- Bool Type

   declare

      T_Object: BRBON.Block.Static_Unprotected.Instance;
      T_Serializer: Serializable.Instance;
      Expected_Bytes: Array_Of_Unsigned_8_Ptr;

   begin

      Expected_Bytes := new Array_Of_Unsigned_8 (0 .. Type_1_Block_No_Parameters'Length - 1);
      Expected_Bytes.all := Type_1_Block_No_Parameters;
      Expected_Bytes.all (16#50# .. 16#5F#) :=
        (16#02#, 16#00#, 16#00#, 16#00#,  16#10#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#);

      T_Object := BRBON.Block.Static_Unprotected.Factory
        (Type_Of_Block                   => BRBON.Types.Single_Item,
         Minimum_Byte_Count              => 128,
         Header_Field_Storage_Byte_Count => 0,
         Options                         => BRBON.Types.No_Block_Options,
         Using_Endianness                => BRBON.Configure.Machine_Endianness,
         Origin                          => "",
         Identifier                      => "",
         Extension                       => "",
         Path_Prefix                     => "",
         Acquisition_URL                 => "",
         Target_List                     => "",
         Public_Key_URL                  => "",
         Creation_Timestamp              => 16#1234_5678_8765_4321#,
         Expiry_Timestamp                => 16#FFEE_DDCC_BBAA_9988#);

      T_Object.Add_Root_Item (Of_Type         => BRBON.Types.Bool_Type,
                              With_Byte_Count => 0,
                              With_Name       => "");

      T_Serializer := T_Object.Test_Serializer;


      if Evaluate (T_Serializer, Expected_Bytes) /= Passed then
         return Failed;
      end if;

   end; -- Bool check

   return Passed;

end Create_Blocks_With_Types;

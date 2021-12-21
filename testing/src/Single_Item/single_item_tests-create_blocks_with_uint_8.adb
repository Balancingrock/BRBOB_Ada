with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

with BRBON;
with BRBON.Block.Static_Unprotected;
with BRBON.Block; use BRBON.Block;
with BRBON.Configure;
with BRBON.Types; use BRBON.Types;
with BRBON.Portal;

with Support;
with Serializable;

separate (Single_Item_Tests)

function Create_Blocks_With_UInt_8 (Count: in out Integer) return Test_Result is

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


   Skip_Map: Array_Of_Boolean (Unsigned_32 range Type_1_Block'Range) :=
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


   T_Object: BRBON.Block.Static_Unprotected.Instance;
   T_Serializer: Serializable.Instance;
   Expected_Bytes: Array_Of_Unsigned_8_Ptr;
   P: BRBON.Portal.Instance;

begin

   Expected_Bytes := new Array_Of_Unsigned_8 (Type_1_Block'Range);
   Expected_Bytes.all := Type_1_Block;
   Expected_Bytes.all (16#50# .. 16#6F#) :=
     (16#07#, 16#00#, 16#00#, 16#10#,  16#20#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#,
      16#14#, 16#1A#, 16#0A#, 16#41#,  16#6E#, 16#79#, 16#49#, 16#6E#,
      16#74#, 16#65#, 16#67#, 16#65#,  16#72#, 16#00#, 16#00#, 16#00#
     );

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

   P := T_Object.Add_Root_Item (Of_Type         => BRBON.Types.UInt_8_Type,
                                With_Byte_Count => 0,
                                With_Name       => "AnyInteger");

   T_Serializer := T_Object.Test_Serializer;


   if Support.Verify_Array_Of_Unsigned_8 (T_Serializer, Expected_Bytes, Skip_Map) /= Passed then
      return Failed;
   end if;

   if Static_Unprotected.Get_Type (P) /= UInt_8_Type then
      New_Line (2);
      Put_Line ("Expected the type 'UInt_8_Type', found:" & Static_Unprotected.Get_Type (P)'Image);
      return Failed;
   end if;

   if Static_Unprotected.Get_Options (P) /= No_Item_Options then
      New_Line (2);
      Put_Line ("Expected 'No_Options (0)', found:" & To_Unsigned_8 (Static_Unprotected.Get_Options (P))'Image);
      return Failed;
   end if;

   if Static_Unprotected.Get_Flags (P) /= No_Item_Flags then
      New_Line (2);
      Put_Line ("Expected 'No_Flags (0)', found:" & To_Unsigned_8 (Static_Unprotected.Get_Flags (P))'Image);
      return Failed;
   end if;

   if Static_Unprotected.Get_Name (P) /= "AnyInteger" then
      New_Line (2);
      Put_Line ("Expected 'AnyInteger', found:" & Static_Unprotected.Get_Name (P));
      return Failed;
   end if;

   if Static_Unprotected.Get_Parent_Offset (P) /= 0 then
      New_Line (2);
      Put_Line ("Expected Parent_Offset of 0, found:" & Static_Unprotected.Get_Parent_Offset (P)'Image);
      return Failed;
   end if;

   if Static_Unprotected.Get_Byte_Count (P) /= 32 then
      New_Line (2);
      Put_Line ("Expected Byte_Count of 32, found: " & Static_Unprotected.Get_Byte_Count (P)'Image);
      return Failed;
   end if;

   if Static_Unprotected.Get_Value_Area_Byte_Count (P) /= 0 then
      New_Line (2);
      Put_Line ("Expected Value_Area_Byte_Count of 0, found:" & Static_Unprotected.Get_Value_Area_Byte_Count (P)'Image);
      return Failed;
   end if;

   if Static_Unprotected.Get_UInt_8 (P) /= 0 then
      New_Line (2);
      Put_Line ("Expected initial value '0', found:" & Static_Unprotected.Get_Int_8 (P)'Image);
      return Failed;
   end if;

   Static_Unprotected.Set_UInt_8 (P, 34);

   if Static_Unprotected.Get_UInt_8 (P) /= 34 then
      New_Line (2);
      Put_Line ("Expected value '34', found:" & Static_Unprotected.Get_Int_8 (P)'Image);
      return Failed;
   end if;

   Expected_Bytes.all (16#50# .. 16#6F#) :=
     (16#07#, 16#00#, 16#00#, 16#10#,  16#20#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#,      34, 16#00#, 16#00#, 16#00#,
      16#14#, 16#1A#, 16#0A#, 16#41#,  16#6E#, 16#79#, 16#49#, 16#6E#,
      16#74#, 16#65#, 16#67#, 16#65#,  16#72#, 16#00#, 16#00#, 16#00#
     );

   T_Serializer := T_Object.Test_Serializer;

   return Support.Verify_Array_Of_Unsigned_8 (T_Serializer, Expected_Bytes, Skip_Map);

end Create_Blocks_With_UInt_8;

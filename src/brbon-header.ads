with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Header is

   Block_Synchronization_Byte_1_Offset:      constant Unsigned_32 := 16#00#; -- 1 byte
   Block_Synchronization_Byte_2_Offset:      constant Unsigned_32 := 16#01#; -- 1 byte
   Block_Synchronization_Byte_3_Offset:      constant Unsigned_32 := 16#02#; -- 1 byte
   Block_Synchronization_Byte_4_Offset:      constant Unsigned_32 := 16#03#; -- 1 byte
   Block_Type_Offset:                        constant Unsigned_32 := 16#04#; -- 2 bytes
   Block_Options_Offset:                     constant Unsigned_32 := 16#06#; -- 2 bytes

   Block_Byte_Count_Offset:                  constant Unsigned_32 := 16#08#; -- 4 bytes
   Block_Header_Byte_Count_Offset:           constant Unsigned_32 := 16#0C#; -- 2 Bytes
   Block_Encrypted_Header_Byte_Count_Offset: constant Unsigned_32 := 16#0E#; -- 2 Bytes

   Block_Origin_CRC16_Offset:                constant Unsigned_32 := 16#10#; -- 2 Bytes
   Block_Identifier_CRC16_Offset:            constant Unsigned_32 := 16#12#; -- 2 Bytes
   Block_Extension_CRC16_Offset:             constant Unsigned_32 := 16#14#; -- 2 Bytes
   Block_Path_Prefix_CRC16_Offset:           constant Unsigned_32 := 16#16#; -- 2 Bytes

   Block_Origin_Byte_Count_Offset:           constant Unsigned_32 := 16#18#; -- 1 Byte
   Block_Identifier_Byte_Count_Offset:       constant Unsigned_32 := 16#19#; -- 1 Byte
   Block_Extension_Byte_Count_Offset:        constant Unsigned_32 := 16#1A#; -- 1 Byte
   Block_Path_Prefix_Byte_Count_Offset:      constant Unsigned_32 := 16#1B#; -- 1 Byte
   Block_Origin_Offset_Offset:               constant Unsigned_32 := 16#1C#; -- 2 Bytes
   Block_Identifier_Offset_Offset:           constant Unsigned_32 := 16#1E#; -- 2 Bytes

   Block_Extension_Offset_Offset:            constant Unsigned_32 := 16#20#; -- 2 Bytes
   Block_Path_Prefix_Offset_Offset:          constant Unsigned_32 := 16#22#; -- 2 Bytes
   Block_Acquisition_URL_Byte_Count_Offset:  constant Unsigned_32 := 16#24#; -- 2 Bytes
   Block_Acquisition_URL_Offset_Offset:      constant Unsigned_32 := 16#26#; -- 2 Bytes

   Block_Target_List_Byte_Count_Offset:      constant Unsigned_32 := 16#28#; -- 2 Bytes
   Block_Target_List_Offset_Offset:          constant Unsigned_32 := 16#2A#; -- 2 Bytes
   Block_Public_Key_URL_Byte_Count_Offset:   constant Unsigned_32 := 16#2C#; -- 2 Bytes
   Block_Public_Key_URL_Offset_Offset:       constant Unsigned_32 := 16#2E#; -- 2 Bytes

   Block_Creation_Timestamp_Offset:          constant Unsigned_32 := 16#30#; -- 8 Bytes
   Block_Modification_Timestamp_Offset:      constant Unsigned_32 := 16#38#; -- 8 Bytes
   Block_Expiry_Timestamp_Offset:            constant Unsigned_32 := 16#40#; -- 8 Bytes

   Block_Type_Dependant_Header_Offset:       constant Unsigned_32 := 16#48#; -- N * 8 Bytes

   Block_Header_Field_Storage_Type_1_Offset: constant Unsigned_32 := 16#48#; -- M * 8 Bytes

   Block_Reserved_1a_Offset:                 constant Unsigned_32 := 8;
   Block_Reserved_1b_Offset:                 constant Unsigned_32 := 4;
   Block_Header_CRC16_Offset:                constant Unsigned_32 := 2;

   Block_Synchronization_Byte_1_Expected_Value:               constant Unsigned_8 := 16#96#;
   Block_Synchronization_Byte_2_Expected_Value:               constant Unsigned_8 := 16#7F#;
   Block_Synchronization_Byte_3_Expected_Value:               constant Unsigned_8 := 16#81#;
   Block_Synchronization_Byte_4_Little_Endian_Expected_Value: constant Unsigned_8 := 16#5A#;
   Block_Synchronization_Byte_4_Big_Endian_Expected_Value:    constant Unsigned_8 := 16#A5#;


   -- The size of the fixed part of a block header
   --
   Fixed_Part_Byte_Count: constant Unsigned_16 := 9 * 8;

   -- The size of the fixed part of a block header after the type dependent header and the field storage
   --
   Past_Storage_Field_Byte_Count: constant Unsigned_16 := 8;


end BRBON.Header;

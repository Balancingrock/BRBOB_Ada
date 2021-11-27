with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Header is

   Synchronization_Byte_1_Offset:      constant Unsigned_32 := 16#00#; -- 1 byte
   Synchronization_Byte_2_Offset:      constant Unsigned_32 := 16#01#; -- 1 byte
   Synchronization_Byte_3_Offset:      constant Unsigned_32 := 16#02#; -- 1 byte
   Synchronization_Byte_4_Offset:      constant Unsigned_32 := 16#03#; -- 1 byte
   Type_Offset:                        constant Unsigned_32 := 16#04#; -- 2 bytes
   Options_Offset:                     constant Unsigned_32 := 16#06#; -- 2 bytes

   Byte_Count_Offset:                  constant Unsigned_32 := 16#08#; -- 4 bytes
   Header_Byte_Count_Offset:           constant Unsigned_32 := 16#0C#; -- 2 Bytes
   Encrypted_Header_Byte_Count_Offset: constant Unsigned_32 := 16#0E#; -- 2 Bytes

   Origin_CRC16_Offset:                constant Unsigned_32 := 16#10#; -- 2 Bytes
   Identifier_CRC16_Offset:            constant Unsigned_32 := 16#12#; -- 2 Bytes
   Extension_CRC16_Offset:             constant Unsigned_32 := 16#14#; -- 2 Bytes
   Path_Prefix_CRC16_Offset:           constant Unsigned_32 := 16#16#; -- 2 Bytes

   Origin_Byte_Count_Offset:           constant Unsigned_32 := 16#18#; -- 1 Byte
   Identifier_Byte_Count_Offset:       constant Unsigned_32 := 16#19#; -- 1 Byte
   Extension_Byte_Count_Offset:        constant Unsigned_32 := 16#1A#; -- 1 Byte
   Path_Prefix_Byte_Count_Offset:      constant Unsigned_32 := 16#1B#; -- 1 Byte
   Origin_Offset_Offset:               constant Unsigned_32 := 16#1C#; -- 2 Bytes
   Identifier_Offset_Offset:           constant Unsigned_32 := 16#1E#; -- 2 Bytes

   Extension_Offset_Offset:            constant Unsigned_32 := 16#20#; -- 2 Bytes
   Path_Prefix_Offset_Offset:          constant Unsigned_32 := 16#22#; -- 2 Bytes
   Acquisition_URL_Byte_Count_Offset:  constant Unsigned_32 := 16#24#; -- 2 Bytes
   Acquisition_URL_Offset_Offset:      constant Unsigned_32 := 16#26#; -- 2 Bytes

   Target_List_Byte_Count_Offset:      constant Unsigned_32 := 16#28#; -- 2 Bytes
   Target_List_Offset_Offset:          constant Unsigned_32 := 16#2A#; -- 2 Bytes
   Public_Key_URL_Byte_Count_Offset:   constant Unsigned_32 := 16#2C#; -- 2 Bytes
   Public_Key_URL_Offset_Offset:       constant Unsigned_32 := 16#2E#; -- 2 Bytes

   Creation_Timestamp_Offset:          constant Unsigned_32 := 16#30#; -- 8 Bytes
   Modification_Timestamp_Offset:      constant Unsigned_32 := 16#38#; -- 8 Bytes
   Expiry_Timestamp_Offset:            constant Unsigned_32 := 16#40#; -- 8 Bytes

   Type_Dependant_Header_Offset:       constant Unsigned_32 := 16#48#; -- N * 8 Bytes

   Header_Field_Storage_Type_1_Offset: constant Unsigned_32 := 16#48#; -- M * 8 Bytes

   Reserved_1a_Distance_Before_Header_End:              constant Unsigned_32 := 8; -- Acts as minus value
   Reserved_1b_Distance_Before_Header_End:              constant Unsigned_32 := 4; -- Acts as minus value
   Header_CRC16_Distance_Before_Header_End:             constant Unsigned_32 := 2; -- Acts as minus value


   -- Expected synchronization values
   --
   Synchronization_Byte_1_Expected_Value:               constant Unsigned_8 := 16#96#;
   Synchronization_Byte_2_Expected_Value:               constant Unsigned_8 := 16#7F#;
   Synchronization_Byte_3_Expected_Value:               constant Unsigned_8 := 16#81#;
   Synchronization_Byte_4_Little_Endian_Expected_Value: constant Unsigned_8 := 16#5A#;
   Synchronization_Byte_4_Big_Endian_Expected_Value:    constant Unsigned_8 := 16#A5#;


   -- The size of the fixed part of a block header
   --
   Fixed_Part_Byte_Count: constant Unsigned_16 := 9 * 8;

   -- The size of the fixed part of a block header after the type dependent header and the field storage
   --
   Past_Storage_Field_Byte_Count: constant Unsigned_16 := 8;



end BRBON.Header;

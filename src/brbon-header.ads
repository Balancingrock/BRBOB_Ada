with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Header_Package is

private

   -- Type_Dependant_Header_Offset:       constant Unsigned_32 := 16#48#; -- N * 8 Bytes

   -- Header_Field_Storage_Type_1_Offset: constant Unsigned_32 := 16#48#; -- M * 8 Bytes

   -- Reserved_1a_Distance_Before_Header_End:   constant Unsigned_32 := 8; -- Acts as minus value
   -- Reserved_1b_Distance_Before_Header_End:   constant Unsigned_32 := 4; -- Acts as minus value
   -- Header_CRC16_Distance_Before_Header_End:  constant Unsigned_32 := 2; -- Acts as minus value


   -- The size of the fixed part of a block header
   --
   Block_Header_Begin_Byte_Count: constant Unsigned_16 := 96;


   -- The block header layout
   --
   type Block_Header_Leading is
      record
         Synchronization_Byte_1: Unsigned_8;
         Synchronization_Byte_2: Unsigned_8;
         Synchronization_Byte_3: Unsigned_8;
         Synchronization_Byte_4: Unsigned_8;
         Is_Type: Types.Block_Type;                -- 2 bytes
         Options: Types.Block_Options;             -- 2 bytes

         Block_Byte_Count: Unsigned_32;
         Header_Byte_Count: Unsigned_16;
         Encrypted_Header_Byte_Count: Unsigned_16;

         Origin_CRC: Unsigned_16;
         Identifier_CRC: Unsigned_16;
         Extension_CRC: Unsigned_16;
         Path_Prefix_CRC: Unsigned_16;

         Origin_Byte_Count: Unsigned_8;
         Identifier_Byte_Count: Unsigned_8;
         Extension_Byte_Count: Unsigned_8;
         Path_Prefix_Byte_Count: Unsigned_8;
         Origin_Offset: Unsigned_16;
         Identifier_Offset: Unsigned_16;

         Extension_Offset: Unsigned_16;
         Path_Prefix_Offset: Unsigned_16;
         Acquisition_URL_Byte_Count: Unsigned_16;
         Acquisition_URL_Offset: Unsigned_16;

         Target_List_Byte_Count: Unsigned_16;
         Target_List_Offset: Unsigned_16;
         Public_Key_URL_Byte_Count: Unsigned_16;
         Public_Key_Offset: Unsigned_16;

         Creation_Timestamp: Unsigned_64;
         Modification_Timestamp: Unsigned_64;
         Expiry_Timestamp: Unsigned_64;
      end record;

   for Block_Header_Layout'Size use Block_Header_Layout_Byte_Count * 8;

   for Block_Header_Layout use
      record
         Synchronization_Byte_1     at 0 range 0..7;
         Synchronization_Byte_2     at 1 range 0..7;
         Synchronization_Byte_3     at 2 range 0..7;
         Synchronization_Byte_4     at 3 range 0..7;
         Is_Type                    at 4 range 0..15;
         Options                    at 6 range 0..15;

         Block_Byte_Count           at 8 range 0..31;
         Header_Byte_Count          at 12 range 0..15;
         Encrypted_Header           at 14 range 0..15;

         Origin_CRC                 at 16 range 0..15;
         Identifier_CRC             at 18 range 0..15;
         Extension_CRC              at 20 range 0..15;
         Path_Prefix_CRC            at 22 range 0..15;

         Origin_Byte_Count          at 24 range 0..7;
         Identifier_Byte_Count      at 25 range 0..7;
         Extension_Byte_Count       at 26 range 0..7;
         Path_Prefix_Byte_Count     at 27 range 0..7;
         Origin_Offset              at 28 range 0..15;
         Identifier_Offset          at 30 range 0..15;

         Extension_Offset           at 32 range 0..15;
         Path_Prefix_Offset         at 34 range 0..15;
         Acquisition_URL_Byte_Count at 36 range 0..15;
         Acquisition_URL_Offset     at 38 range 0..15;

         Target_List_Byte_Count     at 40 range 0..15;
         Target_List_Offset         at 42 range 0..15;
         Public_Key_URL_Byte_Count  at 44 range 0..15;
         Public_Key_Offset          at 46 range 0..15;

         Creation_Timestamp         at 48 range 0..63;
         Modification_Timestamp     at 64 range 0..63;
         Expiry_Timestamp           at 80 range 0..63;
      end record;


   -- The size of the fixed part of a block header after the type dependent header and the field storage
   --
   Past_Field_Storage_Byte_Count: constant Unsigned_16 := 8;


   -- Expected synchronization values
   --
   Synchronization_Byte_1_Expected_Value:               constant Unsigned_8 := 16#96#;
   Synchronization_Byte_2_Expected_Value:               constant Unsigned_8 := 16#7F#;
   Synchronization_Byte_3_Expected_Value:               constant Unsigned_8 := 16#81#;
   Synchronization_Byte_4_Little_Endian_Expected_Value: constant Unsigned_8 := 16#5A#;
   Synchronization_Byte_4_Big_Endian_Expected_Value:    constant Unsigned_8 := 16#A5#;

end BRBON.Header_Package;

with BRBON.Utils;

package body BRBON.Block.Header.Single_Item_File is


   procedure Create
      (
         In_Container: in out BRBON.Container.Instance;
         Minimum_Byte_Count: Unsigned_32;
         Options: BRBON.Block.Options;
         Using_Endianness: Endianness;
         Origin: String;
         Identifier: String;
         Extension: String;
         Path_Prefix: String;
         Acquisition_URL: String;
         Target_List: String;
         Public_Key_URL: String;
         Creation_Timestamp: Unsigned_64;
         Expiry_Timestamp: Unsigned_64;
      ) is

      C: BRBON.Container.Instance renames In_Container;
      Now: constant Unsigned_64 := Utils.Milli_Sec_Since_Jan_1_1970;

   begin

      Set_Synchronization_Byte_1 (C);
      Set_Synchronization_Byte_2 (C);
      Set_Synchronization_Byte_3 (C);
      Set_Synchronization_Byte_4 (C); -- Uses the endianness set in the block header above
      Set_Block_Type  (C, Block.Single_Item_File);
      Set_Block_Options (C, Block.Header.No_Options);

      Set_Block_Byte_Count (C, C.Byte_Count);
      Set_Block_Header_Byte_Count (C, Block.Header.Minimum_Byte_Count (Block.Single_Item_File)); -- Type 1 does not use the type dependent header or the field storage
      Set_Block_Encrypted_Header_Byte_Count (C, 0);

      Set_Block_Origin_Crc16 (C, 0);
      Set_Block_Identifier_Crc16 (C, 0);
      Set_Block_Extension_Crc16 (C, 0);
      Set_Block_Path_Prefix_Crc16 (C, 0);

      Set_Block_Origin_Byte_Count (C, 0);
      Set_Block_Identifier_Byte_Count (C, 0);
      Set_Block_Extension_Byte_Count (C, 0);
      Set_Block_Path_Prefix_Byte_Count (C, 0);
      Set_Block_Origin_Offset (C, 0);
      Set_Block_Identifier_Offset (C, 0);

      Set_Block_Extension_Offset (C, 0);
      Set_Block_Path_Prefix_Offset (C, 0);
      Set_Block_Acquisition_URL_Byte_Count (C, 0);
      Set_Block_Acquisition_URL_Offset (C, 0);

      Set_Block_Target_List_Byte_Count (C, 0);
      Set_Block_Target_List_Offset (C, 0);
      Set_Block_Public_Key_URL_Byte_Count (C, 0);
      Set_Block_Public_Key_URL_Offset (C, 0);

      Set_Block_Creation_Timestamp (C, Now);
      Set_Block_Modification_Timestamp (C, Now);
      Set_Block_Expiry_Timestamp (C, 16#7FFF_FFFF_FFFF_FFFF#);

      Set_Reserved_1a (C, Get_Block_Header_Byte_Count (C), 0);
      Set_Reserved_1b (C, Get_Block_Header_Byte_Count (C), 0);
      Update_Block_Header_Crc16 (C);

   end Create;


end BRBON.Block.Header.Single_Item_File;

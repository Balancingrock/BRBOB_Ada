with BRBON.Block; use BRBON.Block;

separate (BRBON.Block)

procedure Create_Single_Item_Block_Header
     (
       In_Block: in out Instance;
       Field_Storage_Byte_Count: Unsigned_16 := 1;
       Options: Block_Options := No_Block_Options;
       Origin: String := "localhost";
       Identifier: String := "";
       Extension: String := "";
       Path_Prefix: String := "";
       Acquisition_URL: String := "";
       Target_List: String := "";
       Public_Key_URL: String := "";
       Creation_Timestamp: Unsigned_64 := BRBON.Utils.Milli_Sec_Since_Jan_1_1970;
       Expiry_Timestamp: Unsigned_64 := 16#7FFF_FFFF_FFFF_FFFF#
     ) is

      B: Instance renames In_Block;

      -- Calculate the space needed for the variable part of the block header
      --
      Variable_Part_Byte_Count: Unsigned_32 := BRBON.Utils.Round_Up_To_Nearest_Multiple_of_8
        (
         Unsigned_32 (Origin'Length + Identifier'Length + Extension'Length + Path_Prefix'Length + Acquisition_URL'Length + Target_List'Length + Public_Key_URL'Length)
        );

begin

   B.Header_Set_Synchronization_Byte_1;
   B.Header_Set_Synchronization_Byte_1;
   B.Header_Set_Synchronization_Byte_2;
   B.Header_Set_Synchronization_Byte_3;
   B.Header_Set_Synchronization_Byte_4; -- Assumes the endianness is already set in the container
   B.Header_Set_Type (Types.Single_Item);
   B.Header_Set_Options (No_Block_Options);

   B.Header_Set_Byte_Count (B.Container.Byte_Count);
   B.Header_Set_Header_Byte_Count (Header_Byte_Count); -- Type 1 does not use the type dependent header or the field storage
   B.Header_Set_Encrypted_Header_Byte_Count (0);

   B.Header_Set_Origin_Crc16 (0);
   B.Header_Set_Identifier_Crc16 (0);
   B.Header_Set_Extension_Crc16 (0);
   B.Header_Set_Path_Prefix_Crc16 (0);

   B.Header_Set_Origin_Byte_Count (0);
   B.Header_Set_Identifier_Byte_Count (0);
   B.Header_Set_Extension_Byte_Count (0);
   B.Header_Set_Path_Prefix_Byte_Count (0);
   B.Header_Set_Origin_Offset (0);
   B.Header_Set_Identifier_Offset (0);

   B.Header_Set_Extension_Offset (0);
   B.Header_Set_Path_Prefix_Offset (0);
   B.Header_Set_Acquisition_URL_Byte_Count (0);
   B.Header_Set_Acquisition_URL_Offset (0);

   B.Header_Set_Target_List_Byte_Count (0);
   B.Header_Set_Target_List_Offset (0);
   B.Header_Set_Public_Key_URL_Byte_Count (0);
   B.Header_Set_Public_Key_URL_Offset (0);

   B.Header_Set_Creation_Timestamp (Creation_Timestamp);
   B.Header_Set_Modification_Timestamp (Creation_Timestamp);
   B.Header_Set_Expiry_Timestamp (Expiry_Timestamp);

   B.Header_Set_Reserved_1a (B.Get_Header_Byte_Count, 0);
   B.Header_Set_Reserved_1b (B.Get_Header_Byte_Count, 0);
   B.Header_Update_Block_Header_Crc16;


end Create_Single_Item_Block_Header;

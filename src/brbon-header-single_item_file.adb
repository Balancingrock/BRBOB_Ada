with BRBON.Utils;

package body BRBON.Header.Single_Item_File is


   procedure Create
      (
       In_Container: in out BRBON.Container.Instance;
       Header_Byte_Count: Unsigned_16;
       Options: Block_Options;
       Origin: String;
       Identifier: String;
       Extension: String;
       Path_Prefix: String;
       Acquisition_URL: String;
       Target_List: String;
       Public_Key_URL: String;
       Creation_Timestamp: Unsigned_64;
       Expiry_Timestamp: Unsigned_64
      ) is

      C: BRBON.Container.Instance renames In_Container;

      -- Calculate the space needed for the variable part of the block header
      --
      Variable_Part_Byte_Count: Unsigned_32 := BRBON.Utils.Round_Up_To_Nearest_Multiple_of_8
        (
         Unsigned_32 (Origin'Length + Identifier'Length + Extension'Length + Path_Prefix'Length + Acquisition_URL'Length + Target_List'Length + Public_Key_URL'Length)
        );

   begin


      Set_Synchronization_Byte_1 (C);
      Set_Synchronization_Byte_2 (C);
      Set_Synchronization_Byte_3 (C);
      Set_Synchronization_Byte_4 (C); -- Uses the endianness set in the block header above
      Set_Block_Type  (C, BRBON.Types.Single_Item_File);
      Set_Block_Options (C, No_Block_Options);

      Set_Block_Byte_Count (C, C.Byte_Count);
      Set_Block_Header_Byte_Count (C, Header_Byte_Count); -- Type 1 does not use the type dependent header or the field storage
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

      Set_Block_Creation_Timestamp (C, Creation_Timestamp);
      Set_Block_Modification_Timestamp (C, Creation_Timestamp);
      Set_Block_Expiry_Timestamp (C, Expiry_Timestamp);

      Set_Reserved_1a (C, Get_Block_Header_Byte_Count (C), 0);
      Set_Reserved_1b (C, Get_Block_Header_Byte_Count (C), 0);
      Update_Block_Header_Crc16 (C);

   end Create;


end BRBON.Header.Single_Item_File;

with BRBON.Utils;

package body BRBON.Block.Header.Single_Item_File is


   function Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block.Header.Instance is
      H: Block.Header.Instance;
      Store: Container.Instance := Container.Factory (Memory_Area_Ptr, Using_Endianness);
      Now: constant Unsigned_64 := Utils.Milli_Sec_Since_Jan_1_1970;
   begin
      H.Store := Store;
      H.Endianness := Using_Endianness;

      H.Set_Synchronization_Byte_1;
      H.Set_Synchronization_Byte_2;
      H.Set_Synchronization_Byte_3;
      H.Set_Synchronization_Byte_4; -- Uses the endianness set in the block header above
      H.Set_Block_Type (Value => Block.Single_Item_File);
      H.Set_Block_Options (Value => Block.Header.No_Options);

      H.Set_Block_Byte_Count (Value => H.Store.Byte_Count);
      H.Set_Block_Header_Byte_Count (Value => Block.Header.Minimum_Byte_Count (Block.Single_Item_File)); -- Type 1 does not use the type dependent header or the field storage
      H.Set_Block_Encrypted_Header_Byte_Count (Value => 0);

      H.Set_Block_Origin_Crc16 (Value => 0);
      H.Set_Block_Identifier_Crc16 (Value => 0);
      H.Set_Block_Extension_Crc16 (Value => 0);
      H.Set_Block_Path_Prefix_Crc16 (Value => 0);

      H.Set_Block_Origin_Byte_Count (Value => 0);
      H.Set_Block_Identifier_Byte_Count (Value => 0);
      H.Set_Block_Extension_Byte_Count (Value => 0);
      H.Set_Block_Path_Prefix_Byte_Count (Value => 0);
      H.Set_Block_Origin_Offset (Value => 0);
      H.Set_Block_Identifier_Offset (Value => 0);

      H.Set_Block_Extension_Offset (Value => 0);
      H.Set_Block_Path_Prefix_Offset (Value => 0);
      H.Set_Block_Acquisition_URL_Byte_Count (Value => 0);
      H.Set_Block_Acquisition_URL_Offset (Value => 0);

      H.Set_Block_Target_List_Byte_Count (Value => 0);
      H.Set_Block_Target_List_Offset (Value => 0);
      H.Set_Block_Public_Key_URL_Byte_Count (Value => 0);
      H.Set_Block_Public_Key_URL_Offset (Value => 0);

      H.Set_Block_Creation_Timestamp (Value => Now);
      H.Set_Block_Modification_Timestamp (Value => Now);
      H.Set_Block_Expiry_Timestamp (Value => 16#7FFF_FFFF_FFFF_FFFF#);

--      H.Set_Reserved_1a (For_Block_Header_Byte_Count => H.Get_Block_Header_Byte_Count,
--                         Value                       => 0);
--      H.Set_Reserved_1b (For_Block_Header_Byte_Count => H.Get_Block_Header_Byte_Count,
--                         Value                       => 0);
      H.Update_Block_Header_Crc16;

      return H;

   end Factory;


end BRBON.Block.Header.Single_Item_File;

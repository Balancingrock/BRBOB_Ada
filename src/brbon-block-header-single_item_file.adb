package body BRBON.Block.Header.Single_Item_File is


   function Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Instance is
      H: Instance;
      Store: BRBON.Container.Instance := BRBON.Container.Store_Factory (Memory_Area_Ptr, Using_Endianness);
   begin
      H.Store := Store;
      H.Endianness := Using_Endianness;

      H.Set_Synchronization_Byte_1;
      H.Set_Synchronization_Byte_2;
      H.Set_Synchronization_Byte_3;
      H.Set_Synchronization_Byte_4; -- Uses the endianness set in the block header above
      H.Set_Block_Type (Value => BRBON.Block.Single_Item_File);
      H.Set_Block_Options (Value => BRBON.Block.Header.No_Options);

      H.Set_Block_Byte_Count (Value => H.Store.Byte_Count);
      H.Set_Block_Header_Byte_Count (Value => BRBON.Block.Header.Minimum_Byte_Count (BRBON.Block.Single_Item_File)); -- Type 1 does not use the type dependent header or the field storage


      return H;

   end Factory;


end BRBON.Block.Header.Single_Item_File;

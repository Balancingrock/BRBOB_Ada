package body BRBON.Block.Header.Type_1 is


   function Block_Header_Type_1_Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header is
      H: Block_Header;
      Store: BRBON.Container.Store := BRBON.Container.Store_Factory (Memory_Area_Ptr, Using_Endianness);
   begin
      H.Store := Store;
      H.Endianness := Using_Endianness;

      H.Set_Synchronization_Byte_1;
      H.Set_Synchronization_Byte_2;

      return H;
   end Block_Header_Type_1_Factory;





end BRBON.Block.Header.Type_1;

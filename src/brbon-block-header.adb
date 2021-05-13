with BRBON.Block.Header.LowLevel; use BRBON.Block.Header.LowLevel;

package body BRBON.Block.Header is


   function Block_Header_Type_1_Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header is
      H: aliased Block_Header;
      Store: BRBON.Container.Store := BRBON.Container.Store_Factory (Memory_Area_Ptr, Using_Endianness);
   begin
      H.Store := Store;
      H.Endianness := Using_Endianness;

      Set_Synchronization_Byte_1 (H'Access);
      Set_Synchronization_Byte_2 (H'Access);

      return H;
   end Block_Header_Type_1_Factory;





end BRBON.Block.Header;

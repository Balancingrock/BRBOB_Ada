with BRBON.Block.Header.LowLevel; use BRBON.Block.Header.LowLevel;


package body BRBON.Block.Header.Shared is

   procedure Set_Synchonization_Bytes (H: Block_Header_Ptr) is
   begin
      Set_Synchronization_Byte_1 (H);
      Set_Synchronization_Byte_2 (H);
      Set_Synchronization_Byte_3 (H);
      Set_Synchronization_Byte_4 (H);
   end Set_Synchonization_Bytes;

end BRBON.Block.Header.Shared;

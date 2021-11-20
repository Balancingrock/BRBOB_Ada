with Interfaces; use Interfaces;

with Ada.Exceptions;

with BRBON.Block.Header;


package body BRBON.Block is


   function Type_Of_Block (I: in out Instance) return Instance_Type is
   begin
      return BRBON.Block.Header.Get_Block_Type (I.Container);
   end Type_Of_Block;


   procedure Write_To_File (I: in out Instance; To_Path: String) is
   begin
      I.Ensure_Block_Consistency;
      I.Container.Write_To_File (To_Path);
   end Write_To_File;


   function Test_Serializer (I: in out Instance) return Serializable.Instance is
   begin
      return Serializable.Create_Without_Copy
        (Use_In_Place => I.Memory_Ptr,
         First        => I.Memory_Ptr.all'First,
         Last         => I.Memory_Ptr.all'Last);
   end Test_Serializer;


   procedure Ensure_Block_Consistency (I: in out Instance) is
   begin
      raise BRBON.Types.Not_Implemented_Yet;
   end Ensure_Block_Consistency;


end BRBON.Block;

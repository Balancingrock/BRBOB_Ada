with Pointer_Access;
with BRBON; use BRBON;
with Item_Manager;


package body Item is

   procedure Increase_Item_Byte_Size (Mgr: in out Item_Manager.Item_Manager; Item_Ptr: Unsigned_8_Ptr; Byte_Count: Unsigned_32) is
   begin
      raise Incomplete_Code;
   end Increase_Item_Byte_Size;

end Item;

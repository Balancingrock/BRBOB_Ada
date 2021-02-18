package body Item_Manager is

   procedure Increase_Storage_Byte_Count (Mgr: Item_Manager_Ptr; Value: Unsigned_32) is
   begin
      raise BRBON.Incomplete_Code;
   end Increase_Storage_Byte_Count;

   function Unused_Storage (Mgr: in Item_Manager_Ptr) return Unsigned_32 is
   begin
      raise BRBON.Incomplete_Code;
      return 0;
   end Unused_Storage;

   function Item_Manager_Factory(
                                 Use_Endianness: in Endianness := Machine_Endianness;
                                 Byte_Count: in Unsigned_32 := 10 * 2**10;
                                 Root_Type: in Item_Type := Br_Dictionary
                                ) return Item_Manager_Ptr is
   begin
      raise BRBON.Incomplete_Code;
      return null;
   end Item_Manager_Factory;

end Item_Manager;

with Portal; use Portal;

package body Item_Manager is


   -- Item_Manager_Factory
   -- --------------------
   --
   function Item_Manager_Factory(
                                 Use_Endianness: in Endianness := Machine_Endianness;
                                 Byte_Count: in Unsigned_32 := 10 * 2**10;
                                 Increment: in Unsigned_32 := 10 * 2**10;
                                 Root_Type: in Item_Type := Br_Dictionary
                                ) return Item_Manager_Ptr is

      Bc: Unsigned_32 := (Byte_Count + 8) and 16#FFFFFFFC#; -- ensure rounding up to nearest 32 bit
      Im: Item_Manager_Ptr;
   begin
      raise BRBON.Incomplete_Code;
      return Im;
   end Item_Manager_Factory;

   -- Initialization
   --
   procedure Initialize (Mgr: in out Item_Manager) is
   begin
      raise BRBON.Incomplete_Code;
   end Initialize;

   -- Finalization
   --
   procedure Finalization (Mgr: in out Item_Manager) is
   begin
      raise BRBON.Incomplete_Code;
   end Finalization;


   -- Increase_Storage_Byte_Count
   -- ---------------------------
   --
   procedure Increase_Storage_Byte_Count (Mgr: in out Item_Manager; Value: Unsigned_32) is
   begin
      raise BRBON.Incomplete_Code;
   end Increase_Storage_Byte_Count;


   -- Unused_Storage
   -- --------------
   --
   function Unused_Storage (Mgr: in Item_Manager) return Unsigned_32 is
   begin
      raise BRBON.Incomplete_Code;
      return 0;
   end Unused_Storage;



end Item_Manager;

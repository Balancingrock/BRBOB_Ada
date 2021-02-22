with Portal; use Portal;

package body Item_Manager is


   -- Item_Manager_Factory
   -- --------------------
   --
   function Allocate_And_Create(
                                Use_Endianness: in Endianness := Machine_Endianness;
                                Byte_Count: in Unsigned_32 := 10 * 2**10;
                                Increment: in Unsigned_32 := 10 * 2**10;
                                Root_Type: in BR_Item_Type := Br_Dictionary
                               ) return Item_Manager_Ptr is

      Bc: Unsigned_32 := (Byte_Count + 8) and 16#FFFFFFFC#; -- ensure rounding up to nearest 32 bit
      Inc: Unsigned_32 := (Increment + 8) and 16#FFFFFFFC#; -- ensure rounding up to nearest 32 bit
      Im: Item_Manager_Ptr;
      St: Storage_Area_Ptr;

   begin

      St := Allocate_And_Create (Byte_Count       => Bc,
                                 Using_Endianness => Use_Endianness);

      Im := new Item_Manager;
      Im.Storage := St;
      Im.Increments := Inc;

      Im.Storage.all.Create (0, Root_Type);

      return Im;

   end Allocate_And_Create;


   -- Finalization
   --
   procedure Finalization (M: in out Item_Manager) is
   begin
      Deallocate (M.Storage);
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

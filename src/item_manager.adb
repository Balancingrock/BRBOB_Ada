with Portal_Package; use Portal_Package;

package body Item_Manager is


   -- Item_Manager_Factory
   -- --------------------
   --
   function Allocate_And_Create(
                                Use_Endianness: in Endianness := Machine_Endianness;
                                Byte_Count: in Unsigned_32 := 10 * 2**10;
                                Increment: in Unsigned_32 := 10 * 2**10;
                                Root_Type: in BR_Item_Type := Br_Dictionary;
                                Element_Type: in BR_Item_Type := BR_Bool
                               ) return Item_Manager_Ptr is

      Bc: Unsigned_32 := Round_Up_To_Nearest_Multiple_Of_8 (Byte_Count); -- ensure alignment
      Mgr: Item_Manager_Ptr;
      Storage_Ptr: Storage_Area_Ptr;
      Item: Item_Access;

   begin

      -- Root type cannot be Null
      --
      if Root_Type = BR_Null then raise BRBON.Illegal_Item_Type; end if;

      -- Ensure that the byte count is sufficient for an empty item without name.
      --
      Bc := Max (Bc, Minimum_Item_Byte_Count (Root_Type));

      -- Allocate the necessary memory.
      --
      Storage_Ptr := Allocate_And_Create (Byte_Count       => Bc,
                                          Using_Endianness => Use_Endianness);

      -- Create the new manager
      --
      Mgr := new Item_Manager;

      -- Initialize it
      --
      Mgr.Storage := Storage_Ptr;
      Mgr.Increments := Round_Up_To_Nearest_Multiple_Of_8 (Increment);

      -- Create an access item
      --
      Item := Create_Item_Access (Storage_Ptr, 0);

      -- Initialize the root item
      --
      case Root_Type is
         when BR_Null => null; -- Null not allowed as root element
         when BR_Bool => Item.Create_BR_Bool (Byte_Count => Bc);
         when BR_Int8 => Item.Create_BR_Int8 (Byte_Count => Bc);
         when BR_Int16 => Item.Create_BR_Int16 (Byte_Count => Bc);
         when BR_Int32 => Item.Create_BR_Int32 (Byte_Count => Bc);
         when BR_Int64 => Item.Create_BR_Int64 (Byte_Count => Bc);
         when BR_UInt8 => Item.Create_BR_UInt8 (Byte_Count => Bc);
         when BR_UInt16 => Item.Create_BR_UInt16 (Byte_Count => Bc);
         when BR_UInt32 => Item.Create_BR_UInt32 (Byte_Count => Bc);
         when BR_UInt64 => Item.Create_BR_UInt64 (Byte_Count => Bc);
         when BR_Float32 => Item.Create_BR_Float32 (Byte_Count => Bc);
         when BR_Float64 => Item.Create_BR_Float64 (Byte_Count => Bc);
         when BR_String => Item.Create_BR_String (Byte_Count => Bc);
         when BR_CRC_String => Item.Create_BR_CRC_String (Byte_Count => Bc);
         when BR_Binary => Item.Create_Binary (Byte_Count => Bc);
         when BR_CRC_Binary => Item.Create_CRC_Binary (Byte_Count => Bc);
         when BR_Array => Item.Create_Array (Byte_Count => Bc, Element_Type => Element_Type);
         when BR_Dictionary => Item.Create_Dictionary (Byte_Count => Bc);
         when BR_Sequence => Item.Create_Sequence (Byte_Count => Bc);
         when BR_Table => Item.Create_Table (Byte_Count => Bc);
         when BR_UUID => Item.Create_UUID (Byte_Count => Bc);
         when BR_Color => Item.Create_Color (Byte_Count => Bc);
         when BR_Font => Item.Create_Font (Byte_Count => Bc);
      end case;

      return Mgr;

   end Allocate_And_Create;


   -- Finalization
   --
   procedure Finalization (Mgr: in out Item_Manager) is
   begin
      Deallocate (Mgr.Storage);
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

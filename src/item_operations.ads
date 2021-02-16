with BRBON; use BRBON;
with BRBON_Basic_Types; use BRBON_Basic_Types;


package Item_Operations is


   -- This exception is raised when a bit pattern in the raw data could not be mapped to a corresponding enum.

   Enum_Mapping_Failed: exception;


   -- Sets the type of an item.
   --
   procedure Set_Item_Type (Item_Ptr: Unsigned_8_Ptr; Value: Item_Type);

   -- Returns the type of the item.
   -- Can raise Enum_Mapping_Failed.
   --
   function Get_Item_Type (Item_Ptr: Unsigned_8_Ptr) return Item_Type;


   -- Sets the options of an item.
   --
   procedure Set_Item_Options (Item_Ptr: Unsigned_8_Ptr; Value: Item_Options);


   -- Returns the options of an item.
   --
   function Get_Item_Options (Item_Ptr: Unsigned_8_Ptr) return Item_Options;


   -- Sets the flags of an item
   --
   procedure Set_Item_Flags (Item_Ptr: Unsigned_8_Ptr; Value: Bits_8);


   -- Returns the flags of an item
   --
   function Get_Item_Flags (Item_Ptr: Unsigned_8_Ptr) return Bits_8;


end Item_Operations;

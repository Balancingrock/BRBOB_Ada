with BRBON; use BRBON;
with BRBON_Basic_Types; use BRBON_Basic_Types;
with Item_Manager;


package Item is


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


   -- Increase the size for the given item.
   -- If necessary the parent items will be increased in length as well and the storage area if needed and possible.
   -- @value Mgr The item manager for the item.
   -- @value Item_Ptr The item to increase.
   -- @value: Byte_Count The requested byte count for the item. Note: this cannot be used to make the item smaller and the byte-count will be rounded up to the next 32 bit boundary.
   -- @raise This operation can raise the Storage_Error exception.
   --
   procedure Increase_Item_Byte_Size (Mgr: Item_Manager.Item_Manager_Ptr; Item_Ptr: Unsigned_8_Ptr; Byte_Count: Unsigned_32);

end Item;

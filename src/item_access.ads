with BRBON; use BRBON;
with BRBON_Basic_Types; use BRBON_Basic_Types;
with Item_Manager; use Item_Manager;

package Item_Access is

   -- This exception is raised when a bit pattern in the raw data could not be mapped to a corresponding enum.

   Enum_Mapping_Failed: exception;


   -- Sets the type of an item.
   --
   procedure Set_Item_Type (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Item_Type);

   -- Returns the type of the item.
   -- Can raise Enum_Mapping_Failed.
   --
   function Get_Item_Type (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Item_Type;


   -- Sets the options of an item.
   --
   procedure Set_Item_Options (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Item_Options);


   -- Returns the options of an item.
   --
   function Get_Item_Options (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Item_Options;


   -- Sets the flags of an item
   --
   procedure Set_Item_Flags (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Bits_8);


   -- Returns the flags of an item
   --
   function Get_Item_Flags (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Bits_8;


   -- Sets the name field byte count of an item
   --
   procedure Set_Item_Name_Field_Byte_Count (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_8);


   -- Returns the name field byte count of an item
   --
   function Get_Item_Name_Field_Byte_Count (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Unsigned_8;

end Item_Access;

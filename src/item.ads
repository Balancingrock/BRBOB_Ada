with Ada.Finalization; use Ada.Finalization;
with Interfaces; use Interfaces;

with BRBON; use BRBON;
with Item_Manager; use Item_Manager;



package Item is


   -- The Item type
   --
   type Item is
      record
         Manager: Item_Manager_Ptr;
         Offset: Unsigned_32;
      end record;


   -- Create a new Item Type
   --
   function New_Item (Manager: Item_Manager_ptr; Offset: Unsigned_32) return Item;


   -- Sets the type of an item.
   --
   procedure Set_Item_Type (I: Item; Value: Item_Type);


   -- Returns the type of the item.
   -- Can raise Enum_Mapping_Failed.
   --
   function Get_Item_Type (I: Item) return Item_Type;


   -- Sets the options of an item.
   --
   procedure Set_Item_Options (I: Item; Value: Item_Options);


   -- Returns the options of an item.
   --
   function Get_Item_Options (I: Item) return Item_Options;


   -- Sets the flags of an item
   --
   procedure Set_Item_Flags (I: Item; Value: Bits_8);


   -- Returns the flags of an item
   --
   function Get_Item_Flags (I: Item) return Bits_8;


   -- Sets the name field byte count of an item
   --
   procedure Set_Item_Name_Field_Byte_Count (I: Item; Value: Unsigned_8);


   -- Returns the name field byte count of an item
   --
   function Get_Item_Name_Field_Byte_Count (I: Item) return Unsigned_8;


end Item;

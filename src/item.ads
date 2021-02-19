with Ada.Finalization; use Ada.Finalization;

with Item_Manager; use Item_Manager;
with BRBON_Basic_Types; use BRBON_Basic_Types;
with BRBON; use BRBON;


package Item is

   type Item;

   type Item_Ptr is access Item;

   type Item is new Limited_Controlled with
      record
         Manager: Item_Manager.Item_Manager;
         Offset: Unsigned_32;
      end record;


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

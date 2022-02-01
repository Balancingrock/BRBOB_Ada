with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;

with Item_Package; use Item_Package;

with BRBON; use BRBON;
with Container_Package; use Container_Package;


package Item_Bool_Package is


   -- The item header for a boolean value
   --
   type Item_Bool is
      record
         The_Item_Header: Item_Header;
         Value: Unsigned_8;
         Filler_8: Unsigned_8;
         Filler_16: Unsigned_16;
      end record;

   for Item_Bool use
      record
         The_Item_Header at                          0 range 0 .. (Item_Header_Byte_Count * 8 - 1);
         Value           at Item_Header_Byte_Count * 8 + 0 range 0..7;
         Filler_8        at Item_Header_Byte_Count * 8 + 1 range 0..7;
         Filler_16       at Item_Header_Byte_Count * 8 + 2 range 0..15;
      end record;

   type Item_Bool_Ptr is access Item_Bool;

   function To_Item_Bool_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Bool_Ptr);
   function To_Item_Bool_Ptr is new Ada.Unchecked_Conversion (Item_Header_Ptr, Item_Bool_Ptr);


   -- Return the boolean value
   --
   function Get_Value (C: Container; Item_Offset: Unsigned_32) return Boolean;
   pragma Inline (Get_Value);


   -- Sets the boolean value
   --
   procedure Set_Value (C: Container; Item_Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Set_Value);


   -- This creates a bool item with only the specified values set.
   --
   procedure Create_Item_Bool (C: Container; Item_Offset: Unsigned_32; Parent_Offset: Unsigned_32; Value: Boolean := False; Name: Item_Name_Package.Bounded_String := No_Name);


end Item_Bool_Package;

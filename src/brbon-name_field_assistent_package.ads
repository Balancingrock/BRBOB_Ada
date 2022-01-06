with Interfaces; use Interfaces;
with System;

with Ada.Unchecked_Conversion;

with BRBON; use BRBON;


package BRBON.Name_Field_Assistent_Package is


   -- Use name field assistents if a name must be used mutliple times in a call to a BRBON API.
   -- This will speed up access to the designated item.
   --
   type Name_Field_Assistent is private;


   -- Returns a name field assistent that can be used to speed up access to items.
   --
   -- Note that assistents are tied to a store and should not be used with different stores.
   -- An exception will be raised if used between incompatible stores.
   --
   function Name_Field_Assistent_Factory (Name: String; S: Store) return Name_Field_Assistent;


   function Get_Swap_Status (NFA: Name_Field_Assistent) return Boolean;
   pragma Inline (Get_Swap_Status);

   function Get_Quick_Check_Value (NFA: Name_Field_Assistent) return Unsigned_32;
   pragma Inline (Get_Quick_Check_Value);

private

   type Unsigned_16_Ptr is access all Unsigned_16;

   type Quick_Check_Type is
      record
         CRC: Unsigned_16;
         Count: Unsigned_8;
         Char: Unsigned_8;
      end record;

   for Quick_Check_Type'Size use 32;

   for Quick_Check_Type use
      record
         CRC at 0 range 0..15;
         Count at 2 range 0..7;
         Char at 3 range 0..7;
      end record;

   function Quick_Check_As_Unsigned_32 is new Ada.Unchecked_Conversion (Quick_Check_Type, Unsigned_32);

   type Name_Field_Assistent is
      record
         Quick_Check: Unsigned_32;
         Field_Byte_Count: Unsigned_8;
         Swap: Boolean;
         CRC: Unsigned_16;
         Ascii_Byte_Count: Unsigned_8;
         Ascii_Code: BRBON.Unsigned_8_Array (1 .. 245);
      end record;


   -- Returns the minimum byte count of the item to which this name belongs.
   -- The byte count for the contained value is excluded.
   --
--   function Get_Minimum_Item_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_32;


end BRBON.Name_Field_Assistent_Package;

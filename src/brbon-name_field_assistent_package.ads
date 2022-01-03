with Interfaces; use Interfaces;
with System;

with Ada.Unchecked_Conversion;

with BRBON;


package BRBON.Name_Field_Assistent_Package is


   -- Use name field assistents if a name must be used mutliple times in a call to a BRBON API.
   -- This will speed up access to the designated item.
   --
   type Name_Field_Assistent is
      record
         Field_Byte_Count: Unsigned_8;
         Unused: Unsigned_8;
         CRC: aliased Unsigned_16;
         Ascii_Byte_Count: Unsigned_8;
         Ascii_Code: BRBON.Unsigned_8_Array (1 .. 245);
      end record;

   for Name_Field_Assistent'Size use 8 + 8 + 16 + 8 + 245*8;

   for Name_Field_Assistent use
      record
         Field_Byte_Count at 0 range 0..7;
         Unused           at 1 range 0..7;
         CRC              at 2 range 0..15;
         Ascii_Byte_Count at 4 range 0..7;
         Ascii_Code       at 5 range 0..(245*8-1);
      end record;


   -- Returns a name field assistent that can be used to speed up access to items.
   --
   function Name_Field_Assistent_Factory (S: String) return Name_Field_Assistent;


private

   type Unsigned_16_Ptr is access Unsigned_16;

   type Quick_Check_Ptr is access Unsigned_32;

   function To_Quick_Check_Ptr is new Ada.Unchecked_Conversion (Unsigned_16_Ptr, Quick_Check_Ptr);


   -- Returns a 32 bit binary value that is used to quickly falsify a name check
   --
   function Get_Quick_Check_Value (NFA: Name_Field_Assistent) return Unsigned_32;


   -- Returns the minimum byte count of the item to which this name belongs.
   -- The byte count for the contained value is excluded.
   --
   function Get_Minimum_Item_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_32;


end BRBON.Name_Field_Assistent_Package;

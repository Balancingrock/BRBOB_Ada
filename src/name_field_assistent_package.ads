with Interfaces; use Interfaces;
with System;

with Ada.Unchecked_Conversion;

with CRC_Package; use CRC_Package;
with Item_Package; use Item_Package;

with BRBON; use BRBON;
with Container_Package; use Container_Package;


package Name_Field_Assistent_Package is


   -- The quick check value is used to prevent lengthy character comparisons when a name does not match.
   --
   subtype Quick_Check_Type is Unsigned_32;

   type Quick_Check_Ptr is access Quick_Check_Type;

   function To_Quick_Check_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Quick_Check_Ptr);


   -- Internally in BRBON all name searches/lookups are made using a name field assistent.
   -- When the API user creates a name field assistent this obliviates the need to create a new
   -- name field assistent every time a certain name is used.
   --
   type Name_Field_Assistent is
      record
         Quick_Check: Quick_Check_Type;
         Field_Byte_Count: Unsigned_8;
         Swap: Boolean;
         CRC: aliased CRC_Package.CRC_16;
         Name_Byte_Count: Unsigned_8;
         Name: Item_Name_Package.Bounded_String;
      end record;


   -- Returns a name field assistent that can be used to speed up access to items.
   --
   -- Note that assistents are byte-order sensitive and can only be used on blocks with the same byte-order.
   -- An exception will be raised when the block is not compatible with the assistent.
   --
   function Name_Field_Assistent_Factory (C: Container; Name: Item_Name_Package.Bounded_String) return Name_Field_Assistent;


   -- Returns the swap status for this assistent (must be equal to the swap status of the block)
   --
   function Swap_Status (N: Name_Field_Assistent) return Boolean;
   pragma Inline (Swap_Status);


   -- Compares the quick check value to the name field assistent
   --
   function Quick_Check_Passes (N: Name_Field_Assistent; Ptr: Quick_Check_Ptr) return Boolean;


   -- Compares the name to the name in the name field assistent
   --
   function Name_Check_Passes (N: Name_Field_Assistent; Name: Item_Name_Package.Bounded_String) return Boolean;


end Name_Field_Assistent_Package;

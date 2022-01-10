with Interfaces; use Interfaces;
with System;

with Ada.Unchecked_Conversion;

with CRC_Package; use CRC_Package;

with BRBON; use BRBON;
with BRBON.Container_Package; use BRBON.Container_Package;


package BRBON.Name_Field_Assistent_Package is


   -- Internally in BRBON all name searches/lookups are made using a name field assistent.
   -- When the API user creates a name field assistent this obliviates the need to create a new
   -- name field assistent every time a certain name is used.
   --
   type Name_Field_Assistent is private;


   -- Returns a name field assistent that can be used to speed up access to items.
   --
   -- Note that assistents are byte-order sensitive and can only be used on blocks with the same byte-order.
   -- An exception will be raised when the block is not compatible with the assistent.
   --
   function Name_Field_Assistent_Factory (Name: String; C: Container) return Name_Field_Assistent;


   -- Returns the swap status for this assistent (must be equal to the swap status of the block)
   --
   function Swap_Status (N: Name_Field_Assistent) return Boolean;
   pragma Inline (Swap_Status);


   -- Compares the quick check value to the name field assistent
   --
   function Quick_Check_Passes (N: Name_Field_Assistent; Ptr: Quick_Check_Value_Ptr) return Boolean;


   -- Compares the name to the name in the name field assistent
   --
   function Name_Check_Passes (N: Name_Field_Assistent; Name: Item_Name) return Boolean;


private

   type Name_Field_Assistent is
      record
         Quick_Check: Quick_Check_Value;
         Field_Byte_Count: Unsigned_8;
         Swap: Boolean;
         CRC: Unsigned_16;
         Name_Byte_Count: Unsigned_8;
         Name: Item_Name;
      end record;

end BRBON.Name_Field_Assistent_Package;

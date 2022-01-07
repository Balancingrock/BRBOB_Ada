with Interfaces; use Interfaces;
with System;

with Ada.Unchecked_Conversion;

with CRC_Package; use CRC_Package;

with BRBON; use BRBON;



private package BRBON.Name_Field_Assistent_Package is

   subtype Quick_Check_Value is Unsigned_32;

   type Quick_Check_Value_Ptr is access Quick_Check_Value;

   function Swap_Status (NFA: Name_Field_Assistent) return Boolean;
   pragma Inline (Swap_Status);

   function Quick_Check (NFA: Name_Field_Assistent) return Quick_Check_Value;
   pragma Inline (Quick_Check);

   function CRC (NFA: Name_Field_Assistent) return CRC_16;

   function Name_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_8;

   function Field_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_8;

   function Name (NFA: Name_Field_Assistent) return String;

   function Compare_Quick_Check (NFA: Name_Field_Assistent; Ptr: Quick_Check_Value_Ptr) return Boolean;

   function Compare_String (NFA: Name_Field_Assistent; Ptr: BRBON.Unsigned_8_Ptr; Byte_Count: Unsigned_8) return Boolean;

   function To_Quick_Check_Value is new Ada.Unchecked_Conversion (Quick_Check_Value, Unsigned_32);

end BRBON.Name_Field_Assistent_Package;

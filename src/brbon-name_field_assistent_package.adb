with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with Crc_Package;

with BRBON.Utils;



package body BRBON.Name_Field_Assistent_Package is


   -----------------------------------------------------------------------------

   function Swap_Status (NFA: Name_Field_Assistent) return Boolean is (NFA.Swap);


   -----------------------------------------------------------------------------

   function Quick_Check_Value (NFA: Name_Field_Assistent) return Quick_Check_Value is (NFA.Quick_Check);


   -----------------------------------------------------------------------------

   function CRC (NFA: Name_Field_Assistent) return CRC_16 is (NFA.CRC);


   -----------------------------------------------------------------------------

   function Name_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_8 is (NFA.Name_Byte_Count);


   -----------------------------------------------------------------------------

   function Field_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_8 is (NFA.Field_Byte_Count);


   -----------------------------------------------------------------------------

   function Name (NFA: Name_Field_Assistent) return String is (NFA.Name);


   -----------------------------------------------------------------------------

   function Compare_Quick_Check (NFA: Name_Field_Assistent; Ptr: Quick_Check_Value_Ptr) return Boolean is
   begin
      return Ptr.all = NFA.Quick_Check;
   end Compare_Quick_Check;


   -----------------------------------------------------------------------------

   function Compare_String (NFA: Name_Field_Assistent; Ptr: Unsigned_8_Ptr; Byte_Count: Unsigned_8) return Boolean is
      type Arr is new Unsigned_8_Array (1 .. Byte_Count);
      type Arr_Ptr is access Arr;
      function To_Arr_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Arr_Ptr);
   begin
      if Byte_Count /= NFA.Ascii_Byte_Count then
         return false;
      else
         return To_Arr_Ptr (Ptr).all = NFA.Ascii_Code;
      end if;
   end Compare_String;


end BRBON.Name_Field_Assistent_Package;

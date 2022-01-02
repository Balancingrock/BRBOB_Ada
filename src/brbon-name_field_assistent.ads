with Interfaces; use Interfaces;

with System;

with BRBON;


package BRBON.Name_Field_Assistent is

   Tag_Length: constant := Standard'Address_Size / System.Storage_Unit;

   type Instance is
      record
         CRC: Unsigned_16;
         Name_Field_Byte_Count: Unsigned_8;
         Ascii_Code: BRBON.Unsigned_8_Array (1 .. 245);
         Ascii_Byte_Count: Unsigned_8;
      end record;

   for Instance'Size use Standard'Address_Size + 16 + 8 + 245*8 + 8;

   for Instance use
      record
         CRC                   at Tag_Length + 0 range 0..15;
         Name_Field_Byte_Count at Tag_Length + 2 range 0..7;
         Ascii_Code            at Tag_Length + 3 range 0..(245*8-1);
         Ascii_Byte_Count      at Tag_Length + 248 range 0..7;
      end record;

   function Create_Name_Field_Assistent (S: String) return Instance;

   function Get_Quick_Check_Value (I: in out Instance) return Unsigned_32;

   function Get_Minimum_Item_Byte_Count (I: Instance) return Unsigned_32;

end BRBON.Name_Field_Assistent;

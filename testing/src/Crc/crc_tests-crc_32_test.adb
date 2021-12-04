with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with CRC_Package;

with Serializable;


separate (CRC_Tests)


function CRC_32_Test (Count: in out Integer) return Test_Result is


   package Hex_IO is new Modular_IO (Unsigned_32);
   use Hex_IO;

   Str_Src: String := "localhost";
   Arr_Src: Array_Of_Unsigned_8 := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

   Str_Exp: Unsigned_32 := 16#8734E372#; -- Note: online crc32 calculators often use initial value 0 which yields: 16#9EC20823#
   Arr_Exp: Unsigned_32 := 16#3955C0F2#; -- Note: online crc32 calculators often use initial value 0 which yields: 16#2520577B#

   Crc: Unsigned_32;
   Fnd: String (1..13);
   Exp: String (1..13);

begin

   Hex_IO.Default_Width := 13;
   Hex_IO.Default_Base := 16;

   Crc := CRC_Package.Calculate_CRC_32 (Str_Src);
   if Crc /= Str_Exp then
      Hex_IO.Put (Fnd, Crc);
      Hex_IO.Put (Exp, Str_Exp);
      New_line (2);
      Put_Line ("Wrong CRC-32 value calculated over string, found:" & Fnd & ", expected:" & Exp);
      --return Failed;
   end if;

   Crc := CRC_Package.Calculate_CRC_32 (Arr_Src);
   if Crc /= Arr_Exp then
      Hex_IO.Put (Fnd, Crc);
      Hex_IO.Put (Exp, Arr_Exp);
      New_line (2);
      Put_Line ("Wrong CRC-32 value calculated over array, found:" & Fnd & ", expected:" & Exp);
      return Failed;
   end if;

   return Passed;

end CRC_32_Test;

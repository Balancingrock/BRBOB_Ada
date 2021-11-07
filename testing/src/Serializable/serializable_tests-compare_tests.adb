with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;

with BRBON.Utils; use BRBON.Utils;

separate (Serializable_Tests)



function Compare_Tests (Count: in out Integer) return Test_Result is

   procedure Put_Hex_Tests is
   begin

      New_line (2);

      BRBON.Utils.Put_Hex_32 (16#0234_5678#); New_Line;
      BRBON.Utils.Put_Hex_32 (16#9654_5678#); New_Line;
      BRBON.Utils.Put_Hex_32 (16#1#); New_Line;

      BRBON.Utils.Put_Hex_8 (16#68#); New_Line;
      BRBON.Utils.Put_Hex_8 (16#ED#); New_Line;
      BRBON.Utils.Put_Hex_8 (16#01#); New_Line;

      declare
         Arr: Array_Of_Unsigned_8 := (0, 1, 2, 3, 4, 5);
      begin
         New_Line;
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 0); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 3); New_Line;
      end;

      declare
         Arr: Array_Of_Unsigned_8 := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
      begin
         New_line;
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 12); New_Line;
      end;

      declare
         Arr: Array_Of_Unsigned_8 := ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                                      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                                      32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
                                      48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63);
      begin
         New_Line;
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 7+16); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 8+32); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 15); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Around => 63); New_Line;
      end;

   end Put_Hex_Tests;


begin

   -- First perform the hex output tetst, these test must be examined by visually inspecting the output on the display
   --
   Put_Hex_Tests;


   return Failed;

exception

   when Test_Failed =>

      return Failed;

end Compare_Tests;

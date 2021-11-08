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
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 0, Show_Cursor => True); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 3, Show_Cursor => True); New_Line;
      end;

      declare
         Arr: Array_Of_Unsigned_8 := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);
      begin
         New_line;
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 12, Show_Cursor => True); New_Line;
      end;

      declare
         Arr: Array_Of_Unsigned_8 := ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                                      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                                      32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
                                      48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63);
      begin
         New_Line;
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 7+16, Show_Cursor => True); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 8+32, Show_Cursor => True); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 15, Show_Cursor => True); New_Line (2);
         BRBON.Utils.Put_Hex_8_Two_Lines (Source => Arr, Cursor => 63, Show_Cursor => True); New_Line;
      end;

      declare
         Str: String := "Earth";
         Ser: Serializable.Instance := Serializable.New_Instance (Str);
         B: Unsigned_8;
         T: Boolean;
      begin
         New_Line;
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Hex_Dump_With_Cursor; New_Line (2);
      end;

   end Put_Hex_Tests;


begin

   -- First perform the hex output tetst, these test must be examined by visually inspecting the output on the display
   --
   -- Put_Hex_Tests;


   declare
      S1: Serializable.Instance := Serializable.New_Instance ("12345");
      S2: Serializable.Instance := Serializable.New_Instance ("12345");
      S3: Serializable.Instance := Serializable.New_Instance ("12346");
      S4: Serializable.Instance := Serializable.New_Instance ("123");
   begin
      if not S1.Compare (S2) then
         New_Line (2);
         Put_Line ("Two equal serializable instances compare as not equal");
         raise Test_Failed;
      end if;
      if S2.Compare (S3) then
         New_Line (2);
         Put_Line ("Two unequal serializable instances compare as equal");
         raise Test_Failed;
      end if;
      if S3.Compare (S4) then
         New_Line (2);
         Put_Line ("Two unequal length serializable instances compare as equal");
         raise Test_Failed;
      end if;
   end;


   return Passed;

exception

   when Test_Failed =>

      return Failed;

end Compare_Tests;

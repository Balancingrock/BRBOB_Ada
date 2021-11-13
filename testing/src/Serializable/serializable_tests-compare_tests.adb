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
         Ser: Serializable.Instance := Serializable.Create_With_Copy (Str);
         B: Unsigned_8;
         T: Boolean;
      begin
         New_Line;
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
         T := Ser.Copy_Next_Byte (B);
         Ser.Dump_2_Lines; New_Line (2);
      end;

   end Put_Hex_Tests;


begin

   -- First perform the hex output tetst, these test must be examined by visually inspecting the output on the display
   --
   -- Put_Hex_Tests;


   declare
      S1: Serializable.Instance := Serializable.Create_With_Copy ("12345");
      S1E: Array_Of_Unsigned_8 := (16#31#, 16#32#, 16#33#, 16#34#, 16#35#);
      S2: Serializable.Instance := Serializable.Create_With_Copy ("12345");
      S2E: Array_Of_Unsigned_8 := (16#31#, 16#32#, 16#33#, 16#34#, 16#36#);
      S3: Serializable.Instance := Serializable.Create_With_Copy ("12346");
      S3E: Array_Of_Unsigned_8 := (16#31#, 16#32#, 16#33#);
   begin
      if not S1.Compare (S1E) then
         New_Line (2);
         Put_Line ("Two equal serializable instances compare as not equal");
         raise Test_Failed;
      end if;
      if S2.Compare (S2E) then
         New_Line (2);
         Put_Line ("Two unequal serializable instances compare as equal");
         raise Test_Failed;
      end if;
      if S3.Compare (S3E) then
         New_Line (2);
         Put_Line ("Two unequal length serializable instances compare as equal");
         raise Test_Failed;
      end if;
   end;

   declare
      Arr1: Array_Of_Unsigned_8 :=
        ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
          16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
          32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
          48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63);
      Arr1_Ptr: Array_Of_Unsigned_8_Ptr;
      S1: Serializable.Instance;
      Arr2: Array_Of_Unsigned_8 :=
        ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
          16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
          32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
          48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63);
      Dcar: BRBON.Types.Array_Of_Boolean :=
        ( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
          false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
          false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false,
          false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
   begin
      Arr1_Ptr := new Array_Of_Unsigned_8 (Arr1'First .. Arr1'Last);
      Arr1_Ptr.all := Arr1;
      S1 := Serializable.Create_Without_Copy (Arr1_Ptr, Arr1'First, Arr1'Last);
      if not S1.Compare (Arr2) then
         New_Line (2);
         Put_Line ("Expected equality for s1 & s2 when not using Dont_Care");
         raise Test_Failed;
      end if;
      S1 := Serializable.Create_Without_Copy (Arr1_Ptr, Arr1'First, Arr1'Last);
      if not S1.Compare (Arr2, Dcar) then
         New_Line (2);
         Put_Line ("Expected equality for s1 & s2 when using Dont_Care");
         raise Test_Failed;
      end if;
      S1 := Serializable.Create_Without_Copy (Arr1_Ptr, Arr1'First, Arr1'Last);
      Arr2 (15) := 0;
      if S1.Compare (Arr2, Dcar) then
         New_Line (2);
         Put_Line ("Expected unequality for index 16");
         raise Test_Failed;
      else
         if S1.Remaining_Bytes /= 48 then
            New_Line (2);
            Put_Line ("Expected 48 bytes to remain after compare, found" & S1.Remaining_Bytes'Image);
            raise Test_Failed;
         end if;
      end if;
      S1 := Serializable.Create_Without_Copy (Arr1_Ptr, Arr1'First, Arr1'Last);
      Dcar (15):= true;
      if not S1.Compare (Arr2, Dcar) then
         New_Line (2);
         Put_Line ("Expected equality with ignored byte 16");
         raise Test_Failed;
      end if;
   end;

   return Passed;

exception

   when Test_Failed =>

      return Failed;

end Compare_Tests;

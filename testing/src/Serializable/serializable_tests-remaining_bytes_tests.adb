with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function Remaining_Bytes_Tests (Count: in out Integer) return Test_Result is


   Source: Array_Of_Unsigned_8 (0 .. 7) := (0, 1, 2, 3, 4, 5, 6, 7);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;
   Index: Integer := 8;

begin

   Destination := Serializable.Create_With_Copy (Copy_Bytes_From => Source);

   if Destination.Remaining_Bytes /= Index then
      New_Line (2);
      Put_Line ("1: Expected " & Index'Image & " bytes to remain, found: " & Destination.Remaining_Bytes'Image);
      raise Test_Failed;
   end if;

   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      Index := Index - 1;
      if Destination.Remaining_Bytes /= Index then
         New_Line (2);
         Put_Line ("2: Expected " & Index'Image & " bytes to remain, found: " & Destination.Remaining_Bytes'Image);
         raise Test_Failed;
      end if;
   end loop;

   if Destination.Remaining_Bytes /= 0 then
      New_Line (2);
      Put_Line ("3: Expected 0 bytes to remain, found: " & Destination.Remaining_Bytes'Image);
      raise Test_Failed;
   end if;

   return Passed;

exception

   when Test_Failed =>

      return Failed;

end Remaining_Bytes_Tests;

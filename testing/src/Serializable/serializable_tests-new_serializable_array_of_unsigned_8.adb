with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function New_Serializable_Array_Of_Unsigned_8 (Count: in out Integer) return Test_Result is


   Source: Array_Of_Unsigned_8 (0 .. 7) := (0, 1, 2, 3, 4, 5, 6, 7);
   Expected: Array_Of_Unsigned_8 := Source;
   Destination: Serializable.Instance;
   Byte: Unsigned_8;
   Index: Unsigned_32 := 0;

begin

   Destination := Serializable.New_Instance (Copy_Bytes_From => Source);

   Source := (others => 9);

   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      if Byte /= Expected (Index) then
         Put_Line ("Byte at position: " & Index'Image & ", found: " & Byte'Image & ", expected: " & Expected (Index)'Image);
         raise Test_Failed;
      else
         Index := Index + 1;
      end if;
   end loop;

   return Passed;

exception

   when Test_Failed =>

      return Failed;

end New_Serializable_Array_Of_Unsigned_8;

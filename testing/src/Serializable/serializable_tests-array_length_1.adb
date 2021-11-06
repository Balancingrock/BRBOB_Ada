with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function Array_Length_1 (Count: in out Integer) return Test_Result is


   Source: Array_Of_Unsigned_8 (0 .. 0);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;
   Index: Unsigned_32 := 0;

begin

   Source := (Source'First => 23);

   Destination := Serializable.New_Instance (Copy_Bytes_From => Source);


   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      if Byte /= Source (Index) then
         Put_Line ("Byte at position: " & Index'Image & ", found: " & Byte'Image & ", expected: " & Source (Index)'Image);
         raise Test_Failed;
      else
         Index := Index + 1;
      end if;
   end loop;

   return Passed;

exception

   when Test_Failed =>

      return Failed;

end Array_Length_1;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function New_Serializable_Array_Of_Unsigned_8 (Count: in out Integer) return Test_Result is


   Source: Array_Of_Unsigned_8 (0 .. 7) := (0, 1, 2, 3, 4, 5, 6, 7);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;
   Index: Unsigned_32 := 0;

begin

   Destination := Serializable.New_Instance (Copy_Bytes_From => Source);


   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      if Byte /= Source (Index) then
         Put ("Byte at position: "); Put (Index'Image); Put (", found: "); Put (Byte'Image); Put (", expected: "); Put (Source (Index)'Image);
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

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function New_Serializable_String (Count: in out Integer) return Test_Result is

   Source: String := "1234567";
   Expected: Array_Of_Unsigned_8 := (16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;
   Index: Unsigned_32 := 0;

begin

   Destination := Serializable.Create_With_Copy (Copy_Bytes_From => Source);

   Source := (others => 'A');

   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      if Byte /= Expected (Index) then
         New_Line (2);
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

end New_Serializable_String;

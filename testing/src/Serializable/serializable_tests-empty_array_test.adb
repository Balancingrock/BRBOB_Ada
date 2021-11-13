with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function Empty_Array_Test (Count: in out Integer) return Test_Result is


   Source: Array_Of_Unsigned_8 (1 .. 0);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;

begin

   Destination := Serializable.Create_With_Copy (Copy_Bytes_From => Source);


   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      Put_Line ("Serializable instance should have been empty");
      raise Test_Failed;
   end loop;

   return Passed;

exception

   when Test_Failed =>

      return Failed;

end Empty_Array_Test;

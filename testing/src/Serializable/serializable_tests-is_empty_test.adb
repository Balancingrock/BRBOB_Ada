with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function Is_Empty_Test (Count: in out Integer) return Test_Result is

   Source: Array_Of_Unsigned_8 (0 .. 7) := (0, 1, 2, 3, 4, 5, 6, 7);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;

begin

   Destination := Serializable.New_Instance (Copy_Bytes_From => Source);

   if Destination.Is_Empty then
      Put_Line ("The serializable instance should not have been empty");
      raise Test_Failed;
   end if;

   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      null;
   end loop;

   if not Destination.Is_Empty then
      Put_Line ("The serializable instance should have been empty");
      raise Test_Failed;
   end if;

   return Passed;

exception

   when Test_Failed =>

      return Failed;

end Is_Empty_Test;

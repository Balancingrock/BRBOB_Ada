with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.text_IO;

with Serializable;


separate (Serializable_Tests)


function Use_In_Place_Test (Count: in out Integer) return Test_Result is

   Source: Array_Of_Unsigned_8_Ptr;
   Expected: Array_Of_Unsigned_8 (0 .. 7);
   Destination: Serializable.Instance;
   Byte: Unsigned_8;
   Index: Unsigned_32 := 0;

begin

   Source := new Array_Of_Unsigned_8 (0 .. 7);
   Source.all := (0, 1, 2, 3, 4, 5, 6, 7);
   Expected := Source.all;

   Destination := Serializable.Create_Without_Copy (Use_In_Place => Source,
                                                    First        => Source.all'First,
                                                    Last         => Source.all'Last);

   Source.all (5) := 9;
   Expected (5) := 9;

   while Serializable.Copy_Next_Byte (Destination, Byte) loop
      if Byte /= Expected (Index) then
         Put_Line ("Byte at position: " & Index'Image & ", found: " & Byte'Image & ", expected: " & Expected (Index)'Image);
         raise Test_Failed;
      else
         Index := Index + 1;
      end if;
   end loop;

   BRBON.Types.Deallocate_Array_Of_Unsigned_8 (Source);

   return Passed;

exception

   when Test_Failed =>

      BRBON.Types.Deallocate_Array_Of_Unsigned_8 (Source);

      return Failed;

   when others =>

      BRBON.Types.Deallocate_Array_Of_Unsigned_8 (Source);

      raise;

end Use_In_Place_Test;

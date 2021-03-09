with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Ada.Calendar; use Ada.Calendar;


package body Test_Driver is

   function Run (Tests: in out Array_Of_Tests; Count: in out Integer) return Test_Result is

      Result: Test_Result;

      Start_Time: Time;
      End_Time: Time;

   begin

      for Test of Tests loop

         Put (Integer'Image (Count) & "   ");
         Put (Test.Desc);

         Start_Time := Clock;

         Result := Test.Func (Count);

         End_Time := Clock;

         Count := Count + 1;

         if Result = Passed then
            Put_Line (" - Passed in" & Duration'Image((End_Time - Start_Time) * 1000) & " mS");
         end if;

         exit when Result /= Passed;

      end loop;

      return Result;

   end Run;

end Test_Driver;

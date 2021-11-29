with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;

with Types; use Types;
with Test_Driver; use Test_Driver;

with Container_Tests; use Container_Tests;
with Single_Item_Tests; use Single_Item_Tests;
with Serializable_Tests; use Serializable_Tests;


procedure Main is

   type Test_Driver_Info is
      record
         Desc: Unbounded_String;
         Tests: Array_Of_Tests_Ptr;
      end record;

   type Array_Of_Test_Drivers is array (Integer range <>) of Test_Driver_Info;

   Result: Test_Result;


   -- Add new test tables to this table.
   --
   Test_Drivers: Array_Of_Test_Drivers (1..3) :=
     (
        (UStr ("Serializable tests:"), Serializable_Tests.Tests'Access),
        (UStr ("Container tests:"), Container_Tests.Tests'Access),
        (UStr ("Single Item tests:"), Single_Item_Tests.Tests'Access)
     );

   Count: Integer := 1;


   -- Elapsed time measurement
   --
   Start_Time: Time;
   End_Time: Time;
   Dur: Duration;
   DurStr: Unbounded_String;

begin

   Start_Time := Clock;

   for Driver of Test_Drivers loop
      New_Line (2);
      Put_Line ("Running " & Driver.Desc);
      New_Line;
      Result := Run (Driver.Tests.all, Count);
      exit when Result /= Passed;
   end loop;

   End_Time := Clock;

   Dur := (End_Time - Start_Time) * 1000;
   DurStr := To_Unbounded_String (Duration'Image (Dur));
   Delete (DurStr, (Length (DurStr) - 5), Length (DurStr)); -- Remove excess zero's

   if Result = Passed then
      New_Line (2);
      Put_Line ("All Tests Passed in" & DurStr & " mS");
   else
      New_Line (2);
      Put_Line ("Last test failed");
   end if;



   New_Line;

exception

   when E: others =>

      New_Line (2);
      Put_Line ("Exception occured: " & Ada.Exceptions.Exception_Message(E));
      New_Line (2);
      Put_Line ("Testing Failed");
      New_Line;

end Main;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Types; use Types;
with Test_Driver; use Test_Driver;

with Container_Tests; use Container_Tests;
with Single_Item_File_Tests; use Single_Item_File_Tests;
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
        (UStr ("Single_Item_File tests:"), Single_Item_File_Tests.Tests'Access)
     );

   Count: Integer := 1;

begin

   for Driver of Test_Drivers loop
      New_Line (2);
      Put_Line ("Running " & Driver.Desc);
      New_Line;
      Result := Run (Driver.Tests.all, Count);
      exit when Result /= Passed;
   end loop;

   if Result = Passed then
      New_Line (2);
      Put_Line ("All Tests Passed");
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

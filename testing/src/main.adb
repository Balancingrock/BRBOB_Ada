with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Types; use Types;
with Test_Driver; use Test_Driver;

with Container_Tests; use Container_Tests;
with Single_Item_File_Tests; use Single_Item_File_Tests;


procedure Main is

   type Test_Driver_Info is
      record
         Desc: Unbounded_String;
         Tests: Array_Of_Tests_Ptr;
      end record;

   type Array_Of_Test_Drivers is array (Integer range <>) of Test_Driver_Info;

   Result: Test_Result;

   Test_Drivers: Array_Of_Test_Drivers (1..2) :=
     (
        (UStr ("Running Container tests:"), Container_Tests.Tests'Access),
        (UStr ("Running Single_Item_File tests:"), Single_Item_File_Tests.Tests'Access)
     );

   Count: Integer := 1;

begin

   New_Line;

   for Driver of Test_Drivers loop
      Put_Line (Driver.Desc);
      Result := Run (Driver.Tests.all, Count);
      exit when Result /= Passed;
   end loop;

   if Result = Passed then
      Put_Line ("All Tests Passed");
   else
      Put_Line ("Failed");
   end if;

   New_Line;

end Main;

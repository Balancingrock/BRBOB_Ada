with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Types; use Types;

with Container_Tests; use Container_Tests;


procedure Main is

   Result: Test_Result;

   Tests: Array_Of_Tests (1..1) :=
     (
      others => (UStr ("Running Container tests"), Container_Tests.Run'Access)
     );

begin

   for Test of Tests loop
      Put_Line (Test.Desc);
      Result := Test.Func.all;
      exit when Result /= Passed;
   end loop;

   if Result = Passed then
      Put_Line ("Passed");
   else
      Put_Line ("Failed");
   end if;

end Main;

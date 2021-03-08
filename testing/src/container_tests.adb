with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;



package body Container_Tests is

   function Run return Test_Result is
      Result: Test_Result;
   begin
      for Test of Tests loop
         Put ("   ");
         Put_Line (Test.Desc);
         Result := Test.Func.all;
         exit when Result /= Passed;
      end loop;
      return Result;
   end Run;

   function Size_0 return Test_Result is separate;
   function Size_21 return Test_Result is separate;
   function Size_81 return Test_Result is separate;
   function Size_1000 return Test_Result is separate;
   function Bool_Access return Test_Result is separate;

end Container_Tests;

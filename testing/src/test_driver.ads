with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Types; use Types;


package Test_Driver is

   type Test_Ptr is access function (Count: in out Integer) return Test_Result;

   type Test_Info is tagged
      record
         Desc: Unbounded_String;
         Func: Test_Ptr;
      end record;

   type Array_Of_Tests is array (Integer range <>) of Test_Info;

   type Array_Of_Tests_Ptr is access all Array_Of_Tests;

   function Run (Tests: in out Array_Of_Tests; Count: in out Integer) return Test_Result;

end Test_Driver;

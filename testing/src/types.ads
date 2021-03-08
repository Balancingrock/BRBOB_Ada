with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Types is

   type Test_Result is (Passed, Failed);

   type Test_Ptr is access function return Test_Result;

   type Test_Info is
      record
         Desc: Unbounded_String;
         Func: Test_Ptr;
      end record;

   type Array_Of_Tests is array (integer range <>) of Test_Info;

   function UStr (Str: String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

   Test_Files_Root: String := "/home/rien/Projects/Software/Ada/BTest/tmp";

end Types;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;

package Types is

   type Test_Result is (Passed, Failed);

   function UStr (Str: String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

   Test_Files_Root: String := Current_Directory & "/files/";

   Break_Before: Integer := 0;

   Test_Failed: Exception;

end Types;

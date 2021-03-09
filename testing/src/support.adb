with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with BRBON.Container; use BRBON.Container;


package body Support is

   function Put_As_Line (Arr: in Array_Of_Unsigned_8) return String is

      package Hex_IO is new Modular_IO (Unsigned_8);
      use Hex_IO;

      Result: Unbounded_String := To_Unbounded_String ("");
      Temp: String := "16#00#";

   begin

      Hex_IO.Default_Width := 7;
      Hex_IO.Default_Base := 16;

      for B of Arr loop
         Hex_IO.Put (Temp, B);
         if B < 16#10# then
            Result := Result & " 16#0" & Temp (5..6);
         else
            Result := Result & " " & Temp;
         end if;
      end loop;

      return To_String (Result);

   end Put_As_Line;


   function Verify_Small_Bytes (Container: in out Storage_Area; Offset: Unsigned_32; Expected: Array_Of_Unsigned_8) return Test_Result is

      Actual: Array_Of_Unsigned_8 (Expected'Range);

   begin

      Container.Test_Support_Get_Bytes (Start => 0, Dest => Actual);

      if Actual /= Expected then
         Put_Line (" - Failed, Expected: " & Put_As_Line (Expected) & ", Found: " & Put_As_Line (Actual));
         return Failed;
      end if;
      return Passed;

   end Verify_Small_Bytes;

end Support;

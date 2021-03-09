with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces; use Interfaces;


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

end Support;

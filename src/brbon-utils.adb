with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;

with BRBON.Types; use BRBON.Types;

package body BRBON.Utils is

   package Hex_64_IO is new Ada.Text_IO.Integer_IO (Integer_64);
   package Hex_16_IO is new Ada.Text_IO.Integer_IO (Integer_16);

   -- Suppress warnings for changes in Time representation
   pragma Warnings (Off);
   function To_Integer_64 is new Ada.Unchecked_Conversion (Ada.Calendar.Time, Integer_64);
   pragma Warnings (On);

   function To_Unsigned_64 is new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);

   Jan_1_1970: constant Integer_64 := To_Integer_64 (
                                                     Ada.Calendar.Time_Of (Year    => 1970,
                                                                           Month   => 1,
                                                                           Day     => 1,
                                                                           Seconds => 0.0)
                                                    );



   function Milli_Sec_Since_Jan_1_1970 return Unsigned_64 is
      Now: constant Integer_64 := To_Integer_64 (Ada.Calendar.Clock);
      Nano_Sec_Since_Jan_1_1970: constant Integer_64 := Now - Jan_1_1970;
      Milli_Sec: constant Unsigned_64 := To_Unsigned_64 (Nano_Sec_Since_Jan_1_1970 / 1_000_000);
   begin
      return Milli_Sec;
   end Milli_Sec_Since_Jan_1_1970;

   procedure Put_Hex_32 (Value: Unsigned_32) is
      Str: String := "############";
      Str2: String := "0x0000_0000";
      I: Integer := Str'Last - 1;
      C: Character;

   begin
      Hex_64_IO.Put (To   => Str,
                     Item => To_Integer_64 (Unsigned_64 (Value)));
      while I > 3 loop
         C := Str (I);
         exit when C = '#';
         case I is
            when 11 => Str2 (11) := C;
            when 10 => Str2 (10) := C;
            when 9 => Str2 (9) := C;
            when 8 => Str2 (8) := C;
            when 7 => Str2 (6) := C;
            when 6 => Str2 (5) := C;
            when 5 => Str2 (4) := C;
            when 4 => Str2 (3) := C;
            when others => null;
         end case;
         I := I - 1;
      end loop;
      Put (Str2);
   end Put_Hex_32;

   procedure Put_Hex_8 (Value: Unsigned_8; Display_Cursor: Boolean := False) is
      Str: String := "######";
      Str2: String := "00";
      I: Integer := Str'Last - 1;
      C: Character;
   begin
      Hex_16_IO.Put (To   => Str,
                     Item => To_Integer_16 (Unsigned_16 (Value)));
      while I > 3 loop
         C := Str (I);
         exit when C = '#';
         case I is
            when 5 => Str2 (2) := C;
            when 4 => Str2 (1) := C;
            when others => null;
         end case;
         I := I - 1;
      end loop;
      Put (Str2);
   end Put_Hex_8;

   Procedure Put_Hex_8_Two_Lines (Source: Array_Of_Unsigned_8; Around: Unsigned_32) is

      Start: Unsigned_32 := Around and 16#FFFF_FFF0#;
      Cursor_Start: constant Character := '(';
      Cursor_End: constant Character := ')';
      Close_Cursor: Boolean;

   begin

      -- Line 1
      Put_Hex_32 (Start); Put (" ");
      Close_Cursor := False;

      for I in Unsigned_32 range 0 .. 15 loop

         exit when Start + I > Source'Last;

         if Start + I = Around then
            Put (Cursor_Start);
            Close_Cursor := True;
         else
            if Close_Cursor then
               Put (Cursor_End);
               Close_Cursor := False;
            else
               Put (" ");
            end if;
         end if;

         Put_Hex_8 (Source (Start + I));

         if I = 7 then
            if Close_Cursor then
               Put (Cursor_End);
               Close_Cursor := False;
            else
               Put (" ");
            end if;
         end if;

      end loop;

      if Close_Cursor then
         Put (Cursor_End);
      end if;


      -- Line 2
      if Start + 16 <= Source'Last then

         New_Line;
         Put_Hex_32 (Start); Put (" ");
         Close_Cursor := False;

         for I in Unsigned_32 range 16 .. 31 loop

            exit when Start + I > Source'Last;

            if Start + I = Around then
               Put (Cursor_Start);
               Close_Cursor := True;
            else
               if Close_Cursor then
                  Put (Cursor_End);
                  Close_Cursor := False;
               else
                  Put (" ");
               end if;
            end if;

            Put_Hex_8 (Source (Start + I));

            if I = 16 + 7 then
               if Close_Cursor then
                  Put (Cursor_End);
                  Close_Cursor := False;
               else
                  Put (" ");
               end if;
            end if;

         end loop;

         -- This will never happen, hence commented out (cursor at end => only 16 bytes output)
         --
         -- if Close_Cursor then
         --    Put (Cursor_End);
         -- end if;

      end if;

   end Put_Hex_8_Two_Lines;

begin

   Hex_64_IO.Default_Width := 12;
   Hex_64_IO.Default_Base := 16;
   Hex_16_IO.Default_Width := 5;
   Hex_16_IO.Default_Base := 16;

end BRBON.Utils;

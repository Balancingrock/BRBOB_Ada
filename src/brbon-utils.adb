with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;

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



   function Milli_Sec_Since_Jan_1_1970 return BRBON.Timestamp is
      Now: constant Integer_64 := To_Integer_64 (Ada.Calendar.Clock);
      Nano_Sec_Since_Jan_1_1970: constant Integer_64 := Now - Jan_1_1970;
      Milli_Sec: constant Timestamp := Timestamp (Nano_Sec_Since_Jan_1_1970 / 1_000_000);
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

   Procedure Put_Hex_8_Two_Lines (Source: Unsigned_8_Array; Cursor: Unsigned_32; Show_Cursor: Boolean := False) is

      Start: Unsigned_32 := Source'First + (Cursor and 16#FFFF_FFF0#);

      Cursor_Start: Character := '(';
      Cursor_End: Character := ')';

      procedure Put_Hex_Line (Offset: in out Unsigned_32) is

         Close_Cursor: Boolean := False;
         Again: Unsigned_32 := Offset;
         
      begin

         -- Line of hex
         
         for I in Unsigned_32 range 0 .. 15 loop

            exit when Offset > Source'Last;

            if Offset = Cursor then
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

            Put_Hex_8 (Source (Offset));

            if I = 7 then
               if Close_Cursor then
                  Put (Cursor_End);
                  Close_Cursor := False;
               else
                  Put (" ");
               end if;
            end if;

            Offset := Offset + 1;
            
         end loop;

         if Close_Cursor then
            Put (Cursor_End & " ");
         else
            Put ("  ");
         end if;

         
         for I in Unsigned_32 range 0 .. 15 loop
            
            exit when Again > Source'Last;
            
            if Source (Again) < 16#20# or else Source (Again) > 16#7F# then
               Put (" .");
            else
               Put (" " & Character'Val (Integer (Source (Again))));
            end if;
            
            if I = 7 then
               Put (" ");
            end if;
            
            Again := Again + 1;
            
         end loop;
         
      end Put_Hex_Line;

   begin

      If not Show_Cursor then
         Cursor_Start := ' ';
         Cursor_End := ' ';
      end if;

      Put_Hex_32 (Start); Put (" ");
      Put_Hex_Line (Start);

      if Start < Source'Last then
         New_Line;
         Put_Hex_32 (Start); Put (" ");
         Put_Hex_Line (Start);
      end if;

   end Put_Hex_8_Two_Lines;

   
   Procedure Put_Hex (Source: Unsigned_8_Array) is

      Start: Unsigned_32 := Source'First;

      procedure Put_Hex_Line (Offset: in out Unsigned_32) is

         Again: Unsigned_32 := Offset;
         
      begin

         -- Line of hex
         
         for I in Unsigned_32 range 0 .. 15 loop

            exit when Offset > Source'Last;

            Put (" ");

            Put_Hex_8 (Source (Offset));

            if I = 7 then
               Put (" ");
            end if;

            Offset := Offset + 1;
            
         end loop;

         Put ("  ");
         
         for I in Unsigned_32 range 0 .. 15 loop
            
            exit when Again > Source'Last;
            
            if Source (Again) < 16#20# or else Source (Again) > 16#7F# then
               Put (" .");
            else
               Put (" " & Character'Val (Integer (Source (Again))));
            end if;
            
            if I = 7 then
               Put (" ");
            end if;
            
            Again := Again + 1;
            
         end loop;
         
      end Put_Hex_Line;

   begin

      while Start < Source'Last loop
         New_Line;
         Put_Hex_32 (Start); Put (" ");
         Put_Hex_Line (Start);
      end loop;

   end Put_Hex;
   
begin

   Hex_64_IO.Default_Width := 12;
   Hex_64_IO.Default_Base := 16;
   Hex_16_IO.Default_Width := 5;
   Hex_16_IO.Default_Base := 16;

end BRBON.Utils;

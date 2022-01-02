with Interfaces; use Interfaces;

with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Conversion;


package body UUID_Package is


   Package Random_Bytes is new Ada.Numerics.Discrete_Random (Unsigned_8);

   Generator: Random_Bytes.Generator;


   function Factory return UUID is
      A_Byte: Unsigned_8;
      Bytes: UUID_Bytes;
   begin
      for I in Unsigned_32 range 1 .. 16 loop
         A_Byte := Random_Bytes.Random (Generator);
         if I = 7 then -- Version 4
            A_Byte := A_Byte and 16#0F#;
            A_Byte := A_Byte or  16#40#;
         end if;
         if I = 9 then -- Variant 1
            A_Byte := A_Byte and 2#0011_1111#;
            A_Byte := A_Byte or  2#1000_0000#;
         end if;
         Bytes (I) := A_Byte;
      end loop;
      return (Bytes => Bytes);
   end Factory;


   function Factory (Bytes: UUID_Bytes) return UUID is
   begin
      return (Bytes => Bytes);
   end Factory;

   -- To convert ASCII to HEX
   --                                                          0  1  2  3  4  5  6  7  8  9  .  .  .  .  .  .  .  A   B   C   D   E   F   .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .   a   b   c   d   e   f
   Ascii_LUT: Array (Character range '0'..'f') of Unsigned_8 := (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);
   --
   function Factory (Str: UUID_String) return UUID is
      Bytes: Unsigned_8_Array (1..16);
      A_Byte: Unsigned_8;
      I: Integer := 0;
      B: Unsigned_32 := 1;
   begin
      for C of Str loop

         case C is
            when '0'..'9' => null;
            when 'A'..'F' => null;
            when 'a'..'f' => null;
            when '-' => null;
            when others => raise Illegal_Character;
         end case;

         I := I + 1;

         case I is
            when 1|3|5|7|10|12|17|22|25|27|29|31|33|35 => A_Byte := Shift_Left (Ascii_LUT (C), 4);
            when 2|4|6|8|11|13|16|18|21|23|26|28|30|32|34|36 => A_Byte := A_Byte or Ascii_LUT (C); Bytes (B) := A_Byte; B := B + 1;
            when 9|14|19|24 => null;
            when 15 => A_Byte := Ascii_LUT (C);
               if A_Byte /= 4 then
                  raise UUID_Version_Or_Variant_Not_Supported;
               else
                  A_Byte := Shift_Left (A_Byte, 4);
               end if;
            when 20 => A_Byte := Ascii_LUT (C);
               if (A_Byte and 2#0000_1100#) /= 2#0000_1000# then
                  raise UUID_Version_Or_Variant_Not_Supported;
               else
                  A_Byte := Shift_Left (A_Byte, 4);
               end if;
            when others => raise UUID_Version_Or_Variant_Not_Supported;
         end case;
      end loop;

      return (Bytes => Bytes);

   end Factory;


   function Nil_UUID return UUID is
   begin
      return Nil_Instance;
   end Nil_UUID;

   function Get_Bytes (I: UUID) return UUID_Bytes is
   begin
      return I.Bytes;
   end Get_Bytes;

   Byte_LUT: Array (Unsigned_8 range 0..15) of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
   --
   function Get_String (I: UUID) return UUID_String is
      Str: UUID_String;
      K: Integer := 1;
   begin
      for B of I.Bytes loop
         declare
            H_Nibble: Unsigned_8 := Shift_Right ((B and 16#F0#), 4);
            L_Nibble: Unsigned_8 := B and 16#0F#;
         begin
            Str (K) := Byte_LUT (H_Nibble);
            K := K + 1;
            Str (K) := Byte_LUT (L_Nibble);
            K := K + 1;
            if (K = 9) or else (K = 14) or else (K = 19) or else (K = 24) then
               Str (K) := '-';
               K := K + 1;
            end if;
         end;
      end loop;
      return Str;
   end Get_String;


   function "=" (lhs, rhs: UUID_Package.UUID) return Boolean is
   begin
      if lhs.Bytes'Length /= rhs.Bytes'Length then return false; end if;
      for I in lhs.Bytes'First .. lhs.Bytes'Last loop
         if lhs.Bytes (I) /= rhs.Bytes (I) then return false; end if;
      end loop;
      return true;
   end "=";

begin

   declare
      Unix_Epoch_Start: Time := Time_Of (1970, 01, 01);
      Now: Time := Ada.Calendar.Clock;
      Seed: Integer := Integer (Now - Unix_Epoch_Start);
   begin

      Random_Bytes.Reset (Gen       => Generator,
                          Initiator => Seed);
   end;

end UUID_Package;

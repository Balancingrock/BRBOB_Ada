with Interfaces; use Interfaces;

with Ada.Calendar;

with BRBON.Types; use BRBON.Types;


package BRBON.Utils is

   function Max (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A > B then A else B);
   function Min (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A < B then A else B);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_32) return Unsigned_32 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#FFFF_FFF8#);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_8) return Unsigned_8 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#F8#);
   function Round_Down_To_Nearest_Multiple_of_32 (A: Unsigned_32) return Unsigned_32 is (A and 16#FFFF_FFE0#);

   -- Returns the number of milli seconds since 1 jan 1970.
   -- This corresponds to a JAVA timestamp
   --
   function Milli_Sec_Since_Jan_1_1970 return Unsigned_64;

   procedure Put_Hex_32 (Value: Unsigned_32);
   procedure Put_Hex_8 (Value: Unsigned_8; Display_Cursor: Boolean := False);
   Procedure Put_Hex_8_Two_Lines (Source: Array_Of_Unsigned_8; Cursor: Unsigned_32; Show_Cursor: Boolean := False);

end BRBON.Utils;

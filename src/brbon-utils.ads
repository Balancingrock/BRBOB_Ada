with Interfaces; use Interfaces;


package BRBON.Utils is

   function Max (A: Unsigned_16; B: Unsigned_16) return Unsigned_16 is (if A > B then A else B);
   pragma Inline (Max);

   function Max (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A > B then A else B);
   pragma Inline (Max);

   function Min (A: Unsigned_16; B: Unsigned_16) return Unsigned_16 is (if A < B then A else B);
   pragma Inline (Min);

   function Min (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A < B then A else B);
   pragma Inline (Min);

   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_8) return Unsigned_8 is (if A rem 8 = 0 then A else (A + 8) and 16#F8#);
   pragma Inline (Round_Up_To_Nearest_Multiple_of_8);

   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_16) return Unsigned_16 is (if A rem 8 = 0 then A else (A + 8) and 16#FFF8#);
   pragma Inline (Round_Up_To_Nearest_Multiple_of_8);

   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_32) return Unsigned_32 is (if A rem 8 = 0 then A else (A + 8) and 16#FFFF_FFF8#);
   pragma Inline (Round_Up_To_Nearest_Multiple_of_8);

   function Round_Down_To_Nearest_Multiple_of_32 (A: Unsigned_32) return Unsigned_32 is (A and 16#FFFF_FFE0#);
   pragma Inline (Round_Up_To_Nearest_Multiple_of_8);


   -- Returns the number of milli seconds since 1 jan 1970.
   -- This corresponds to a JAVA timestamp
   --
   function Milli_Sec_Since_Jan_1_1970 return BRBON.Timestamp;
   pragma Inline (Milli_Sec_Since_Jan_1_1970);


   -- IO operations intended to aid testing/debugging
   --
   procedure Put_Hex_32 (Value: Unsigned_32);
   procedure Put_Hex_8 (Value: Unsigned_8; Display_Cursor: Boolean := False);
   Procedure Put_Hex_8_Two_Lines (Source: Unsigned_8_Array; Cursor: Unsigned_32; Show_Cursor: Boolean := False);
   procedure Put_Hex (Source: Unsigned_8_Array);

end BRBON.Utils;

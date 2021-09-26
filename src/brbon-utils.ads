with Interfaces; use Interfaces;


package BRBON.Utils is

   function Max (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A > B then A else B);
   function Min (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A < B then A else B);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_32) return Unsigned_32 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#FFFFFFF8#);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_8) return Unsigned_8 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#F8#);
   function Round_Down_To_Nearest_Multiple_of_32 (A: Unsigned_32) return Unsigned_32 is (A and 16#FFFFFFE0#);

end BRBON.Utils;

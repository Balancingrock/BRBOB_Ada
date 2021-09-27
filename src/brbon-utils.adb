with Ada.Unchecked_Conversion;

with BRBON.Types; use BRBON.Types;

package body BRBON.Utils is

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

end BRBON.Utils;

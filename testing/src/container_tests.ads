with Types; use Types;
with Test_Driver; use Test_Driver;

package Container_Tests is

   function Size_0 (Count: in out Integer) return Test_Result;
   function Size_21 (Count: in out Integer) return Test_Result;
   function Size_81 (Count: in out Integer) return Test_Result;
   function Size_1000 (Count: in out Integer) return Test_Result;
   function Bool_Access (Count: in out Integer) return Test_Result;
   function UInt8_Access (Count: in out Integer) return Test_Result;

   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("Size_0"), Size_0'Access),
      (UStr ("Size_21"), Size_21'Access),
      (UStr ("Size_81"), Size_81'Access),
      (UStr ("Size_1000"), Size_1000'Access),
      (UStr ("Bool Access"), Bool_Access'Access),
      (UStr ("UInt8_Access"), UInt8_Access'Access)
     );

end Container_Tests;

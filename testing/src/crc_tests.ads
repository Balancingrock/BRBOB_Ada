with Types; use Types;
with Test_Driver; use Test_Driver;

package CRC_Tests is


   function CRC_16_ARC_Test (Count: in out Integer) return Test_Result;
   function CRC_32_Test (Count: in out Integer) return Test_Result;

   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("CRC_16_ARC"), CRC_16_ARC_Test'Access),
      (UStr ("CRC_32"), CRC_32_Test'Access)
     );

end CRC_Tests;

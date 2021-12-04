with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;

with Support; use Support;

package body CRC_Tests is

   function CRC_16_ARC_Test (Count: in out Integer) return Test_Result is separate;
   function CRC_32_Test (Count: in out Integer) return Test_Result is separate;

end CRC_Tests;

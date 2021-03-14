with Interfaces; use Interfaces;
with Ada.text_IO; use Ada.Text_IO;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;

with Support; use Support;

package body Container_Tests is

   Buffer: aliased Array_Of_Unsigned_8 := (0..1000 => 0);
   LBuffer: aliased  Array_Of_Unsigned_8 := (0..1000 => 0);

   function Bool_Access (Count: in out Integer) return Test_Result is separate;
   function UInt8_Access (Count: in out Integer) return Test_Result is separate;
   function UInt16_Access (Count: in out Integer) return Test_Result is separate;
   function UInt32_Access (Count: in out Integer) return Test_Result is separate;
   function UInt64_Access (Count: in out Integer) return Test_Result is separate;
   function Int8_Access (Count: in out Integer) return Test_Result is separate;
   function Int16_Access (Count: in out Integer) return Test_Result is separate;
   function Int32_Access (Count: in out Integer) return Test_Result is separate;
   function Int64_Access (Count: in out Integer) return Test_Result is separate;
   function Item_Access (Count: in out Integer) return Test_Result is separate;
   function Float32_Access (Count: in out Integer) return Test_Result is separate;
   function Float64_Access (Count: in out Integer) return Test_Result is separate;
   function String_Access (Count: in out Integer) return Test_Result is separate;
   function Array_Access (Count: in out Integer) return Test_Result is separate;
   function Read_Write (Count: in out Integer) return Test_Result is separate;

end Container_Tests;

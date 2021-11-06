with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;

with Support; use Support;

package body Serializable_Tests is


   function New_Serializable_String (Count: in out Integer) return Test_Result is separate;
   function New_Serializable_Array_Of_Unsigned_8 (Count: in out Integer) return Test_Result is separate;
   function Empty_Array_Test (Count: in out Integer) return Test_Result is separate;
   function Array_Length_1 (Count: in out Integer) return Test_Result is separate;
   function Is_Empty_Test (Count: in out Integer) return Test_Result is separate;
   function Remaining_Bytes_Tests (Count: in out Integer) return Test_Result is separate;

end Serializable_Tests;

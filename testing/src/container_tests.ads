with Types; use Types;
with Test_Driver; use Test_Driver;

package Container_Tests is

   function Size_0 (Count: in out Integer) return Test_Result;
   function Size_21 (Count: in out Integer) return Test_Result;
   function Size_81 (Count: in out Integer) return Test_Result;
   function Size_1000 (Count: in out Integer) return Test_Result;
   function Bool_Access (Count: in out Integer) return Test_Result;
   function UInt8_Access (Count: in out Integer) return Test_Result;
   function UInt16_Access (Count: in out Integer) return Test_Result;
   function UInt32_Access (Count: in out Integer) return Test_Result;
   function UInt64_Access (Count: in out Integer) return Test_Result;
   function Int8_Access (Count: in out Integer) return Test_Result;
   function Int16_Access (Count: in out Integer) return Test_Result;
   function Int32_Access (Count: in out Integer) return Test_Result;
   function Int64_Access (Count: in out Integer) return Test_Result;
   function Item_Access (Count: in out Integer) return Test_Result;
   function Float32_Access (Count: in out Integer) return Test_Result;
   function Float64_Access (Count: in out Integer) return Test_Result;
   function String_Access (Count: in out Integer) return Test_Result;
   function Array_Access (Count: in out Integer) return Test_Result;
   function Read_Write (Count: in out Integer) return Test_Result;

   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("Size_0"), Size_0'Access),
      (UStr ("Size_21"), Size_21'Access),
      (UStr ("Size_81"), Size_81'Access),
      (UStr ("Size_1000"), Size_1000'Access),
      (UStr ("Bool Access"), Bool_Access'Access),
      (UStr ("UInt8_Access"), UInt8_Access'Access),
      (UStr ("UInt16_Access"), UInt16_Access'Access),
      (UStr ("UInt32_Access"), UInt32_Access'Access),
      (UStr ("UInt64_Access"), UInt64_Access'Access),
      (UStr ("Int8_Access"), UInt8_Access'Access),
      (UStr ("Int16_Access"), UInt16_Access'Access),
      (UStr ("Int32_Access"), UInt32_Access'Access),
      (UStr ("Int64_Access"), UInt64_Access'Access),
      (UStr ("Item_Access"), Item_Access'Access),
      (UStr ("Float32_Access"), Float32_Access'Access),
      (UStr ("Float64_Access"), Float64_Access'Access),
      (UStr ("String_Access"), String_Access'Access),
      (UStr ("Array_Access"), Array_Access'Access),
      (UStr ("Read_Write"), Read_Write'Access)
     );

end Container_Tests;

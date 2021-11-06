with Types; use Types;
with Test_Driver; use Test_Driver;

package Serializable_Tests is


   function New_Serializable_String (Count: in out Integer) return Test_Result;
   function New_Serializable_Array_Of_Unsigned_8 (Count: in out Integer) return Test_Result;
   function Empty_Array_Test (Count: in out Integer) return Test_Result;
   function Array_Length_1 (Count: in out Integer) return Test_Result;
   function Is_Empty_Test (Count: in out Integer) return Test_Result;
   function Remaining_Bytes_Tests (Count: in out Integer) return Test_Result;
   function Use_In_Place_Test (Count: in out Integer) return Test_Result;


   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("New Serializable String"), New_Serializable_String'Access),
      (UStr ("New Serializable Array Of Unsigned 8"), New_Serializable_Array_Of_Unsigned_8'Access),
      (UStr ("Empty_Array_Test"), Empty_Array_Test'Access),
      (UStr ("Array_Length_1"), Array_Length_1'Access),
      (UStr ("Is_Empty_Test"), Is_Empty_Test'Access),
      (UStr ("Remaining_Bytes_Tests"), Remaining_Bytes_Tests'Access),
      (UStr ("Use_In_Place_Test"), Use_In_Place_Test'Access)
     );

end Serializable_Tests;

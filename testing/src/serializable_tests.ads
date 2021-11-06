with Types; use Types;
with Test_Driver; use Test_Driver;

package Serializable_Tests is


   function New_Serializable_String (Count: in out Integer) return Test_Result;
   function New_Serializable_Array_Of_Unsigned_8 (Count: in out Integer) return Test_Result;


   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("New Serializable String"), New_Serializable_String'Access),
      (UStr ("New Serializable Array Of Unsigned 8"), New_Serializable_Array_Of_Unsigned_8'Access)
     );

end Serializable_Tests;

with Types; use Types;
with Test_Driver; use Test_Driver;


package Single_Item_Tests is

   function Create_Block_Test_No_Param (Count: in out Integer) return Test_Result;
   function Create_Block_Test_With_Param (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Types (Count: in out Integer) return Test_Result;

   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("Create Single_Item block, no param"), Create_Block_Test_No_Param'Access),
      (UStr ("Create Single_Item block, with param"), Create_Block_Test_With_Param'Access),
      (UStr ("Create Single_Item block with different types"), Create_Blocks_With_Types'Access)
     );

end Single_Item_Tests;

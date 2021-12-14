with Types; use Types;
with Test_Driver; use Test_Driver;


package Single_Item_Tests is

   function Create_Block_Test_No_Param (Count: in out Integer) return Test_Result;
   function Create_Block_Test_With_Param (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Null (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Bool (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Int_8 (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Int_16 (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Int_32 (Count: in out Integer) return Test_Result;
   function Create_Blocks_With_Int_64 (Count: in out Integer) return Test_Result;

   Tests: aliased Array_Of_Tests :=
     (
      (UStr ("Create Single_Item block, no param"), Create_Block_Test_No_Param'Access),
      (UStr ("Create Single_Item block, with param"), Create_Block_Test_With_Param'Access),
      (UStr ("Create Single_Item block with null as root"), Create_Blocks_With_Null'Access),
      (UStr ("Create Single_Item block with bool as root"), Create_Blocks_With_Bool'Access),
      (UStr ("Create Single_Item block with int_8 as root"), Create_Blocks_With_Int_8'Access),
      (UStr ("Create Single_Item block with int_16 as root"), Create_Blocks_With_Int_16'Access),
      (UStr ("Create Single_Item block with int_32 as root"), Create_Blocks_With_Int_32'Access),
      (UStr ("Create Single_Item block with int_64 as root"), Create_Blocks_With_Int_64'Access)
     );

end Single_Item_Tests;

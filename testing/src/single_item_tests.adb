package body Single_Item_Tests is

   function Create_Block_Test_No_Param (Count: in out Integer) return Test_Result is separate;
   function Create_Block_Test_With_Param (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Null (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Bool (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Int_8 (Count: in out Integer) return Test_Result is separate;

end Single_Item_Tests;

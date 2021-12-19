package body Single_Item_Tests is

   function Create_Block_Test_No_Param (Count: in out Integer) return Test_Result is separate;
   function Create_Block_Test_With_Param (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Null (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Bool (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Int_8 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Int_16 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Int_32 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Int_64 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_UInt_8 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_UInt_16 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_UInt_32 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_UInt_64 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Float_32 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Float_64 (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_String (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_CRC_String (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_Binary (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_CRC_Binary (Count: in out Integer) return Test_Result is separate;
   function Create_Blocks_With_UUID (Count: in out Integer) return Test_Result is separate;

end Single_Item_Tests;

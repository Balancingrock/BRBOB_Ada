package body Single_Item_File_Tests is

   function Create_Block_Test (Count: in out Integer) return Test_Result is separate;

   function Dummy (Count: in out Integer) return Test_Result is
   begin
      return Passed;
   end Dummy;


end Single_Item_File_Tests;

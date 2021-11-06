with BRBON;
with BRBON.Static_Unprotected;
with BRBON.Block;
with BRBON.Configure;

separate (Single_Item_File_Tests)

function Create_Block_Test (Count: in out Integer) return Test_Result is

   Test_Object: BRBON.Static_Unprotected.Instance;

begin

   Test_Object := BRBON.Static_Unprotected.Factory (Block_Type         => BRBON.Block.Single_Item_File,
                                                    Minimum_Byte_Count => 1000,
                                                    Using_Endianness   => BRBON.Configure.Machine_Endianness);

   return Passed;

end Create_Block_Test;

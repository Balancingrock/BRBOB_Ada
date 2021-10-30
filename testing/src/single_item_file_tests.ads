with Types; use Types;
with Test_Driver; use Test_Driver;


package Single_Item_File_Tests is

   function Create_Block_Test (Count: in out Integer) return Test_Result;
   function Dummy (Count: in out Integer) return Test_Result;

   Tests: aliased Array_Of_Tests :=
     (
        (UStr ("Create Single_Item_File"), Create_Block_Test'Access),
        (UStr ("Dummy"), Dummy'Access)
     );

end Single_Item_File_Tests;

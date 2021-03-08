with Types; use Types;

package Container_Tests is

   function Run return Test_Result;

private

   function Size_0 return Test_Result;
   function Size_21 return Test_Result;
   function Size_81 return Test_Result;
   function Size_1000 return Test_Result;
   function Bool_Access return Test_Result;

   Tests: Array_Of_Tests :=
     (
      (UStr ("Size_0"), Size_0'Access),
      (UStr ("Size_21"), Size_21'Access),
      (UStr ("Size_81"), Size_81'Access),
      (UStr ("Size_1000"), Size_1000'Access),
      (Ustr ("Bool Access"), Bool_Access'Access)
     );

end Container_Tests;

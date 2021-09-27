with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;

with Types; use Types;

package Support is

   function Put_As_Line (Arr: in Array_Of_Unsigned_8) return String;

   function Verify_Small_Bytes (Container: in out BRBON.Container.Instance; Offset: Unsigned_32; Expected: Array_Of_Unsigned_8) return Test_Result;

end Support;

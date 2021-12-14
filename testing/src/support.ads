with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;
with Serializable;

with Types; use Types;

package Support is

   function Put_As_Line (Arr: in Array_Of_Unsigned_8) return String;

   function Verify_Small_Bytes (Container: in out BRBON.Container.Instance; Offset: Unsigned_32; Expected: Array_Of_Unsigned_8) return Test_Result;

   function Verify_Array_Of_Unsigned_8 (Found: in out Serializable.Instance; Expected: Array_Of_Unsigned_8_Ptr; Skip_Map: Array_Of_Boolean) return Test_Result;

end Support;

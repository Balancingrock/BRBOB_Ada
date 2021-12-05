with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

with BRBON.Types;
with BRBON.Container;


package BRBON.Item is

   procedure Create_Item
    (
     Of_Type: Types.Item_Type;
     In_Container: in out Container.Instance;
     At_Location: Unsigned_32,
     With_Name: String := "",
     Using_Byte_Count: Unsigned_32 := 0,
     Parent_Offset: Unsigned_32 := 0
    );

end BRBON.Item;

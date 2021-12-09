with Interfaces; use Interfaces;

with BRBON;
with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;

package BRBON.Null_Item is

   procedure Create_Null_Item
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );

end BRBON.Null_Item;

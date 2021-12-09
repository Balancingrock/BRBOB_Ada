with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;

package BRBON.Item.Bool is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );

   function Get_Bool (C: Container.Instance; Item_Offset: Unsigned_32) return Boolean;
   pragma Inline (Get_Bool);

   procedure Set_Bool (C: Container.Instance; Item_Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Set_Bool);

end BRBON.Item.Bool;

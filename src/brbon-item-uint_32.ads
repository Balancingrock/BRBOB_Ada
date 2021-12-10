with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.UInt_32 is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: Unsigned_32 := 0
    );

   function Get_UInt_32 (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_UInt_32);

   procedure Set_UInt_32 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_UInt_32);

end BRBON.Item.UInt_32;

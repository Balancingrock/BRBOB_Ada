with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.UInt_8 is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: Unsigned_8 := 0
    );

   function Get_UInt_8 (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_UInt_8);

   procedure Set_UInt_8 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_UInt_8);

end BRBON.Item.UInt_8;

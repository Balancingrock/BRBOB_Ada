with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.Float_64 is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: IEEE_Float_64 := 0.0
    );

   function Get_Float_64 (C: Container.Instance; Item_Offset: Unsigned_32) return IEEE_Float_64;
   pragma Inline (Get_Float_64);

   procedure Set_Float_64 (C: Container.Instance; Item_Offset: Unsigned_32; Value: IEEE_Float_64);
   pragma Inline (Set_Float_64);

end BRBON.Item.Float_64;

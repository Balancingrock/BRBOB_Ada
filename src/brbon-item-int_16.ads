with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.Int_16 is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: Integer_16 := 0
    );

   function Get_Int_16 (C: Container.Instance; Item_Offset: Unsigned_32) return Integer_16;
   pragma Inline (Get_Int_16);

   procedure Set_Int_16 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Integer_16);
   pragma Inline (Set_Int_16);

end BRBON.Item.Int_16;

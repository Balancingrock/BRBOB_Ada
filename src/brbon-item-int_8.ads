with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.Int_8 is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: Integer_8 := 0
    );

   function SU_Get_Int_8 (C: Container.Instance; Item_Offset: Unsigned_32) return Integer_8;
   pragma Inline (SU_Get_Int_8);

   procedure SU_Set_Int_8 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Integer_8);
   pragma Inline (SU_Set_Int_8);

end BRBON.Item.Int_8;

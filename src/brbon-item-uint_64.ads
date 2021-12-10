with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.UInt_64 is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: Unsigned_64 := 0
    );

   function Get_UInt_64 (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_64;
   pragma Inline (Get_UInt_64);

   procedure Set_UInt_64 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_64);
   pragma Inline (Set_UInt_64);

end BRBON.Item.UInt_64;

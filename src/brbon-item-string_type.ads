with Interfaces; use Interfaces;

with BRBON.Item;
with BRBon.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item.String_Type is

   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0;
     Initial_Value: Integer_8 := 0
    );

   function Get_String (C: Container.Instance; Item_Offset: Unsigned_32) return String;
   pragma Inline (Get_String);

   procedure Set_String (C: Container.Instance; Item_Offset: Unsigned_32; Value: String);
   pragma Inline (Set_String);

end BRBON.Item.String_Type;

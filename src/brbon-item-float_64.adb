package body BRBON.Item.Float_64 is


   procedure Create_Layout
     (
      In_Container: in out Container.Instance;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      Using_Byte_Count: Unsigned_32 := 0;
      Parent_Offset: Unsigned_32 := 0;
      Initial_Value: IEEE_Float_64 := 0.0
     ) is

   begin

      Item.Create_Layout
        (
         In_Container,
         At_Offset,
         Types.Float_64_Type,
         With_Name,
         Using_Byte_Count,
         Parent_Offset
        );

      Set_Float_64 (In_Container, At_Offset, Initial_Value);

   end Create_Layout;


   function Get_Float_64 (C: Container.Instance; Item_Offset: Unsigned_32) return IEEE_Float_64 is
   begin
      return Container.Get_Float_64 (C, Item.Get_Value_Offset (C, Item_Offset));
   end Get_Float_64;


   procedure Set_Float_64 (C: Container.Instance; Item_Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      Container.Set_Float_64 (C, Item.Get_Value_Offset (C, Item_Offset), Value);
   end Set_Float_64;


end BRBON.Item.Float_64;

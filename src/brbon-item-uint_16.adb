package body BRBON.Item.UInt_16 is


   procedure Create_Layout
     (
      In_Container: in out Container.Instance;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      Using_Byte_Count: Unsigned_32 := 0;
      Parent_Offset: Unsigned_32 := 0;
      Initial_Value: Unsigned_16 := 0
     ) is

   begin

      Item.Create_Layout
        (
         In_Container,
         At_Offset,
         Types.UInt_16_Type,
         With_Name,
         Using_Byte_Count,
         Parent_Offset
        );

      Set_UInt_16 (In_Container, At_Offset, Initial_Value);

   end Create_Layout;


   function Get_UInt_16 (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (C, Item.Get_Value_Offset (C, Item_Offset));
   end Get_UInt_16;


   procedure Set_UInt_16 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (C, Item.Get_Value_Offset (C, Item_Offset), Value);
   end Set_UInt_16;


end BRBON.Item.UInt_16;

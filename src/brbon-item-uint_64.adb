package body BRBON.Item.UInt_64 is


   procedure Create_Layout
     (
      In_Container: in out Container.Instance;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      Using_Byte_Count: Unsigned_32 := 0;
      Parent_Offset: Unsigned_32 := 0;
      Initial_Value: Unsigned_64 := 0
     ) is

   begin

      Item.Create_Layout
        (
         In_Container,
         At_Offset,
         Types.UInt_64_Type,
         With_Name,
         Using_Byte_Count,
         Parent_Offset
        );

      Set_UInt_64 (In_Container, At_Offset, Initial_Value);

   end Create_Layout;


   function Get_UInt_64 (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (C, Item.Get_Value_Offset (C, Item_Offset));
   end Get_UInt_64;


   procedure Set_UInt_64 (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      Container.Set_Unsigned_64 (C, Item.Get_Value_Offset (C, Item_Offset), Value);
   end Set_UInt_64;


end BRBON.Item.UInt_64;

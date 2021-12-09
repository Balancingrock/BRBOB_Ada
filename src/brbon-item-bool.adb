package body BRBON.Item.Bool is


   procedure Create_Layout
     (
      In_Container: in out Container.Instance;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      Using_Byte_Count: Unsigned_32 := 0;
      Parent_Offset: Unsigned_32 := 0;
      Value: Boolean
     ) is

   begin

      Item.Create_Layout
        (
         In_Container,
         At_Offset,
         Types.Bool_Type,
         With_Name,
         Using_Byte_Count,
         Parent_Offset
        );

      Set_Bool (C, At_Offset, Value);

   end Create_Layout;


   function Get_Bool (C: Container.Instance; Item_Offset: Unsigned_32) return Boolean is
   begin
      return Container.Get_Bool (C, Item.Get_Value_Offset (C, Item_Offset));
   end Get_Bool;


   procedure Set_Bool (C: Container.Instance; Item_Offset: Unsigned_32; Value: Boolean) is
   begin
      Container.Set_Bool (C, Item.Get_Value_Offset (C, Item_Offset), Value);
   end Set_Bool;


end BRBON.Item.Bool;

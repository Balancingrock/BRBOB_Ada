package body BRBON.Item.Null_Type is

   procedure Create_Layout
     (
      In_Container: in out Container.Instance;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      Using_Byte_Count: Unsigned_32 := 0;
      Parent_Offset: Unsigned_32 := 0
     ) is

   begin

      Item.Create_Layout
        (
         In_Container,
         At_Offset,
         Types.Null_Type,
         With_Name,
         Using_Byte_Count,
         Parent_Offset
        );

   end Create_Layout;

end BRBON.Item.Null_Type;

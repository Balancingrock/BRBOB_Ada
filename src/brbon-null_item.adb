with BRBON.Item;
with BRBON.Types;

package body BRBON.Null_Item is

   procedure Create_Null_Item
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    ) is
   begin

      Item.Create_Item_Layout (In_Container     => In_Container,
                               At_Offset        => At_Offset,
                               Of_Type          => Types.Null_Type,
                               With_Name        => With_Name,
                               Using_Byte_Count => Using_Byte_Count,
                               Parent_Offset    => Parent_Offset);

   end Create_Null_Item;

end BRBON.Null_Item;

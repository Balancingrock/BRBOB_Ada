with BRBON_Basic_Types; use BRBON_Basic_Types;


package BRBON.Item_Manager_Operations is

   procedure Create_Item(Manager: in out Item_Manager;
                         Of_Type: in Item_Type;
                         At_Offset: in Unsigned_32);

   procedure Create_Item(Manager: in out Item_Manager;
                         Of_Type: in Item_Type;
                         With_Name: in Item_Name;
                         At_Offset: in Unsigned_32);

end BRBON.Item_Manager_Operations;

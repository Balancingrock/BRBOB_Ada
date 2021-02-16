separate (Brbon.Item_Manager_Operations)

procedure Set_Item_Name(Mgr: in out Item_Manager;
                        Item_Ptr: in Unsigned_8_Ptr;
                        Name: in Item_Name)is ;

   New_Name_Field_Byte_Count, Existing_Name_Field_Byte_Count: Unsigned_8;

begin

   -- If the new name byte count is lower or equal to the existing name byte count then the current name can be overwritten.
   -- Note: It could also be possible to reclaim unused space but since speed is prioritized over space this is not done.

   New_Byte_Count := Bounded_Item_Name.Length(Source => Name) + Item_Name_Overhead;
   Existing_Byte_Count := Item_Ptr.all.Item_Name_Field_Byte_Count;

   if New_Byte_Count <= Existing_Byte_Count then
      Replace(Item_Ptr, Name);
      return
   end if;

   -- The new byte count is higher than the previous one. There are now two possibilities:
   -- 1. The byte count of the item is sufficient to accomodate the new name after the value is shifted to make place for the new name.
   -- 2. The byte count of the item must be increased, shifting all items after it, to make place for the new name.





end Set_Item_Name;

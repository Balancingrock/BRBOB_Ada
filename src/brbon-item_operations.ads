package BRBON.Item_Operations is

   -- Increase the size for the given item.
   -- If necessary the parent items will be increased in length as well and the storage area if needed and possible.
   -- @value Mgr The item manager for the item.
   -- @value Item_Ptr The item to increase.
   -- @value: Byte_Count The requested byte count for the item. Note: this cannot be used to make the item smaller and the byte-count will be rounded up to the next 32 bit boundary.
   -- @raise This operation can raise the Storage_Error exception.
   --
   procedure Increase_Item_Byte_Size (Mgr: Item_Manager_Ptr; Item_Ptr: Unsigned_8_Ptr; Byte_Count: Unsigned_32);

end BRBON.Item_Operations;

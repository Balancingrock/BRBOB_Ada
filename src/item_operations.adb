with Pointer_Operations;


package body Item_Operations is

   procedure Set_Item_Type (Item_Ptr: Unsigned_8_Ptr; Value: Item_Type) is
   begin
      Pointer_Operations.Set_Item_Type (Ptr => Item_Ptr, Value => Unsigned_8 (Item_Type'Pos (Value)));
   end Set_Item_Type;

   function Get_Item_Type (Item_Ptr: Unsigned_8_Ptr) return Item_Type is
      Raw: Unsigned_8 := Pointer_Operations.Get_Item_Type (Ptr => Item_Ptr);
   begin
      if Raw > Item_Type'Pos (Item_Type'Last) then raise Enum_Mapping_Failed; end if;
      return Item_Type'Val (Raw);
   end Get_Item_Type;

   procedure Set_Item_Options (Item_Ptr: Unsigned_8_Ptr; Value: Item_Options) is
   begin
      Pointer_Operations.Set_Item_Options (Ptr   => Item_Ptr, Value => To_Unsigned_8 (Value));
   end Set_Item_Options;

   function Get_Item_Options (Item_Ptr: Unsigned_8_Ptr) return Item_Options is
   begin
      return To_Item_Options (Pointer_Operations.Get_Item_Options (Ptr => Item_Ptr));
   end Get_Item_Options;

   procedure Set_Item_Flags (Item_Ptr: Unsigned_8_Ptr; Value: Bits_8) is
   begin
      Pointer_Operations.Set_Item_Flags (Ptr   => Item_Ptr, Value => To_Unsigned_8 (Value));
   end Set_Item_Flags;

   function Get_Item_Flags (Item_Ptr: Unsigned_8_Ptr) return Bits_8 is
   begin
      return To_Bits_8 (Pointer_Operations.Get_Item_Flags (Ptr => Item_Ptr));
   end Get_Item_Flags;


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

      raise Incomplete_Code;

   end Set_Item_Name;

end Item_Operations;

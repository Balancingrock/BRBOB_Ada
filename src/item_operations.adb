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


end Item_Operations;

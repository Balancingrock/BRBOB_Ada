with Data_Access; use Data_Access;
with Item_Manager; use Item_Manager;

package body Item_Access is

   Item_Type_Offset: constant Unsigned_32 := 0;
   Item_Options_Offset: constant Unsigned_32 := 1;
   Item_Flags_Offset: constant Unsigned_32 := 2;
   Item_Name_Field_Byte_Count: constant Unsigned_32 := 3;

   procedure Set_Item_Type (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Item_Type) is
   begin
      Set_Unsigned_8 (Base   => Base,
                      Offset => Item_Offset + Item_Type_Offset,
                      Value  => Unsigned_8 (Item_Type'Pos(Value)));
   end Set_Item_Type;

   function Get_Item_Type (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Item_Type is
      Raw: Unsigned_8 := Get_Unsigned_8 (Base   => Base,
                                         Offset => Item_Offset + Item_Type_Offset);
   begin
      if Raw > Item_Type'Pos (Item_Type'Last) then raise Enum_Mapping_Failed; end if;
      return Item_Type'Val (Raw);
   end Get_Item_Type;

   procedure Set_Item_Options (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Item_Options) is
   begin
      Set_Unsigned_8 (Base   => Base,
                      Offset => Item_Offset + Item_Options_Offset,
                      Value  => To_Unsigned_8 (Value));
   end Set_Item_Options;

   function Get_Item_Options (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Item_Options is
   begin
      return To_Item_Options (Get_Unsigned_8 (Base   => Base,
                                              Offset => Item_Offset + Item_Options_Offset));
   end Get_Item_Options;

   procedure Set_Item_Flags (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Bits_8) is
   begin
      Set_Unsigned_8 (Base   => Base,
                      Offset => Item_Offset + Item_Flags_Offset,
                      Value  => To_Unsigned_8 (Value));
   end Set_Item_Flags;

   function Get_Item_Flags (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Bits_8 is
   begin
      return To_Bits_8 (Get_Unsigned_8 (Base   => Base,
                                        Offset => Item_Offset + Item_Flags_Offset));
   end Get_Item_Flags;

   procedure Set_Item_Name_Field_Byte_Count (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Set_Unsigned_8 (Base   => Base,
                      Offset => Item_Offset + Item_Flags_Offset,
                      Value  => Value);
   end Set_Item_Name_Field_Byte_Count;

   function Get_Item_Name_Field_Byte_Count (Base: Storage_Area_Ptr; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Get_Unsigned_8 (Base   => Base,
                             Offset => Item_Offset + Item_Flags_Offset);
   end Get_Item_Name_Field_Byte_Count;

end Item_Access;

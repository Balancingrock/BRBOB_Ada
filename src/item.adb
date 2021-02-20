package body Item is


   function New_Item (Manager: Item_Manager_Ptr; Offset: Unsigned_32) return Item is
   begin
      return (Manager, Offset);
   end New_Item;


   Item_Type_Offset: constant Unsigned_32 := 0;
   Item_Options_Offset: constant Unsigned_32 := 1;
   Item_Flags_Offset: constant Unsigned_32 := 2;
   Item_Name_Field_Byte_Count: constant Unsigned_32 := 3;


   procedure Set_Item_Type (I: Item; Value: Item_Type) is
   begin
      I.Manager.Storage.Set_Unsigned_8 (I.Offset + Item_Type_Offset, Unsigned_8 (Item_Type'Pos (Value)));
   end Set_Item_Type;


   function Get_Item_Type (I: Item) return Item_Type is
      Raw: Unsigned_8 := I.Manager.Storage.Get_Unsigned_8 (I.Offset + Item_Type_Offset);
   begin
      if Raw > Item_Type'Pos (Item_Type'Last) then raise Enum_Mapping_Failed; end if;
      return Item_Type'Val (Raw);
   end Get_Item_Type;


   procedure Set_Item_Options (I: Item; Value: Item_Options) is
   begin
      I.Manager.Storage.Set_Unsigned_8 (I.Offset + Item_Options_Offset, To_Unsigned_8 (Value));
   end Set_Item_Options;


   function Get_Item_Options (I: Item) return Item_Options is
   begin
      return To_Item_Options (I.Manager.Storage.Get_Unsigned_8 (I.Offset + Item_Options_Offset));
   end Get_Item_Options;


   procedure Set_Item_Flags (I: Item; Value: Bits_8) is
   begin
      I.Manager.Storage.Set_Unsigned_8 (I.Offset + Item_Flags_Offset, To_Unsigned_8 (Value));
   end Set_Item_Flags;


   function Get_Item_Flags (I: Item) return Bits_8 is
   begin
      return To_Bits_8 (I.Manager.Storage.Get_Unsigned_8 (I.Offset + Item_Flags_Offset));
   end Get_Item_Flags;


   procedure Set_Item_Name_Field_Byte_Count (I: Item; Value: Unsigned_8) is
   begin
      I.Manager.Storage.Set_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count, Value);
   end Set_Item_Name_Field_Byte_Count;


   function Get_Item_Name_Field_Byte_Count (I: Item) return Unsigned_8 is
   begin
      return I.Manager.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count);
   end Get_Item_Name_Field_Byte_Count;


end Item;

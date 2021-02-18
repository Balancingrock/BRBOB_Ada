with Pointer_Access; use Pointer_Access;
with Item_Manager; use Item_Manager;

package body Item_Access is

   procedure Set_Item_Type (Item_Ptr: Unsigned_8_Ptr; Value: Item_Type) is
   begin
      Set_Item_Type (Ptr => Item_Ptr, Value => Unsigned_8 (Item_Type'Pos (Value)));
   end Set_Item_Type;

   function Get_Item_Type (Item_Ptr: Unsigned_8_Ptr) return Item_Type is
      Raw: Unsigned_8 := Get_Item_Type (Ptr => Item_Ptr);
   begin
      if Raw > Item_Type'Pos (Item_Type'Last) then raise Enum_Mapping_Failed; end if;
      return Item_Type'Val (Raw);
   end Get_Item_Type;

   procedure Set_Item_Options (Item_Ptr: Unsigned_8_Ptr; Value: Item_Options) is
   begin
      Set_Item_Options (Ptr   => Item_Ptr, Value => To_Unsigned_8 (Value));
   end Set_Item_Options;

   function Get_Item_Options (Item_Ptr: Unsigned_8_Ptr) return Item_Options is
   begin
      return To_Item_Options (Get_Item_Options (Ptr => Item_Ptr));
   end Get_Item_Options;

   procedure Set_Item_Flags (Item_Ptr: Unsigned_8_Ptr; Value: Bits_8) is
   begin
      Set_Item_Flags (Ptr   => Item_Ptr, Value => To_Unsigned_8 (Value));
   end Set_Item_Flags;

   function Get_Item_Flags (Item_Ptr: Unsigned_8_Ptr) return Bits_8 is
   begin
      return To_Bits_8 (Get_Item_Flags (Ptr => Item_Ptr));
   end Get_Item_Flags;


   procedure Set_Item_Name(Mgr: in out Item_Manager.Item_Manager;
                           Item_Ptr: in Unsigned_8_Ptr;
                           Name: in Item_Name) is
   begin
      raise Incomplete_Code;
   end Set_Item_Name;

end Item_Access;

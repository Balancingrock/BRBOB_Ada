with Name_Field_Assistent_Package; use Name_Field_Assistent_Package;


package body Item_Bool_Package is


   function Get_Value (C: Container; Item_Offset: Unsigned_32) return Boolean is
      IPtr: Item_Bool_Ptr := To_Item_Bool_Ptr (C.Get_Unsigned_8_Ptr (Item_Offset));
   begin
      return IPtr.Value /= 0;
   end Get_Value;


   procedure Set_Value (C: Container; Item_Offset: Unsigned_32; Value: Boolean) is
      IPtr: Item_Bool_Ptr := To_Item_Bool_Ptr (C.Get_Unsigned_8_Ptr (Item_Offset));
   begin
      if Value then
         IPtr.Value := 1;
      else
         IPtr.Value := 0;
      end if;
   end Set_Value;


   procedure Create_Item_Bool (C: Container; Item_Offset: Unsigned_32; Parent_Offset: Unsigned_32; Value: Boolean := False; Name: Item_Name_Package.Bounded_String := No_Name) is
      IPtr: Item_Bool_Ptr := To_Item_Bool_Ptr (C.Get_Unsigned_8_Ptr (Item_Offset));
      Assist: Name_Field_Assistent := Name_Field_Assistent_Factory (C, Name);
   begin
      IPtr.The_Item_Header.The_Type := Bool_Type;
      IPtr.The_Item_Header.Options := No_Item_Options;
      IPtr.The_Item_Header.Flags := No_Item_Flags;
      IPtr.The_Item_Header.Name_Field_Byte_Count := Assist.Field_Byte_Count;
      IPtr.The_Item_Header.Parent_Offset := Parent_Offset;
      if Value then
         IPtr.Value := 1;
      else
         IPtr.Value := 0;
      end if;
      IPtr.Filler_8 := 0;
      IPtr.Filler_16 := 0;

   end Create_Item_Bool;

end Item_Bool_Package;

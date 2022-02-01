with GNAT.Byte_Swapping;


package body Item_Package is


   -- Support

   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);


   -- API

   function Get_Item_Type (C: Container; Item_Offset: Unsigned_32) return Item_Type is (Get_Item_Header_Ptr (C, Item_Offset).The_Type);


   procedure Set_Item_Type (C: Container; Item_Offset: Unsigned_32; Value: Item_Type) is
   begin
      Get_Item_Header_Ptr (C, Item_Offset).The_Type := Value;
   end Set_Item_Type;


   function Get_Item_Options (C: Container; Item_Offset: Unsigned_32) return Item_Options is (Get_Item_Header_Ptr (C, Item_Offset).Options);


   procedure Set_Item_Options (C: Container; Item_Offset: Unsigned_32; Value: Item_Options) is
   begin
      Get_Item_Header_Ptr (C, Item_Offset).Options := Value;
   end Set_Item_Options;


   function Get_Item_Flags (C: Container; Item_Offset: Unsigned_32) return Item_Flags is (Get_Item_Header_Ptr (C, Item_Offset).Flags);


   procedure Set_Item_Flags (C: Container; Item_Offset: Unsigned_32; Value: Item_Flags) is
   begin
      Get_Item_Header_Ptr (C, Item_Offset).Flags := Value;
   end Set_Item_Flags;


   function Get_Item_Name_Field_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_8 is (Get_Item_Header_Ptr (C, Item_Offset).Name_Field_Byte_Count);

   procedure Set_Item_Name_Field_Byte_Count (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Get_Item_Header_Ptr (C, Item_Offset).Name_Field_Byte_Count := Value;
   end Set_Item_Name_Field_Byte_Count;


   function Get_Item_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      if C.Swap_Status then
         return Swap_Unsigned_32 (Get_Item_Header_Ptr (C, Item_Offset).Byte_Count);
      else
         return Get_Item_Header_Ptr (C, Item_Offset).Byte_Count;
      end if;
   end Get_Item_Byte_Count;


   procedure Set_Item_Byte_Count (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if C.Swap_Status then
         Get_Item_Header_Ptr (C, Item_Offset).Byte_Count := Swap_Unsigned_32 (Value);
      else
         Get_Item_Header_Ptr (C, Item_Offset).Byte_Count := Value;
      end if;
   end Set_Item_Byte_Count;


   function Get_Item_Parent_Offset (C: Container; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      if C.Swap_Status then
         return Swap_Unsigned_32 (Get_Item_Header_Ptr (C, Item_Offset).Parent_Offset);
      else
         return Get_Item_Header_Ptr (C, Item_Offset).Parent_Offset;
      end if;
   end Get_Item_Parent_Offset;


   procedure Set_Item_Parent_Offset (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if C.Swap_Status then
         Get_Item_Header_Ptr (C, Item_Offset).Parent_Offset := Swap_Unsigned_32 (Value);
      else
         Get_Item_Header_Ptr (C, Item_Offset).Parent_Offset := Value;
      end if;
   end Set_Item_Parent_Offset;


   function Get_Item_Name_CRC (C: Container; Item_Offset: Unsigned_32) return CRC_Package.CRC_16 is
   begin
      if C.Swap_Status then
         return Swap_Unsigned_16 (Get_Item_Header_With_Name_Ptr (C, Item_Offset).Name_CRC);
      else
         return Get_Item_Header_With_Name_Ptr (C, Item_Offset).Name_CRC;
      end if;
   end Get_Item_Name_CRC;


   procedure Set_Item_Name_CRC (C: Container; Item_Offset: Unsigned_32; Value: CRC_Package.CRC_16) is
   begin
      if C.Swap_Status then
         Get_Item_Header_With_Name_Ptr (C, Item_Offset).Name_CRC := Swap_Unsigned_16 (Value);
      else
         Get_Item_Header_With_Name_Ptr (C, Item_Offset).Name_CRC := Value;
      end if;
   end Set_Item_Name_CRC;


   function Get_Item_Name_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_8 is (Get_Item_Header_With_Name_Ptr (C, Item_Offset).Name_Byte_Count);


   procedure Set_Item_Name_Byte_Count (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Get_Item_Header_With_Name_Ptr (C, Item_Offset).Name_Byte_Count := Value;
   end Set_Item_Name_Byte_Count;


   function Get_Item_Name (C: Container; Item_Offset: Unsigned_32) return Item_Name_Package.Bounded_String is
      Length: Unsigned_32 := Unsigned_32 (Get_Item_Name_Byte_Count (C, Item_Offset));
   begin
      return Item_Name_Package.To_Bounded_String (C.Get_String (Item_Offset + Item_Name_Byte_Code_Offset, Length));
   end Get_Item_Name;


   procedure Set_Item_Name (C: Container; Item_Offset: Unsigned_32; Value: Item_Name_Package.Bounded_String) is
   begin
      C.Set_String (Item_Offset + Item_Name_Byte_Code_Offset, Item_Name_Package.To_String (Value));
   end Set_Item_Name;


end Item_Package;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNAT.Byte_Swapping;

with CRC_Package; use CRC_Package;

with BRBON.Utils;
with BRBON.Container;
with BRBON.Portal_Package; use BRBON.Portal_Package;


package body BRBON.Item_Package is
   
   
   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);
   function Swap_Unsigned_64 is new GNAT.Byte_Swapping.Swapped8 (Unsigned_64);
   function Swap_Integer_16 is new GNAT.Byte_Swapping.Swapped2 (Integer_16);
   function Swap_Integer_32 is new GNAT.Byte_Swapping.Swapped4 (Integer_32);
   function Swap_Integer_64 is new GNAT.Byte_Swapping.Swapped8 (Integer_64);
   function Swap_Float_32 is new GNAT.Byte_Swapping.Swapped4 (IEEE_Float_32);
   function Swap_Float_64 is new GNAT.Byte_Swapping.Swapped8 (IEEE_Float_64);

   
   -----------------------------------------------------------------------------
   
   function Item_Header_Get_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Type_Field;
   end Item_Header_Get_Type;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Header_Set_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Type_Field := Value;
   end Item_Header_Set_Type;
   
   
   -----------------------------------------------------------------------------

   function Item_Header_Get_Options (S: Store; Item_Offset: Unsigned_32) return Item_Options is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Options_Field;
   end Item_Header_Get_Options;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Header_Set_Options (S: Store; Item_Offset: Unsigned_32; Value: Item_Options) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Options_Field := Value;
   end Item_Header_Get_Options;
   
   
   -----------------------------------------------------------------------------

   function Item_Header_Get_Flags (S: Store; Item_Offset: Unsigned_32) return Item_Flags is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Flags_Field;
   end Item_Header_Get_Flags;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Header_Set_Flags (S: Store; Item_Offset: Unsigned_32; Value: Item_Flags) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Flags_Field := Value;
   end Item_Header_Set_Flags;
   
   
   -----------------------------------------------------------------------------

   function Item_Header_Get_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Name_Field_Byte_Count_Field;
   end Item_Header_Get_Name_Field_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Header_Set_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Name_Field_Byte_Count_Field := Value;
   end Item_Header_Set_Name_Field_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Header_Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (IPtr.Byte_Count_Field);
      else
         return IPtr.Byte_Count_Field;
      end if;
   end Item_Header_Get_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Header_Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         IPtr.Byte_Count_Field := Swap_Unsigned_32 (Value);
      else
         IPtr.Byte_Count_Field := Value;
      end if;
   end Item_Header_Set_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   function Item_Header_Get_Parent_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      IPtr.Byte_Count_Field := Swap_Unsigned_32 (Value);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (IPtr.Parent_Offset_Field);
      else
         return IPtr.Parent_Offset_Field;
      end if;
   end Item_Header_Get_Parent_Offset;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Header_Set_Parent_Offset (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      IPtr.Byte_Count_Field := Swap_Unsigned_32 (Value);
   begin
      if S.Swap then
         IPtr.Parent_Offset_Field := Swap_Unsigned_32 (Value);
      else
         IPtr.Parent_Offset_Field := Value;
      end if;
   end Item_Header_Set_Parent_Offset;
   

   -----------------------------------------------------------------------------

   function Small_Value_Get_Bool (S: Store; Item_Offset: Unsigned_32) return Boolean is
   begin
      return Container.Get_Bool (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Bool;
   

   -----------------------------------------------------------------------------

   procedure Small_Value_Set_Bool (S: Store; Item_Offset: Unsigned_32; Value: Boolean) is
   begin
      Container.Set_Bool (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Bool;

   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int8 (S: Store; Item_Offset: Unsigned_32) return Integer_8 is
   begin
      return Container.Get_Integer_8 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int8;

   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int8 (S: Store; Item_Offset: Unsigned_32; Value: Integer_8) is
   begin
      Container.Set_Integer_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int8;

   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt8 (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt8;
   
   
   -----------------------------------------------------------------------------

   procedure Small_Value_Set_UInt8 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt8;
   
   
   -----------------------------------------------------------------------------

   function Small_Value_Get_Int16 (S: Store; Item_Offset: Unsigned_32) return Integer_16 is
   begin
      return Container.Get_Integer_16 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int16;

   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int16 (S: Store; Item_Offset: Unsigned_32; Value: Integer_16) is
   begin
      Container.Set_Integer_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int16;
   
   
   -----------------------------------------------------------------------------

   function Small_Value_Get_UInt16 (S: Store; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt16;

   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt16 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt16;

   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int32 (S: Store; Item_Offset: Unsigned_32) return Integer_32 is
   begin
      return Container.Get_Integer_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int32;
   
   
   -----------------------------------------------------------------------------

   procedure Small_Value_Set_Int32 (S: Store; Item_Offset: Unsigned_32; Value: Integer_32) is
   begin
      Container.Set_Integer_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int32;

   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt32 (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt32;
   
   
   -----------------------------------------------------------------------------

   procedure Small_Value_Set_UInt32 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt32;

   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Float32 (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32 is
   begin
      return Container.Get_Float_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Float32;

   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Float32 (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      Container.Set_Float_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Float32;

   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Color (S: Store; Item_Offset: Unsigned_32) return Color is
   begin
      return Color_Factory (Red   => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset),
                            Green => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1),
                            Blue  => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2),
                            Alpha => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3));
   end Small_Value_Get_Color;

   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Color (S: Store; Item_Offset: Unsigned_32; Value: Color) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Get_Red_Component (Color));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1, Get_Green_Component (Color));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2, Get_Blue_Component (Color));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3, Get_Alpha_Component (Color));
   end Small_Value_Set_Color;

   
   -----------------------------------------------------------------------------
   
   function Item_Name_Get_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_16 is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return CRC_16 (Swap_Unsigned_16 (NPtr.CRC));
      else
         return CRC_16 (NPtr.CRC);
      end if;
   end Item_Name_Get_CRC;
   

   -----------------------------------------------------------------------------
   
   procedure Item_Name_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_16) is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         NPtr.CRC = CRC_16 (Swap_Unsigned_16 (Unsigned_16 (Value)));
      else
         NPtr.CRC = Value;
      end if;
   end Item_Name_Set_CRC;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Name_Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      NPtr.Byte_Count := Value;
   end Item_Name_Set_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Name_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String is
      Byte_Count: Unsigned_8 := Item_Name_Get_Byte_Count (S, Item_Offset);
      Offset: Unsigned_32 := Item_Offset + Item_Name_Ascii_Code_Offset;
   begin
      return Container.Get_String (S, Offset, Byte_count);
   end Item_Name_Get_ASCII_Code;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Name_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String) is
      Offset: Unsigned_32 := Item_Offset + Item_Name_Ascii_Code_Offset;
   begin
      Container.Set_String (S, Offset, Value);
   end Item_Name_Set_ASCII_Code;


   -----------------------------------------------------------------------------
   -- Item Value: String

   
   -----------------------------------------------------------------------------
   
   function Item_Value_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      VPtr: Item_Value_String_Header_Ptr := Get_Item_Value_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (VPtr.Count);
      else
         return VPtr.Count;
      end if;
   end Item_Value_String_Get_Count;

   
   -----------------------------------------------------------------------------
   
   procedure Item_Value_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      VPtr: Item_Value_String_Header_Ptr := Get_Item_Value_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return VPtr.Count := Swap_Unsigned_32 (Value);
      else
         return VPtr.Count := Value;
      end if;
   end Item_Value_String_Set_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String is
      Byte_Count: Unsigned_32 := Item_Value_String_Get_Count (S, Item_Offset);
   begin
      return Container.Get_String (S, Item_Offset + Item_Value_String_Header_Byte_Count, Byte_Count);
   end Item_Value_String_Get_ASCII_Code;
   

   -----------------------------------------------------------------------------
   
   procedure Item_Value_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String) is
      Byte_Count: Unsigned_32 := Item_Value_String_Get_Count (S, Item_Offset);
   begin
      Container.Set_String (S, Item_Offset + Item_Value_String_Header_Byte_Count, Value);
   end Item_Value_String_Set_ASCII_Code;


   -----------------------------------------------------------------------------
   -- Item Value: CRC String

   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_String_Get_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_32 is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return CRC_32 (Swap_Unsigned_32 (Unsigned_32 (VPtr.CRC)));
      else
         return Vptr.CRC;
      end if;
   end Item_Value_CRC_String_Get_CRC;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_String_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_32) is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         VPtr.CRC := CRC_32 (Swap_Unsigned_32 (Unsigned_32 (Value)));
      else
         VPtr.CRC := Value;
      end if;
   end Item_Value_CRC_String_Set_CRC;

   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (VPtr.Count);
      else
         return Vptr.Count;
      end if;
   end Item_Value_CRC_String_Get_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         VPtr.CRC := Swap_Unsigned_32 (Value);
      else
         VPtr.CRC := Value;
      end if;
   end Item_Value_CRC_String_Set_Count;

   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String is
      Byte_Count: Unsigned_32 := Item_Value_CRC_String_Get_Count (S, Item_Offset);
   begin
      return Container.Get_String (S, Item_Offset + , Byte_Count);
   end Item_Value_CRC_String_Get_ASCII_Code;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String);


   -----------------------------------------------------------------------------
   -- Item Value: Binary

   type Item_Value_Binary_Header is
      record
         Count: Unsigned_32;
      end record;

   for Item_Value_Binary_Header'Size use 32;

   for Item_Value_Binary_Header use
      record
         Count at 0 range 0..31;
      end record;

   type Item_Value_Binary_Header_Ptr is access Item_Value_Binary_Header;

   function To_Item_Value_Binary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Binary_Header_Ptr);

   function Get_Item_Value_Binary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Binary_Header_Ptr is (To_Item_Value_Binary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array;

   procedure Item_Value_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array);


   -----------------------------------------------------------------------------
   -- Item Value: CRC Binary

   type Item_Value_CRC_Binary_Header is
      record
         CRC: CRC_32;
         Count: Unsigned_32;
      end record;

   for Item_Value_CRC_Binary_Header'Size use 32 + 32;

   for Item_Value_CRC_Binary_Header use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
      end record;

   type Item_Value_CRC_Binary_Header_Ptr is access Item_Value_CRC_Binary_Header;

   function To_Item_Value_CRC_Binary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_CRC_Binary_Header_Ptr);

   function Get_Item_Value_CRC_Binary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_CRC_Binary_Header_Ptr is (To_Item_Value_CRC_Binary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_CRC_Binary_Get_CRC (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_CRC_Binary_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_CRC_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_CRC_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_CRC_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array;

   procedure Item_Value_CRC_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array);


   -----------------------------------------------------------------------------
   -- Item Value: Array

   type Item_Value_Array_Header is
      record
         Reserved_1: Unsigned_32;
         Element_Type: Item_Type;
         Reserved_2: Unsigned_8;
         Reserved_3: Unsigned_16;
         Element_Count: Unsigned_32;
         Element_Byte_Count: Unsigned_32;
      end record;

   for Item_Value_Array_Header'Size use 32 + 8 + 8 + 16 + 32 + 32 + 8;

   for Item_Value_Array_Header use
      record
         Reserved_1         at 0  range 0..31;
         Element_Type       at 4  range 0..7;
         Reserved_2         at 5  range 0..7;
         Reserved_3         at 6  range 0..15;
         Element_Count      at 8  range 0..31;
         Element_Byte_Count at 12 range 0..31;
      end record;

   type Item_Value_Array_Header_Ptr is access Item_Value_Array_Header;

   function To_Item_Value_Array_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Array_Header_Ptr);

   function Get_Item_Value_Array_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_CRC_Binary_Header_Ptr is (To_Item_Value_Array_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Array_Get_Element_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type;

   procedure Item_Value_Array_Set_Element_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type);

   function Item_Value_Array_Get_Element_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Array_Set_Element_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Array_Get_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Array_Set_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Array_Get_First_Element_Offset (S:Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Sequence

   type Item_Value_Sequence_Header is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
      end record;

   for Item_Value_Sequence_Header'Size use 32 + 32;

   for Item_Value_Sequence_Header use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
      end record;

   type Item_Value_Header_Sequence_Ptr is access Item_Value_Sequence_Header;

   function To_Item_Value_Sequence_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Sequence_Header_Ptr);

   function Get_Item_Value_Sequence_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Sequence_Header_Ptr is (To_Item_Value_Sequence_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Sequence_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Sequence_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Sequence_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Dictionary

   type Item_Value_Dictionary_Header is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
      end record;

   for Item_Value_Dictionary_Header'Size use 32 + 32;

   for Item_Value_Dictionary_Header use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
      end record;

   type Item_Value_Dictionary_Header_Ptr is access Item_Value_Dictionary_Header;

   function To_Item_Value_Dictionary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Dictionary_Header_Ptr);

   function Get_Item_Value_Dictionary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Dictionary_Header_Ptr is (To_Item_Value_Dictionary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Dictionary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Dictionary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Dictionary_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Table

   type Item_Value_Table_Header is
      record
         Row_Count: Unsigned_32;
         Column_Count: Unsigned_32;
         Fields_Start: Unsigned_32;
         Row_Byte_Count: Unsigned_32;
      end record;

   for Item_Value_Table_Header'Size use 32 + 32 + 32 + 32;

   for Item_Value_Table_Header use
      record
         Row_Count at 0 range 0..31;
         Column_Count at 4 range 0..31;
         Fields_Start at 8 range 0..31;
         Row_Byte_Count at 12 range 0..31;
      end record;

   type Item_Value_Table_Header_Ptr is access Item_Value_Table_Header;

   function To_Item_Value_Table_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Table_Header_Ptr);

   function Get_Item_Value_Table_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Dictionary_Header_Ptr is (To_Item_Value_Table_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Table_Get_Row_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Table_Set_Row_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Table_Get_Column_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Table_Set_Column_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Table_Get_Fields_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   function Item_Value_Table_Get_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Table_Set_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Table_Get_Column_Descriptor_Start_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Table - Column Descriptor

   type Column_Descriptor is
      record
         Name_CRC: Unsigned_16;
         Name_Byte_Count: Unsigned_8;
         Field_Type: Item_Type;
         Name_Offset: Unsigned_32;
         Field_Offset: Unsigned_32;
         Field_Byte_Count: Unsigned_32;
      end record;

   for Column_Descriptor'Size use 16 + 8 + 8 + 32 + 32 + 32;

   for Column_Descriptor use
      record
         Name_CRC         at 0  range 0..15;
         Name_Byte_Count  at 2  range 0..7;
         Field_Type       at 3  range 0..7;
         Name_Offset      at 4  range 0..31;
         Field_Offset     at 8  range 0..31;
         Field_Byte_Count at 12 range 0..31;
      end record;

   type Column_Descriptor_Ptr is access Column_Descriptor;

   function To_Column_Descriptor_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Column_Descriptor_Ptr);

   function Column_Descriptor_Get_Name_CRC (S: Store; Descriptor_Offset: Unsigned_32) return CRC_16;

   procedure Column_Descriptor_Set_Name_CRC (S: Store; Descriptor_Offset: Unsigned_32; Value: CRC_16);

   function Column_Descriptor_Get_Name_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_8;

   procedure Column_Descriptor_Set_Name_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_8);

   function Column_Descriptor_Get_Field_Type (S: Store; Descriptor_Offset: Unsigned_32) return Item_Type;

   procedure Column_Descriptor_Set_Field_Type (S: Store; Descriptor_Offset: Unsigned_32; Value: Item_Type);

   function Column_Descriptor_Get_Name_Offset (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Name_Offset (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_32);

   function Column_Descriptor_Get_Field_Offset (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Field_Offset (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_32);

   function Column_Descriptor_Get_Field_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Field_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_32);


   -----------------------------------------------------------------------------
   -- Small Item Value: RGBA (Color)

   type Item_Value_Header_RGBA is
      record
         Red: Unsigned_8;
         Green: Unsigned_8;
         Blue: Unsigned_8;
         Alpha: Unsigned_8;
      end record;

   for Item_Value_Header_RGBA'Size use 32;

   for Item_Value_Header_RGBA use
      record
         Red   at 0 range 0..7;
         Green at 1 range 0..7;
         Blue  at 2 range 0..7;
         Alpha at 3 range 0..7;
      end record;

   type Item_Value_Header_RGBA_Ptr is access Item_Value_Header_RGBA;

   function To_Item_Value_Header_RGBA_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_RGBA_Ptr);


   -----------------------------------------------------------------------------
   -- Item Value: Font

   type Item_Value_Header_Font is
      record
         Size: IEEE_Float_32;
         Family_Name_Byte_Count: Unsigned_8;
         Font_Name_Byte_Count: Unsigned_8;
      end record;

   for Item_Value_Header_Font'Size use 32 + 8 + 8;

   for Item_Value_Header_Font use
      record
         Size at 0 range 0..31;
         Family_Name_Byte_Count at 4 range 0..7;
         Font_Name_Byte_count at 5 range 0..7;
      end record;

   type Item_Value_Header_Font_Ptr is access Item_Value_Header_Font;

   function To_Item_Value_Header_Font_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_Font_Ptr);

   function Item_Value_Font_Get_Size (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32;
   
   procedure Item_Value_Font_Set_Size (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32);
                                       
   function Item_Value_Font_Get_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Font_Set_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);
                                       
   function Item_Value_Font_Get_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Font_Set_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);
   
   -- Returns the offset of the first family name character relative to the start of the item
   --
   function Item_Value_Font_Get_Family_Name_Offset return Unsigned_32;
   
   -- Returns the offset of the first font name character relative to the start of the item
   --
   function Item_Value_Font_Get_Font_Name_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   
   
   -----------------------------------------------------------------------------
   -- Creates the layout for the requested type in the container at the requested offset.

   procedure Create_Layout
    (
     S: Store;
     At_Offset: Unsigned_32;
     Of_Type: Item_Type;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );


   -----------------------------------------------------------------------------
   -- Create a new array layout at the given offset

   function Create_Array_Layout
     (
      S: Store;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent;
      For_Element_Type: Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal;

   
   --================================================================================================================
   function Get_Value_Ptr (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Ptr is
   begin
      return S.Data (Get_Value_Offset (S, Item_Offset))'Access;
   end Get_Value_Ptr;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Bool (S: Store; Item_Offset: Unsigned_32) return Boolean is
   begin
      return Container.Get_Bool (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Bool;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Bool (S: Store; Item_Offset: Unsigned_32; Value: Boolean) is
   begin
      Container.Set_Bool (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Bool;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int8 (S: Store; Item_Offset: Unsigned_32) return Integer_8 is
   begin
      return Container.Get_Integer_8 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int8;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int8 (S: Store; Item_Offset: Unsigned_32; Value: Integer_8) is
   begin
      Container.Set_Integer_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int8;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt8 (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt8;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt8 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt8;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int16 (S: Store; Item_Offset: Unsigned_32) return Integer_16 is
   begin
      return Container.Get_Integer_16 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int16;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int16 (S: Store; Item_Offset: Unsigned_32; Value: Integer_16) is
   begin
      Container.Set_Integer_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int16;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt16 (S: Store; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt16;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt16 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt16;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int32 (S: Store; Item_Offset: Unsigned_32) return Integer_32 is
   begin
      return Container.Get_Integer_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int32;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int32 (S: Store; Item_Offset: Unsigned_32; Value: Integer_32) is
   begin
      Container.Set_Integer_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int32;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt32 (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt32;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt32 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt32;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Float32 (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32 is
   begin
      return Container.Get_Float_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Float32;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Float32 (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      Container.Set_Float_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Float32;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Color (S: Store; Item_Offset: Unsigned_32) return Color is
   begin
      return Color_Factory (Red   => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset),
                            Green => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1),
                            Blue  => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2),
                            Alpha => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3));
   end Small_Value_Get_Color;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Color (S: Store; Item_Offset: Unsigned_32; Value: Color) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Get_Red_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1, Get_Green_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2, Get_Blue_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3, Get_Alpha_Component (Value));
   end Small_Value_Set_Color;
   
   
   -----------------------------------------------------------------------------
   
   procedure Create_Layout
    (
     S: Store;
     At_Offset: Unsigned_32;
     Of_Type: Item_Type;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    ) is

      T: Item_Type renames Of_Type;
      O: Unsigned_32 renames At_Offset;
      N: Name_Field_Assistent renames With_Name;
      B: Unsigned_32 renames Using_Byte_Count;
      P: Unsigned_32 renames Parent_Offset;
      
      Item_Ptr: Item_Header_Ptr := To_Item_Header_Ptr (S.Data (At_Offset)'Access);
      
   begin

      if T = Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create illegal item type");
      end if;
      
      Item_Ptr.Type_Field := T;
      Item_Ptr.Options_Field := No_Item_Options;
      Item_Ptr.Flags_Field := No_Item_Flags;
      Item_Ptr.Name_Field_Byte_Count_Field := N.Field_Byte_Count;
      Item_Ptr.Parent_Offset_Field := P;
      Item_Ptr.Byte_Count_Field := B;
      
      Set_Name (Item_Ptr, N);
      
      case T is
         when Illegal => null; -- Cannot happen
         when Null_Type | Bool_Type | Int_8_Type | Int_16_Type | Int_32_Type | UInt_8_Type | UInt_16_Type | UInt_32_Type | Float_32_Type | RGBA_Type =>
            Small_Value_Set_UInt32 (S, O, 0);
         when Int_64_Type =>
            Container.Set_Integer_64 (S, Get_Value_Offset (S, At_Offset), 0);
         when UInt_64_Type =>
            Container.Set_Unsigned_64 (S, Get_Value_Offset (S, At_Offset), 0);
         when Float_64_Type =>
            Container.Set_Float_64 (S, Get_Value_Offset (S, At_Offset), 0.0);
         when String_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
         when Crc_String_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Binary_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
         when Crc_Binary_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Array_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 8, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 12, 0);
         when Dictionary_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Sequence_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Table_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 8, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 12, 0);
         when UUID_Type =>
            Container.Set_Unsigned_64 (S, Get_Value_Offset (S, At_Offset), 0);
         when Font_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
      end case;

   end Create_Layout;

   
   -- Create a new array layout at the given offset
   --
   function Create_Array_Layout
     (
      S: Store;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent;
      For_Element_Type: Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal is
  
      Index: Unsigned_32 renames At_Offset;
      Name: Name_Field_Assistent renames With_Name;
      Element_Type: Item_Type renames For_Element_Type;
      Element_Count: Unsigned_32 renames Max_Element_Count;
      
      Element_Byte_Count: Unsigned_32 := Using_Element_Byte_Count;

   begin
      
      -- Check input parameters for validity
      --
      if Element_Byte_Count = 0 then
         Ada.Exceptions.Raise_Exception (BRBON.Byte_Count_Error'Identity, "Cannot create array with 0 as element byte count");
      end if;
      --
      if Element_Count = 0 then
         Ada.Exceptions.Raise_Exception (BRBON.Array_Error'Identity, "Cannot create array with 0 elements");
      end if;
      
      
      -- Calculate the actual element byte count
      -- (all elements must be properly aligned according to their inherent byte count)
      --
      case Element_Type is
         when Illegal => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot create an array of Illegal elements");
         when Null_Type => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot create an array of Null_Type elements");
         when Bool_Type | UInt_8_Type | Int_8_Type => null;
         when UInt_16_Type | Int_16_Type =>
            if Element_Byte_Count mod 2 = 1 then
               Element_Byte_Count := Element_Byte_Count + 1;
            end if;
         when UInt_32_Type | Int_32_Type | Float_32_Type =>
            if Element_Byte_Count mod 2 = 1 then
               Element_Byte_Count := Element_Byte_Count + 1;
            end if;
            if Element_Byte_Count mod 4 = 2 then
               Element_Byte_Count := Element_Byte_Count + 2;
            end if;
         when UInt_64_Type | Int_64_Type | Float_64_Type | UUID_Type | RGBA_Type =>
            Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
         when Sequence_Type | Dictionary_Type =>
            if Element_Byte_Count < Item_Header_Byte_Count then
               Element_Byte_Count := Item_Header_Byte_Count;
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Array_Type =>
            if Element_Byte_Count < Item_Header_Byte_Count + Item_Overhead_Byte_Count (Array_Type) then
               Element_Byte_Count := Item_Header_Byte_Count + Item_Overhead_Byte_Count (Array_Type);
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Table_Type =>
            if Element_Byte_Count < Item_Header_Byte_Count + Item_Overhead_Byte_Count (Table_Type) then
               Element_Byte_Count := Item_Header_Byte_Count + Item_Overhead_Byte_Count (Table_Type);
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when String_Type | Crc_String_Type | Binary_Type | Crc_Binary_Type | Font_Type =>
            Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
      end case;

         
      -- Create basic item layout
      --
      Create_Layout (S                => S,
                     At_Offset        => Index,
                     Of_Type          => Array_Type,
                     With_Name        => Name,
                     Using_Byte_Count => Item_Overhead_Byte_Count (Array_Type) + Element_Byte_Count * Element_Count,
                     Parent_Offset    => 0);
      
      -- Initialize the array specific parameters
      --
      declare
         Array_Header_Ptr: Item_Value_Header_Array_Ptr := To_Item_Value_Header_Array_Ptr (Get_Value_Ptr (S, At_Offset));
      begin
         Array_Header_Ptr.Element_Type := Element_Type;
         Array_Header_Ptr.Element_Byte_Count := Element_Byte_Count;
      end;
      
      return Portal_Factory (S, Index);
      
   end Create_Array_Layout;
   
   
   -- Get_Value_Offset
   --
   Use_Small_Value_LUT: constant Array (Item_Type) of Boolean :=
     -- Ill  Null  Bool  i8    i16   i32   i64    u8    u16   u32   u64    f32   f64    str    cstr   bin    cbin   arr    dict   seq    tab    uuid   rgb   font
     (False, True, True, True, True, True, False, True, True, True, False, True, False, False, False, False, False, False, False, False, False, False, True, False);
   --                                         
   function Get_Value_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      
      Item_Ptr : Item_Header_Ptr := To_Item_Header_Ptr (S.Data (Item_Offset)'Access);
      T: Item_Type := Item_Ptr.Type_Field;
   
   begin
      
      if T = Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Store contains illegal item type (16#00#)");
      end if;

      if Use_Small_Value_LUT (T) then
         return Item_Offset + Small_Value_Offset;
      else
         return Item_Offset + Item_Header_Byte_Count + Unsigned_32 (Item_Ptr.Name_Field_Byte_Count_Field);
      end if;
   
   end Get_Value_Offset;
   
   
   
   function Get_Name_CRC (IPtr: Item_Header_Ptr) return Unsigned_16 is
   begin
      return To_Item_Name_Field_Ptr (IPtr.Next_Byte'Access).CRC;
   end Get_Name_CRC;
   
   
   function Get_Name_Byte_Count (IPtr: Item_Header_Ptr) return Unsigned_8 is
   begin
      return To_Item_Name_Field_Ptr (IPtr.Next_Byte'Access).Byte_Count;
   end Get_Name_Byte_Count;
   

   function Get_Name_String (IPtr: Item_Header_Ptr) return String is
      Length: Integer := Integer (Get_Name_Byte_Count (IPtr));
      ASCII_Ptr: Unsigned_8_Ptr := To_Item_Name_Field_Ptr (IPtr.Next_Byte'Access).ASCII_Code'Access;
      subtype StrT is String (1 .. Length);
      type StrTPtr is access StrT;
      function ToStrTPtr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, StrTPtr);
   begin
      return ToStrTPtr (ASCII_Ptr).all;
   end Get_Name_String;
   
   
   function Get_Name_Quick_Check_Value (IPtr: Item_Header_Ptr) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (IPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset);
   end Get_Name_Quick_Check_Value;
     
   procedure Set_Name (IPtr: Item_Header_Ptr; Value: Name_Field_Assistent.Instance) is
   begin
      if Value.Name_Field_Byte_Count > 0 then
         Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Byte_Count_Offset, Value.Name_Field_Byte_Count);
         Container.Set_Unsigned_16 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset, Value.CRC);
         Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Byte_Count_Offset, Unsigned_8 (Value.Ascii_Byte_Count));
         Container.Set_Unsigned_8_Array (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Code_Offset, Value.Ascii_Code (1 .. Unsigned_32 (Value.Ascii_Byte_Count)));
      end if;
   end Set_Name;
   
   
   procedure Set_Name_CRC (IPtr: Item_Header_Ptr; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (CPtr, Item_Offset + Name_Field_CRC_Offset, Value);
   end Set_Name_CRC;

   
   procedure Set_Name_Byte_Count (IPtr: Item_Header_Ptr; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Value);
   end Set_Name_Byte_Count;

   
   procedure Set_Name_String (IPtr: Item_Header_Ptr; Value: String) is
   begin
      Container.Set_String (CPtr, Item_Offset + Name_Field_ASCII_Code_Offset, Value);
   end Set_Name_String;
      
end BRBON.Item_Package;

with Crc_Package; use Crc_Package;

package body Item is


   procedure Assign_Item_Type (I: Item_Access'Class; Value: BR_Item_Type) is
   begin
      I.Storage.Set_Item_Type (I.Offset + Item_Header_Offset_Type, Value);
   end Assign_Item_Type;


   procedure Assign_Item_Options (I: Item_Access'Class; Value: BR_Item_Options) is
   begin
      I.Storage.Set_Item_Options (I.Offset + Item_Header_Offset_Options, Value);
   end Assign_Item_Options;


   procedure Assign_Item_Flags (I: Item_Access'Class; Value: BR_Item_Flags) is
   begin
      I.Storage.Set_Item_Flags (I.Offset + Item_Header_Offset_Flags, Value);
   end Assign_Item_Flags;


   procedure Assign_Item_Name_Field_Byte_Count (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Offset + Item_Name_Field_Offset_Byte_Count, Value);
   end Assign_Item_Name_Field_Byte_Count;


   procedure Assign_Item_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Offset + Item_Name_Field_Offset_Byte_Count, Value);
   end Assign_Item_Byte_Count;


   procedure Assign_Item_Parent_Offset (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Offset + Item_Header_Offset_Parent_Offset, Value);
   end Assign_Item_Parent_Offset;


   procedure Assign_Item_Name (I: Item_Access'Class; Value: Item_Name_Assistent) is
   begin
      I.Storage.Set_Unsigned_8 (I.Offset + Item_Header_Offset_Name_Field_Byte_Count, Value.Name_Field_Byte_Count);
      I.Storage.Set_Unsigned_16 (I.Offset + Item_Name_Field_Offset_CRC_16, Value.CRC_16);
      I.Storage.Set_Unsigned_8 (I.Offset + Item_Name_Field_Offset_Byte_Count, Value.Ascii_Code_Count);
      I.Storage.Set_Unsigned_8_Array (I.Offset + Item_Name_Field_Offset_ASCII_Code, Value.Ascii_Code);
   end Assign_Item_Name;


   -- ===================
   -- BR_Null
   -- ===================

   procedure Create_Null_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Null); Parent_Offset: Unsigned_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Null);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

   end Create_Null_Item;


   -- ====================
   -- BR_Bool
   -- ====================

   procedure Assign_Boolean_Value (I: Item_Access'Class; Value: Boolean) is
   begin
      I.Storage.Set_Bool (I.Small_Value_Offset, Value);
   end Assign_Boolean_Value;

   procedure Create_Boolean_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Bool); Parent_Offset: Unsigned_32 := 0; Value: Boolean := False) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Bool);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Boolean_Value (Value);

   end Create_Boolean_Item;


   -- =======================
   -- BR_Int8
   -- =======================

   procedure Assign_Integer_8_Value (I: Item_Access'Class; Value: Integer_8) is
   begin
      I.Storage.Set_Integer_8 (I.Small_Value_Offset, Value);
   end Assign_Integer_8_Value;

   procedure Create_Integer_8_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int8); Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Int8);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Integer_8_Value (Value);

   end Create_Integer_8_Item;


   -- =======================
   -- BR_Int16
   -- =======================

   procedure Assign_Integer_16_Value (I: Item_Access'Class; Value: Integer_16) is
   begin
      I.Storage.Set_Integer_16 (I.Small_Value_Offset, Value);
   end Assign_Integer_16_Value;

   procedure Create_Integer_16_Item (I: Item_Access'Class;  Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int16); Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Int16);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Integer_16_Value (Value);

   end Create_Integer_16_Item;


   -- ========================
   -- BR_Int32
   -- ========================

   procedure Assign_Integer_32_Value (I: Item_Access'Class; Value: Integer_32) is
   begin
      I.Storage.Set_Integer_32 (I.Small_Value_Offset, Value);
   end Assign_Integer_32_Value;

   procedure Create_Integer_32_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int32); Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Int32);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);

      I.Assign_Item_Name (A);

      I.Assign_Integer_32_Value (Value);

   end Create_Integer_32_Item;


   -- ======================
   -- BR_Int64
   -- ======================

   procedure Assign_Integer_64_Value (I: Item_Access'Class; Value: Integer_64) is
   begin
      I.Storage.Set_Integer_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_Integer_64_Value;

   procedure Create_Integer_64_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int64); Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Int64);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Integer_64_Value (Value);

   end Create_Integer_64_Item;


   -- =======================
   -- BR_UInt8
   -- =======================

   procedure Assign_Unsigned_8_Value (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Unsigned_8_Value;

   procedure Create_Unsigned_8_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt8); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_UInt8);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Unsigned_8_Value (Value);

   end Create_Unsigned_8_Item;


   -- ======================
   -- BR_UInt16
   -- ======================

   procedure Assign_Unsigned_16_Value (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Unsigned_16_Value;

   procedure Create_Unsigned_16_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt16); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_UInt16);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Unsigned_16_Value (Value);

   end Create_Unsigned_16_Item;


   -- ======================
   -- BR_UInt32
   -- ======================

   procedure Assign_Unsigned_32_Value (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Unsigned_32_Value;

   procedure Create_Unsigned_32_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt32); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_UInt32);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);

      I.Assign_Item_Name (A);

      I.Assign_Unsigned_32_Value (Value);

   end Create_Unsigned_32_Item;


   -- =======================
   -- BR_UInt64
   -- =======================

   procedure Assign_Unsigned_64_Value (I: Item_Access'Class; Value: Unsigned_64) is
   begin
      I.Storage.Set_Unsigned_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_Unsigned_64_Value;

   procedure Create_Unsigned_64_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt64); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_UInt64);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Unsigned_64_Value (Value);

   end Create_Unsigned_64_Item;


   -- ======================
   -- BR_Float32
   -- ======================

   procedure Assign_Float_32_Value (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Float_32_Value;

   procedure Create_Float_32_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Float32); Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Float32);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);

      I.Assign_Item_Name (A);

      I.Assign_Float_32_Value (Value);

   end Create_Float_32_Item;


   -- ====================
   -- BR_Float64
   -- ====================

   procedure Assign_Float_64_Value (I: Item_Access'Class; Value: IEEE_Float_64) is
   begin
      I.Storage.Set_Float_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_Float_64_Value;

   procedure Create_Float_64_Item (I: Item_Access'Class;  Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Float64); Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Float64);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Float_64_Value (Value);

   end Create_Float_64_Item;


   -- ======================
   -- BR_String
   -- ======================

   procedure Assign_String_Value (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + String_Value_Offset_Byte_Count, Value'Length);
      I.Storage.Set_String (I.Value_Offset + String_Value_Offset_UTF8_Code, Value);
   end Assign_String_Value;

   procedure Create_String_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_String); Parent_Offset: Unsigned_32 := 0; Value: String := "") is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_String);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_String_Value (Value);

   end Create_String_Item;


   -- ======================
   -- BR_CRC_String
   -- ======================

   procedure Assign_CRC_String_Value (I: Item_Access'Class; Value: String) is
      CRC: Unsigned_32 := Calculate_CRC_32 (Value);
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_String_Value_Offset_CRC, CRC);
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_String_Value_Offset_Byte_Count, Value'Length);
      I.Storage.Set_String (I.Value_Offset + CRC_String_Value_Offset_UTF8_Code, Value);
   end Assign_CRC_String_Value;

   procedure Create_CRC_String_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_CRC_String); Parent_Offset: Unsigned_32 := 0; Value: String := "") is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_CRC_String);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_CRC_String_Value (Value);

   end Create_CRC_String_Item;


   -- ======================
   -- BR_Binary
   -- ======================

   procedure Assign_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Binary_Value_Offset_Byte_Count, Value'Length);
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + Binary_Value_Offset_Bytes, Value);
   end Assign_Binary_Value;

   procedure Create_Binary_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Binary); Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Binary);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_Binary_Value (Value);

   end Create_Binary_Item;


   -- =====================
   -- BR_CRC_Binary
   -- =====================

   procedure Assign_CRC_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
      CRC: Unsigned_32 := Calculate_CRC_32 (Value);
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_Offset_CRC, CRC);
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_Offset_Byte_Count, Value'Length);
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + CRC_Binary_Value_Offset_Bytes, Value);
   end Assign_CRC_Binary_Value;

   procedure Create_CRC_Binary_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_CRC_Binary); Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_CRC_Binary);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Assign_CRC_Binary_Value (Value);

   end Create_CRC_Binary_Item;


   -- ====================
   -- Array
   -- ====================

   procedure Set_Array_Element_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Array_Value_Offset_Element_Count, Value);
   end Set_Array_Element_Count;

   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Array_Value_Offset_Element_Byte_Count, Value);
   end Set_Array_Element_Byte_Count;

   procedure Create_Array_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Array); Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Array);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Storage.Set_Item_Type (I.Value_Offset + Array_Value_Offset_Element_Type, Element_Type);
      I.Set_Array_Element_Count (0);
      I.Set_Array_Element_Byte_Count (Element_Byte_Count);

      I.Storage.Set_Unsigned_32 (I.Value_Offset + Array_Value_Offset_Reserved_32, 0);
      I.Storage.Set_Unsigned_16 (I.Value_Offset + Array_Value_Offset_Reserved_16, 0);
      I.Storage.Set_Unsigned_8 (I.Value_Offset + Array_Value_Offset_Reserved_8, 0);

   end Create_Array_Item;


   -- =================
   -- BR_Dictionary
   -- =================

   procedure Set_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Dictionary_Value_Offset_Count, Value);
   end Set_Dictionary_Item_Count;

   procedure Create_Dictionary_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Dictionary); Parent_Offset: Unsigned_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Dictionary);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Set_Dictionary_Item_Count (0);

      I.Storage.Set_Unsigned_32 (I.Value_Offset + Dictionary_Value_Offset_Reserved_32, 0);

   end Create_Dictionary_Item;


   -- =================
   -- BR_Sequence
   -- =================

   procedure Set_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Sequence_Value_Offset_Count, Value);
   end Set_Sequence_Item_Count;

   procedure Create_Sequence_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Sequence); Parent_Offset: Unsigned_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Sequence);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Set_Sequence_Item_Count (0);

      I.Storage.Set_Unsigned_32 (I.Value_Offset + Sequence_Value_Offset_Reserved_32, 0);

   end Create_Sequence_Item;


   -- =================
   -- Table
   -- =================

   procedure Set_Table_Row_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Row_Count, Value);
   end Set_Table_Row_Count;

   procedure Set_Table_Column_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Column_Count, Value);
   end Set_Table_Column_Count;

   procedure Set_Table_Fields_Offset (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Fields_Offset, Value);
   end Set_Table_Fields_Offset;

   procedure Set_Table_Row_Byte_Count  (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Row_Byte_Count, Value);
   end Set_Table_Row_Byte_Count;

   procedure Set_Column_Name_CRC (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Name_CRC_16, Value);
   end Set_Column_Name_CRC;

   procedure Set_Column_Name_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Name_Field_Byte_Count, Value);
   end Set_Column_Name_Field_Byte_Count;

   procedure Set_Column_Type (I: Item_Access'Class; Column_Index: Unsigned_32; Value: BR_Item_Type) is
   begin
      I.Storage.Set_Item_Type (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Field_Type, Value);
   end Set_Column_Type;

   procedure Set_Column_Name_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Offset_Name_Field_Offset, Value);
   end Set_Column_Name_Field_Offset;

   procedure Set_Column_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Offset_Field_Offset, Value);
   end Set_Column_Field_Offset;

   procedure Set_Column_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Offset_Field_Byte_Count, Value);
   end Set_Column_Field_Byte_Count;

   procedure Set_Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Column_Name_Field_Offset (Column_index) + Column_Name_Field_Offset_ASCII_Byte_Count, Value);
   end Set_Column_Name_Byte_Count;

   procedure Set_Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32; Value: String) is
   begin
      I.Storage.Set_String (I.Column_Name_Field_Offset (Column_Index) + Column_Name_Field_Offset_ASCII_Code, Value);
   end Set_Column_Name;


   procedure Create_Table_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Table); Parent_Offset: Unsigned_32 := 0; Column_Specifications: Array_Of_Table_Column_Specification) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Table);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

   end Create_Table_Item;


   -- ===================
   -- BR_UUID
   -- ===================

   procedure Assign_UUID_Value (I: Item_Access'Class; Value: UUID) is
   begin
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset, Array_Of_Unsigned_8 (Value));
   end Assign_UUID_Value;

   procedure Create_UUID_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UUID); Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID) is
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_UUID);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (Assistent);

      I.Assign_UUID_Value (Value);

   end Create_UUID_Item;


   -- =================
   -- BR_Color
   -- =================

   procedure Set_Red (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Red, Value);
   end Set_Red;

   procedure Set_Green (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Green, Value);
   end Set_Green;

   procedure Set_Blue (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Blue, Value);
   end Set_Blue;

   procedure Set_Alpha (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Alpha, Value);
   end Set_Alpha;

   function Color_Value (I: Item_Access'Class) return Color is
   begin
      return To_Color (I.Storage.Get_Unsigned_32 (I.Small_Value_Offset));
   end Color_Value;

   procedure Assign_Color_Value (I: Item_Access'Class; Value: Color) is
   begin
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, To_Unsigned_32 (Value));
   end Assign_Color_Value;

   procedure Create_Color_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Color); Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black) is
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Color);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (Assistent);

      I.Assign_Color_Value (Value);

   end Create_Color_Item;


   -- ====================
   -- BR_Font
   -- ====================

   procedure Set_Points (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (I.Value_Offset + Font_Value_Offset_Points, Value);
   end Set_Points;

   procedure Set_Family_Name_Byte_Count (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Value_Offset + Font_Value_Offset_Family_Name_Byte_Count, Value);
   end Set_Family_Name_Byte_Count;

   procedure Set_Font_Name_Byte_Count (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Value_Offset + Font_Value_Offset_Font_Name_Byte_Count, Value);
   end Set_Font_Name_Byte_Count;

   procedure Set_Family_Name (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_String (I.Value_Offset + Font_Value_Offset_Family_Name_String, Value);
   end Set_Family_Name;

   procedure Set_Font_Name (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_String (I.Value_Offset + Font_Value_Offset_Family_Name_String + Unsigned_32 (I.Family_Name_Byte_Count), Value);
   end Set_Font_Name;

   function Font_Value (I: Item_Access'Class) return Font is
      F: Font;
   begin
      F.Points := I.Points;
      F.Family := Bounded_String_256.To_Bounded_String (I.Family_Name);
      F.Name := Bounded_String_256.To_Bounded_String (I.Font_Name);
      return F;
   end Font_Value;

   procedure Set_Font_Value (I: Item_Access'Class; Value: Font) is
   begin

      I.Set_Points (Value.Points);
      I.Set_Family_Name_Byte_Count (Unsigned_8 (Bounded_String_256.Length (Value.Family)));
      I.Set_Font_Name_Byte_Count (Unsigned_8 (Bounded_String_256.Length (Value.Name)));
      I.Set_Family_Name (Bounded_String_256.To_String (Value.Family));
      I.Set_Font_Name (Bounded_String_256.To_String (Value.Name));

   end Set_Font_Value;

   procedure Create_Font_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Font); Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font) is
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Font);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (Assistent);

      I.Set_Font_Value (Value);

   end Create_Font_Item;


   -- =========================
   -- Item Name Assistent
   -- =========================

   function Create_Item_Name_Assistent (S: BR_Item_Name) return Item_Name_Assistent is

      Name: String := BR_Item_Name_Bounded_String.To_String (S);
      Assistent: Item_Name_Assistent (Name'Length);
      Index: Unsigned_32 := 1;

      type qckt is
         record
            p12: Unsigned_16;
            p3: Unsigned_8;
            p4: Unsigned_8;
         end record;
      for qckt use
         record
            p12 at 0 range 0..15;
            p3 at 2 range 0..7;
            p4 at 3 range 0..7;
         end record;
      for qckt'Size use 32;

      function To_Unsigned_32 is new Ada.Unchecked_Conversion (qckt, Unsigned_32);

      Quick_Check: qckt;

   begin

      Assistent.CRC_16 := Calculate_CRC_16 (Name);

      for C of Name loop
         Assistent.Ascii_Code(Index) := Character'Pos (C);
         Index := Index + 1;
      end loop;

      Quick_Check := (Assistent.CRC_16, Assistent.Ascii_Code_Count, Assistent.Ascii_Code(1));

      Assistent.Quick_Check := To_Unsigned_32 (Quick_Check);

      if Assistent.Ascii_Code_Count > 0 then
         Assistent.Name_Field_Byte_Count := Round_Up_To_Nearest_Multiple_Of_8 (3 + Assistent.Ascii_Code_Count);
      else
         Assistent.Name_Field_Byte_Count := 0;
      end if;

      return Assistent;

   end Create_Item_Name_Assistent;



   -- =======================
   -- Item Access
   -- =======================

   function Create_Item_Access (S: Binary_Store_Ptr; O: Unsigned_32) return Item_Access is
   begin
      return (S, O);
   end Create_Item_Access;


end Item;

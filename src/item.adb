with Crc_Package; use Crc_Package;

package body Item is


   procedure Assign_Item_Type (I: Item_Access'Class; Value: BR_Item_Type) is
   begin
      I.Storage.Set_Item_Type (I.Offset + Item_Type_Offset, Value);
   end Assign_Item_Type;


   procedure Assign_Item_Options (I: Item_Access'Class; Value: BR_Item_Options) is
   begin
      I.Storage.Set_Item_Type (I.Offset + Item_Options_Offset, Value);
   end Assign_Item_Options;


   procedure Assign_Item_Flags (I: Item_Access'Class; Value: BR_Item_Flags) is
   begin
      I.Storage.Set_Item_Flags (I.Offset + Item_Flags_Offset, Value);
   end Assign_Item_Flags;


   procedure Assign_Item_Name_Field_Byte_Count (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count_Offset, Value);
   end Assign_Item_Name_Field_Byte_Count;


   procedure Assign_Item_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Offset + Item_Name_Byte_Count_Offset, Value);
   end Assign_Item_Byte_Count;


   procedure Assign_Item_Parent_Offset (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Offset + Item_Parent_Offset_Offset, Value);
   end Assign_Item_Parent_Offset;



   procedure Assign_Item_Name (I: Item_Access'Class; Value: BR_Item_Name) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Value);
   begin
      I.Storage.Set_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count_Offset, A.Name_Field_Byte_Count);
      I.Storage.Set_Unsigned_16 (I.Offset + Item_Name_CRC_16_Offset, A.CRC_16);
      I.Storage.Set_Unsigned_8 (I.Offset + Item_Name_Byte_Count, A.Ascii_Code_Count);
      I.Storage.Set_Unsigned_8_Array (I.Offset + Item_Name_ASCII_Code_Offset, A.Ascii_Code);
   end Assign_Item_Name;


   function Item_Name (I: Item_Access'Class) return String is
      Length: Unsigned_8 := I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Byte_Count_Offset);
   begin
      return I.Storage.Get_String (I.Offset + Item_Name_ASCII_Code_Offset, Length);
   end Item_Name;


   -- ===================
   -- BR_Null
   -- ===================

   procedure Create_Null_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Null); Parent_Offset: Unsigned_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Null);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Value_Offset, 0);

      I.Assign_Item_Name (A);

   end Create_Null_Item;


   -- ====================
   -- BR_Bool
   -- ====================

   function Boolean_Value (I: Item_Access'Class) return Boolean is
   begin
      return I.Storage.Get_Bool (I.Small_Value_Offset);
   end Boolean_Value;

   procedure Assign_Boolean_Value (I: Item_Access'Class; Value: Boolean) is
   begin
      I.Storage.Set_Bool (I.Small_Value_Offset, Value);
   end Assign_Boolean_Value;

   procedure Create_Boolean_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False) is
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

   function Integer_8_Value (I: Item_Access'Class) return Integer_8 is
   begin
      return I.Storage.Get_Integer_8 (I.Small_Value_Offset);
   end Integer_8_Value;

   procedure Assign_Integer_8_Value (I: Item_Access'Class; Value: Integer_8) is
   begin
      I.Storage.Set_Integer_8 (I.Small_Value_Offset, Value);
   end Assign_Integer_8_Value;

   procedure Create_Integer_8_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0) is
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

   function Integer_16_Value (I: Item_Access'Class) return Integer_16 is
   begin
      return I.Storage.Get_Integer_16 (I.Small_Value_Offset);
   end Integer_16_Value;

   procedure Assign_Integer_16_Value (I: Item_Access'Class; Value: Integer_16) is
   begin
      I.Storage.Set_Integer_16 (I.Small_Value_Offset, Value);
   end Assign_Integer_16_Value;

   procedure Create_Integer_16_Item (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0) is
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

   function Integer_32_Value  (I: Item_Access'Class) return Integer_32 is
   begin
      return I.Storage.Get_Integer_32 (I.Small_Value_Offset);
   end Integer_32_Value;

   procedure Assign_Integer_32_Value (I: Item_Access'Class; Value: Integer_32) is
   begin
      I.Storage.Set_Integer_32 (I.Small_Value_Offset, Value);
   end Assign_Integer_32_Value;

   procedure Create_Integer_32_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0) is
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

   function Integer_64_Value (I: Item_Access'Class) return Integer_64 is
   begin
      return I.Storage.Get_Integer_64 (I.Value_Offset);
   end Integer_64_Value;

   procedure Assign_Integer_64_Value (I: Item_Access'Class; Value: Integer_64) is
   begin
      I.Storage.Set_Integer_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_Integer_64_Value;

   procedure Create_Integer_64_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0) is
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

   function Unsigned_8_Value (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Small_Value_Offset);
   end Unsigned_8_Value;

   procedure Assign_Unsigned_8_Value (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Unsigned_8_Value;

   procedure Create_Unsigned_8_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0) is
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

   function Unsigned_16_Value (I: Item_Access'Class) return Unsigned_16 is
   begin
      return I.Storage.Get_Unsigned_16 (I.Small_Value_Offset);
   end Unsigned_16_Value;

   procedure Assign_Unsigned_16_Value (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Unsigned_16_Value;

   procedure Create_Unsigned_16_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0) is
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

   function Unsigned_32_Value (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Small_Value_Offset);
   end Unsigned_32_Value;

   procedure Assign_Unsigned_32_Value (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Unsigned_32_Value;

   procedure Create_Unsigned_32_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0) is
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

   function Unsigned_64_Value (I: Item_Access'Class) return Unsigned_64 is
   begin
      return I.Storage.Get_Unsigned_64 (I.Value_Offset);
   end Unsigned_64_Value;

   procedure Assign_Unsigned_64_Value (I: Item_Access'Class; Value: Unsigned_64) is
   begin
      I.Storage.Set_Unsigned_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_Unsigned_64_Value;

   procedure Create_Unsigned_64_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0) is
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

   function Float_32_Value (I: Item_Access'Class) return IEEE_Float_32 is
   begin
      return I.Storage.Get_Float_32 (I.Small_Value_Offset);
   end Float_32_Value;

   procedure Assign_Float_32_Value (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_Float_32_Value;

   procedure Create_Float_32_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0) is
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

   function Float_64_Value (I: Item_Access'Class) return IEEE_Float_64 is
   begin
      return I.Storage.Get_Float_64 (I.Value_Offset);
   end Float_64_Value;

   procedure Assign_Float_64_Value (I: Item_Access'Class; Value: IEEE_Float_64) is
   begin
      I.Storage.Set_Float_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_Float_64_Value;

   procedure Create_Float_64_Item (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0) is
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

   function String_Value (I: Item_Access'Class) return String is
      Length: Unsigned_32 := I.String_Byte_Count;
   begin
      return I.Storage.Get_String (I.Value_Offset + String_Value_UTF8_Code_Offset, Length);
   end String_Value;

   function String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + String_Value_Byte_Count_Offset);
   end String_Byte_Count;

   procedure Assign_String_Value (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + String_Value_Byte_Count_Offset, Value'Length);
      I.Storage.Set_String (I.Value_Offset + String_Value_UTF8_Code_Offset, Value);
   end Assign_String_Value;

   procedure Create_String_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is
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

   function CRC_String_CRC (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_String_Value_CRC_Offset);
   end CRC_String_CRC;

   function CRC_String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_String_Value_Byte_Count_Offset);
   end CRC_String_Byte_Count;

   function CRC_String_Value (I: Item_Access'Class) return String is
      Length: Unsigned_32 := I.CRC_String_Byte_Count;
   begin
      return I.Storage.Get_String (I.Value_Offset + CRC_String_Value_UTF8_Code_Offset, Length);
   end CRC_String_Value;

   procedure Assign_CRC_String_Value (I: Item_Access'Class; Value: String) is
      CRC: Unsigned_32 := Calculate_CRC_32 (Value);
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_String_Value_CRC_Offset, CRC);
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_String_Value_Byte_Count_Offset, Value'Length);
      I.Storage.Set_String (I.Value_Offset + CRC_String_Value_UTF8_Code_Offset, Value);
   end Assign_CRC_String_Value;

   procedure Create_CRC_String_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is
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

   function Binary_Value (I: Item_Access'Class) return Array_Of_Unsigned_8 is
      Length: Unsigned_32 := I.Binary_Byte_Count;
   begin
      return I.Storage.Get_Unsigned_8_Array (I.Value_Offset + Binary_Value_Bytes_Offset, Length);
   end Binary_Value;

   function Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Binary_Value_Byte_Count_Offset);
   end Binary_Byte_Count;

   procedure Assign_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Binary_Value_Byte_Count_Offset, Value'Length);
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + Binary_Value_UTF8_Code_Offset, Value);
   end Assign_Binary_Value;

   procedure Create_Binary_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
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


   function CRC_Binary_CRC (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_CRC_Offset);
   end CRC_Binary_CRC;

   function CRC_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_Byte_Count_Offset);
   end CRC_Binary_Byte_Count;

   function CRC_Binary_Value (I: Item_Access'Class) return Array_Of_Unsigned_8 is
      Length: Unsigned_32 := I.CRC_Binary_Byte_Count;
   begin
      return I.Storage.Get_String (I.Value_Offset + CRC_Binary_Value_Bytes_Offset, Length);
   end CRC_Binary_Value;

   procedure Assign_CRC_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
      CRC: Unsigned_32 := Calculate_CRC_32 (Value);
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_CRC_Offset, CRC);
      I.Storage.Set_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_Byte_Count_Offset, Value'Length);
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + CRC_Binary_Value_Bytes_Offset, Value);
   end Assign_CRC_Binary_Value;

   procedure Create_CRC_Binary_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
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

   function Array_Element_Type (I: Item_Access'Class) return BR_Item_Type is
   begin
      return I.Storage.Get_Item_Type (I.Value_Offset + Array_Value_Element_Type_Offset);
   end Array_Element_Type;

   function Array_Element_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Array_Value_Element_Count_Offset);
   end Array_Element_Count;

   function Array_Element_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Array_Value_Element_Byte_Count_Offset);
   end Array_Element_Byte_Count;

   function Array_Element_Offset (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32 is
   begin
      return I.Value_Offset + Array_Value_Element_Base_Offset + (Index - 1) * Array_Element_Byte_Count;
   end Array_Element_Offset;

   procedure Set_Array_Element_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Array_Value_Element_Count_Offset, Value);
   end Set_Array_Element_Count;

   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Array_Value_Element_Byte_Count_Offset, Value);
   end Set_Array_Element_Byte_Count;

   procedure Create_Array_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Array);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);

      I.Assign_Item_Name (A);

      I.Storage.Set_Item_Type (I.Value_Offset + Array_Value_Element_Type_Offset, Element_Type);
      I.Set_Array_Element_Count (0);
      I.Set_Array_Element_Byte_Count (Element_Byte_Count);

      I.Storage.Set_Unsigned_32 (I.Value_Offset + Array_Value_Reserved_32_Offset, 0);
      I.Storage.Set_Unsigned_16 (I.Value_Offset + Array_Value_Reserved_16_Offset, 0);
      I.Storage.Set_Unsigned_8 (I.Value_Offset + Array_Value_Reserved_8_Offset, 0);

   end Create_Array_Item;


   -- =================
   -- BR_Dictionary
   -- =================

   function Dictionary_Item_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Dictionary_Value_Count_Offset);
   end Dictionary_Item_Count;

   procedure Set_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Dictionary_Value_Count_Offset, Value);
   end Set_Dictionary_Item_Count;

   procedure Create_Dictionary_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
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

      I.Storage.Set_Unsigned_32 (I.Value_Offset + Dictionary_Value_Reserved_32_Offset, 0);

   end Create_Dictionary_Item;


   -- =================
   -- BR_Sequence
   -- =================

   function Sequence_Item_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Sequence_Value_Count_Offset);
   end Sequence_Item_Count;

   procedure Set_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Sequence_Value_Count_Offset, Value);
   end Set_Sequence_Item_Count;

   procedure Create_Sequence_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
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

      I.Storage.Set_Unsigned_32 (I.Value_Offset + Sequence_Value_Reserved_32_Offset, 0);

   end Create_Sequence_Item;


   -- =================
   -- Table
   -- =================

   function Table_Row_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Row_Count_Offset);
   end Table_Row_Count;

   procedure Set_Table_Row_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Row_Count_Offset, Value);
   end Set_Table_row_Count;


   function Table_Column_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Column_Count_Offset);
   end Table_Column_Count;

   procedure Set_Table_Column_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Column_Count_Offset, Value);
   end Set_Table_Column_Count;


   function Table_Fields_Offset (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Fields_Offset_Offset);
   end Table_Fields_Offset;

   procedure Set_Table_Fields_Offset (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Fields_Offset_Offset, Value);
   end Set_Table_Fields_Offset;


   function Table_Row_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Row_Byte_Count_Offset);
   end Table_Row_Byte_Count;

   procedure Set_Table_Row_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset + Table_Value_Row_Byte_Count_Offset, Value);
   end Set_Table_Row_Byte_Count;


   function Table_Column_Descriptor_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
   begin
      return I.Value_Offset + Table_Value_Column_Descriptor_Base_Offset + (Column_Index - 1) * Column_Descriptor_Byte_Count;
   end Table_Column_Descriptor_Offset;


   function Column_Name_CRC (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_16 is
   begin
      return I.Storage.Get_Unsigned_8 (Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Name_CRC_16_Offset);
   end Column_Name_CRC;

   procedure Set_Column_Name_CRC (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_8 (Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Name_CRC_16_Offset, Value);
   end Set_Column_Name_CRC;


   function Column_Name_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Name_Field_Byte_Count_Offset);
   end Column_Name_Field_Byte_Count;

   procedure Set_Column_Name_Field_Byte_Count (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_8 (Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Name_Field_Byte_Count_Offset, Value);
   end Set_Column_Name_Field_Byte_Count;


   function Column_Type (I: Item_Access'Class; Column_Index: Unsigned_32) return BR_Item_Type is
   begin
      return I.Storage.Get_Item_Type (Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Field_Type_Offset);
   end Column_Type;

   procedure Set_Column_Type (I: Item_Access'Class; Value: BR_Item_Type) is
   begin
      return I.Storage.Set_Item_Type (Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Field_Type_Offset, Value);
   end Set_Column_Type;


   function Column_Name_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Name_Field_Offset_Offset);
   end Column_Name_Field_Offset;

   procedure Set_Column_Name_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32) is
   begin
      return I.Storage.Set_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Name_Field_Offset_Offset, Value);
   end Set_Column_Name_Field_Offset;


   function Column_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Field_Offset_Offset);
   end Column_Field_Offset;

   procedure Set_Column_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32) is
   begin
      return I.Storage.Set_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Field_Offset_Offset, Value);
   end Set_Column_Field_Offset;


   function Column_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Field_Byte_Count_Offset);
   end Column_Field_Byte_Count;

   procedure Set_Column_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32) is
   begin
      return I.Storage.Set_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_index) + Column_Descriptor_Field_Byte_Count_Offset, Value);
   end Set_Column_Field_Byte_Count;


   function Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Column_Name_Field_Offset (Column_index) + Column_Name_Byte_Count);
   end Column_Name_Byte_Count;

   procedure Set_Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_8) is
   begin
      return I.Storage.Set_Unsigned_8 (I.Column_Name_Field_Offset (Column_index) + Column_Name_Byte_Count, Value);
   end Set_Column_Name_Byte_Count;


   function Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32) return String is
   begin
      return I.Storage.Get_String (I.Column_Name_Field_Offset (Column_Index) + Column_Name_Field_ASCII_Code_Offset, I.Column_Name_Byte_Count (Column_Index));
   end Column_Name;

   procedure Set_Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32; Value: String) is
   begin
      I.Storage.Set_String (I.Column_Name_Field_Offset (Column_Index) + Column_Name_Field_ASCII_Code_Offset, I.Column_Name_Byte_Count (Column_Index), Value);
   end Set_Column_Name;


   function Table_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Row_Index: Unsigned_32) return Unsigned_32 is
   begin
      return I.Value_Offset + Table_Fields_Offset + (Row_index - 1) * Table_Row_Byte_Count + zzzzz
   end Table_Field_Offset;
   --
   procedure Create_Table_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Column_Specifications: Array_Of_Table_Column_Specification);



   function Table_Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32) return String is
      Name_Field_Offset: Unsigned_32 := Name_Field_Offset (Column_Index);
      Length: Unsigned_8 := I.Storage.Get_Unsigned_8 (Name_Field_Offset + Table_Column_Name_Field_ASCII_Byte_Count_Offset);
   begin
      return I.Storage.Get_String (Name_Field_Offset + Table_Column_Name_Field_ASCII_Code_Offset, Length);
   end Table_Column_Name;


   procedure Create_Table_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Column_Specifications: Array_Of_Table_Column_Specification) is
   begin
   end Create_Table_Item;


   -- ===================
   -- BR_UUID
   -- ===================

   function BR_UUID (I: Item_Access'Class) return UUID is
      U: UUID;
   begin
      I.Storage.Get_Unsigned_8_Array (I.Value_Offset, U);
      return U;
   end BR_UUID;

   procedure Assign_UUID (I: Item_Access'Class; Value: UUID) is
   begin
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset, Array_Of_Unsigned_8 (Value));
   end Assign_UUID;

   procedure Create_BR_UUID (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_UUID;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Assign_BR_UUID (Value);

   end Create_BR_UUID;


   -- =================
   -- BR_Color
   -- =================

   function BR_Color (I: Item_Access'Class) return Color is
   begin
      return To_Color (I.Storage.Get_Unsigned_32 (I.Small_Value_Offset));
   end BR_Color;

   procedure Assign_BR_Color (I: Item_Access'Class; Value: Color) is
   begin
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, To_Unsigned_32 (Value));
   end Assign_BR_Color;

   procedure Create_BR_Color (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_UUID;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Assign_BR_Color (Value);

   end Create_BR_Color;


   -- ====================
   -- BR_Font
   -- ====================

   function BR_Font (I: Item_Access'Class) return Font is
      Ptr: BR_Font_Layout_Ptr := I.Storage.Get_BR_Font_Layout_Ptr (I.Value_Offset);
      F: Font;
      Family_Name: String := String (1 .. Ptr.Family_Name_Byte_Count);
      Font_Name: String := String (1 .. Ptr.Font_Name_Byte_Count);
   begin

      return F;
   end BR_Font;

   function Get_Family_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
      Ptr: BR_Font_Layout_Ptr := I.Storage.Get_BR_Font_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Family_Name_Byte_Count;
   end Get_Family_Name_Byte_Count;

   function Get_Font_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
      Ptr: BR_Font_Layout_Ptr := I.Storage.Get_BR_Font_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Font_Name_Byte_Count;
   end Get_Font_Name_Byte_Count;

   procedure Get_Family_Name (I: Item_Access'Class; Value: out String) is
      Ptr: BR_Font_Layout_Ptr := I.Storage.Get_BR_Font_Layout_Ptr (I.Value_Offset);
   begin
      I.Storage.Get_String (I.Value_Offset + Unsigned_32 (BR_Font_Names_Offset), Value);
   end Get_Family_Name;

   procedure Get_Font_Name (I: Item_Access'Class; Value: out String) is
      Ptr: BR_Font_Layout_Ptr := I.Storage.Get_BR_Font_Layout_Ptr (I.Value_Offset);
   begin
      I.Storage.Get_String (I.Value_Offset + Unsigned_32 (BR_Font_Names_Offset) + Unsigned_32 (Ptr.Family_Name_Byte_Count), Value);
   end Get_Font_Name;

   procedure Set_Font (I: Item_Access'Class; Value: Font) is

      Ptr: BR_Font_Layout_Ptr := I.Storage.Get_BR_Font_Layout_Ptr (I.Value_Offset);

   begin

      Ptr.Point_Size := Value.Points;
      Ptr.Family_Name_Byte_Count := Unsigned_8 (Bounded_String_256.Length (Value.Family));
      Ptr.Font_Name_Byte_Count := Unsigned_8 (Bounded_String_256.Length (Value.Name));

      I.Storage.Set_String (I.Value_Offset + Unsigned_32 (BR_Font_Names_Offset), Bounded_String_256.To_String (Value.Family));
      I.Storage.Set_String (I.Value_Offset + Unsigned_32 (BR_Font_Names_Offset) + Unsigned_32 (Ptr.Family_Name_Byte_Count), Bounded_String_256.To_String (Value.Name));

   end Set_Font;

   procedure Create_Font (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Font;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Set_Font (Value);

   end Create_Font;


   -- =========================
   -- Item Name Assistent
   -- =========================

   function Create_Item_Name_Assistent (S: Item_Name) return Item_Name_Assistent is

      Name: String := Item_Name_Bounded_String.To_String (S);
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

   function Create_Item_Access (S: Storage_Area_Ptr; O: Unsigned_32) return Item_Access is
   begin
      return (S, O, S.Get_Item_Header_Ptr (O));
   end Create_Item_Access;


end Item;

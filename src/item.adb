with Crc_Package; use Crc_Package;

package body Item is


   procedure Assign_Item_Type (I: Item_Access'Class; Value: BR_Item_Type) is
   begin
      I.Storage.Set_Item_Type (I.Offset + Item_Type_Offset, Value);
   end Assign_Item_Type;


   procedure Assign_Item_Options (I: Item_Access'Class; Value: BR_Item_Options) is
   begin
      I.Storage.Set_Item_Type (I.Offset + Item_Options_Offset, Value);
   end Assign_Item_Type;


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
      I.Storage.Set_Unsigned_32 (I.Offset + Item_Name_Parent_Offset_Offset, Value);
   end Assign_Item_Byte_Count;


   -- ===================
   -- BR_Null
   -- ===================

   procedure Create_Null_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Null); Parent_Offset: Unsigned_32 := 0) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Null);
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Name_Field_Byte_Count (A.Name_Field_Byte_Count);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Value_Offset, 0);

      I.Set_Name (A);

   end Create_Null_Item;


   -- ====================
   -- BR_Bool
   -- ====================

   function Boolean_Value (I: Item_Access'Class) return Boolean is
   begin
      return I.Storage.Get_Bool (I.Small_Value_Offset);
   end Bool_Value;

   procedure Assign_Boolean_Value (I: Item_Access'Class; Value: Boolean) is
   begin
      I.Storage.Set_Bool (I.Small_Value_Offset, Value);
   end Assign_Boolean_Value;

   procedure Create_Boolean_Item (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False) is
      A: Item_Name_Assistent := Create_Item_Name_Assistent (Name);
   begin

      I.Assign_Item_Type (BR_Bool):
      I.Assign_Item_Options (No_Options);
      I.Assign_Item_Flags (No_Flags);
      I.Assign_Item_Name_Field_Byte_Count (A.Name_Field_Byte_Count);
      I.Assign_Item_Byte_Count (Byte_Count);
      I.Assign_Item_Parent_Offset (Parent_Offset);
      I.Storage.Set_Unsigned_32 (I.Small_Value_Offset, 0);
      I.Storage.Set_Bool (I.Small_Value_Offset, Value);

      I.Set_Name (A);

   end Create_Boolean_Item;


   -- =======================
   -- BR_Int8
   -- =======================

   function BR_Int8 (I: Item_Access'Class) return Integer_8 is
   begin
      return I.Storage.Get_Integer_8 (I.Small_Value_Offset);
   end BR_Int8;

   procedure Assign_BR_Int8 (I: Item_Access'Class; Value: Integer_8) is
   begin
      I.Storage.Set_Integer_8 (I.Small_Value_Offset, Value);
   end Assign_BR_Int8;

   procedure Create_BR_Int8 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Int8;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Int8 (Value);

   end Create_BR_Int8;



   -- =======================
   -- BR_Int16
   -- =======================

   function BR_Int16 (I: Item_Access'Class) return Integer_16 is
   begin
      return I.Storage.Get_Integer_16 (I.Small_Value_Offset);
   end BR_Int16;

   procedure Assign_BR_Int16 (I: Item_Access'Class; Value: Integer_16) is
   begin
      I.Storage.Set_Integer_16 (I.Small_Value_Offset, Value);
   end Assign_BR_Int16;

   procedure Create_BR_Int16 (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Int16;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Int16 (Value);

   end Create_BR_Int16;


   -- ========================
   -- BR_Int32
   -- ========================

   function BR_Int32  (I: Item_Access'Class) return Integer_32 is
   begin
      return I.Storage.Get_Integer_32 (I.Small_Value_Offset);
   end BR_Int32;

   procedure Assign_BR_Int32 (I: Item_Access'Class; Value: Integer_32) is
   begin
      I.Storage.Set_Integer_32 (I.Small_Value_Offset, Value);
   end Assign_BR_Int32;


   procedure Create_BR_Int32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Int32;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Int32 (Value);

   end Create_BR_Int32;


   -- ======================
   -- BR_Int64
   -- ======================

   function BR_Int64 (I: Item_Access'Class) return Integer_64 is
   begin
      return I.Storage.Get_Integer_64 (I.Value_Offset);
   end BR_Int64;

   procedure Assign_BR_Int64 (I: Item_Access'Class; Value: Integer_64) is
   begin
      I.Storage.Set_Integer_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_BR_Int64;


   procedure Create_BR_Int64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Int64;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Int64 (Value);

   end Create_BR_Int64;


   -- =======================
   -- BR_UInt8
   -- =======================

   function BR_UInt8 (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Small_Value_Offset);
   end BR_UInt8;

   procedure Assign_BR_UInt8 (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_BR_UInt8;


   procedure Create_BR_UInt8 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_UInt8;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_UInt8 (Value);

   end Create_BR_UInt8;


   -- ======================
   -- BR_UInt16
   -- ======================

   function BR_UInt16 (I: Item_Access'Class) return Unsigned_16 is
   begin
      return I.Storage.Get_Unsigned_16 (I.Small_Value_Offset);
   end BR_UInt16;

   procedure Assign_BR_UInt16 (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_BR_UInt16;


   procedure Create_BR_UInt16 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_UInt16;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_UInt16 (Value);

   end Create_BR_UInt16;


   -- ======================
   -- BR_UInt32
   -- ======================

   function BR_UInt32 (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Small_Value_Offset);
   end BR_UInt32;

   procedure Assign_BR_UInt32 (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_BR_UInt32;


   procedure Create_BR_UInt32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_UInt32;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_UInt32 (Value);

   end Create_BR_UInt32;


   -- =======================
   -- BR_UInt64
   -- =======================

   function BR_UInt64 (I: Item_Access'Class) return Unsigned_64 is
   begin
      return I.Storage.Get_Unsigned_64 (I.Value_Offset);
   end BR_UInt64;

   procedure Assign_BR_UInt64 (I: Item_Access'Class; Value: Unsigned_64) is
   begin
      I.Storage.Set_Unsigned_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_BR_UInt64;

   procedure Create_BR_UInt64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_UInt64;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_UInt64 (Value);

   end Create_BR_UInt64;


   -- ======================
   -- BR_Float32
   -- ======================

   function BR_Float32 (I: Item_Access'Class) return IEEE_Float_32 is
   begin
      return I.Storage.Get_Float_32 (I.Small_Value_Offset);
   end BR_Float32;

   procedure Assign_BR_Float32 (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Assign_BR_Float32;

   procedure Create_BR_Float32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Float32;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Float32 (Value);

   end Create_BR_Float32;


   -- ====================
   -- BR_Float64
   -- ====================

   function BR_Float64 (I: Item_Access'Class) return IEEE_Float_64 is
   begin
      return I.Storage.Get_Float_64 (I.Value_Offset);
   end BR_Float64;

   procedure Assign_BR_Float64 (I: Item_Access'Class; Value: IEEE_Float_64) is
   begin
      I.Storage.Set_Float_64 (Offset => I.Value_Offset, Value => Value);
   end Assign_BR_Float64;

   procedure Create_BR_Float64 (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Float64;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Float64 (Value);

   end Create_BR_Float64;


   -- ======================
   -- BR_String
   -- ======================

   function BR_String (I: Item_Access'Class) return String is
      Ptr: BR_String_Layout_Ptr := I.Storage.Get_BR_String_Layout_Ptr (I.Value_Offset);
      Str: String (1 .. Integer (Ptr.Byte_Count));
   begin
      I.Get_BR_String (Str);
      return Str;
   end BR_String;

   procedure Get_BR_String (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Value_Offset + Unsigned_32 (BR_String_Byte_Code_Offset), Value);
   end Get_BR_String;

   procedure Assign_BR_String (I: Item_Access'Class; Value: String) is
      Ptr: BR_String_Layout_Ptr := I.Storage.Get_BR_String_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_String (I.Value_Offset + Unsigned_32 (BR_String_Byte_Code_Offset), Value);
   end Assign_BR_String;

   procedure Create_BR_String (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_String;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_String (Value);

   end Create_BR_String;


   -- ======================
   -- BR_CRC_String
   -- ======================

   function BR_CRC_String (I: Item_Access'Class) return String is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
      Str: String (1 .. Integer (Ptr.Byte_Count));
   begin
      I.Get_BR_CRC_String (Str);
      return Str;
   end BR_CRC_String;

   procedure Get_BR_CRC_String (I: Item_Access'Class; Value: out String) is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
   begin
      I.Storage.Get_String (I.Value_Offset + Unsigned_32 (BR_CRC_String_Byte_Code_Offset), Value);
   end Get_BR_CRC_String;

   procedure Assign_BR_CRC_String (I: Item_Access'Class; Value: String) is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.CRC_32 := Calculate_CRC_32 (Value);
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_String (I.Value_Offset + Unsigned_32 (BR_CRC_String_Byte_Code_Offset), Value);
   end Assign_BR_CRC_String;

   function BR_CRC_String_CRC (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.CRC_32;
   end BR_CRC_String_CRC;


   procedure Create_BR_CRC_String (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is

      Ptr: Item_Header_Ptr := I.Storage.Get_Item_Header_Ptr (I.Value_Offset);
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_CRC_String;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_CRC_String (Value);

   end Create_BR_CRC_String;


   -- ======================
   -- BR_Binary
   -- ======================

   function BR_Binary (I: Item_Access'Class) return Array_Of_Unsigned_8 is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
      Arr: Array_Of_Unsigned_8 (1 .. Ptr.Byte_Count);
   begin
      I.Get_BR_Binary (Arr);
      return Arr;
   end BR_Binary;

   function BR_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Byte_Count;
   end BR_Binary_Byte_Count;

   procedure Assign_BR_Binary (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_Binary_Data_Offset), Value);
   end Assign_BR_Binary;

   procedure Get_BR_Binary (I: Item_Access'Class; Value: out Array_Of_Unsigned_8) is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
   begin
      I.Storage.Get_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_Binary_Data_Offset), Value);
   end Get_BR_Binary;

   procedure Create_BR_Binary (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is

      Ptr: Item_Header_Ptr := I.Storage.Get_Item_Header_Ptr (I.Offset);
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Binary;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_Binary (Value);

   end Create_BR_Binary;


   -- =====================
   -- BR_CRC_Binary
   -- =====================

   function BR_CRC_Binary (I: Item_Access'Class) return Array_Of_Unsigned_8 is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
      Arr: Array_Of_Unsigned_8 (1 .. Ptr.Byte_Count);
   begin
      I.Get_BR_CRC_Binary (Arr);
      return Arr;
   end BR_CRC_Binary;

   function BR_CRC_Binary_CRC (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.CRC_32;
   end BR_CRC_Binary_CRC;

   function BR_CRC_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Byte_Count;
   end BR_CRC_Binary_Byte_Count;

   procedure Assign_BR_CRC_Binary (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.CRC_32 := Calculate_CRC_32 (Value);
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_CRC_Binary_Data_Offset), Value);
   end Assign_BR_CRC_Binary;

   procedure Get_BR_CRC_Binary (I: Item_Access'Class; Value: out Array_Of_Unsigned_8) is
   begin
      I.Storage.Get_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_CRC_Binary_Data_Offset), Value);
   end Get_BR_CRC_Binary;

   procedure Create_BR_CRC_Binary (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_CRC_Binary;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);
      I.Assign_BR_CRC_Binary (Value);

   end Create_BR_CRC_Binary;


   -- ====================
   -- Array
   -- ====================

   function BR_Array_Element_Type (I: Item_Access'Class) return BR_Item_Type is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Element_Type;
   end BR_Array_Element_Type;

   function BR_Array_Element_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Element_Count;
   end BR_Array_Element_Count;

   function BR_Array_Element_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Element_Byte_Count;
   end BR_Array_Element_Byte_Count;

   function BR_Array_Element_Offset (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32 is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return I.Value_Offset + Unsigned_32 (BR_Array_Element_Base_Offset) + Ptr.Element_Byte_Count * (Index - 1);
   end BR_Array_Element_Offset;

   procedure Set_BR_Array_Element_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Element_Count := Value;
   end Set_BR_Array_Element_Count;

   procedure Set_BR_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Element_Byte_Count := Value;
   end Set_BR_Array_Element_Byte_Count;

   procedure Create_BR_Array (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      APtr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Array;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      APtr.Reserved_32 := 0;
      APtr.Reserved_8 := 0;
      APtr.Reserved_16 := 0;
      APtr.Element_Type := Element_Type;
      APtr.Element_Byte_Count := Element_Byte_Count;

   end Create_BR_Array;


   -- =================
   -- BR_Dictionary
   -- =================

   function BR_Dictionary_Item_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Dictionary_Layout_Ptr := I.Storage.Get_BR_Dictionary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Item_Count;
   end BR_Dictionary_Item_Count;

   procedure Set_BR_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Dictionary_Layout_Ptr := I.Storage.Get_BR_Dictionary_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Item_Count := Value;
   end Set_BR_Dictionary_Item_Count;

   procedure Create_BR_Dictionary (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      DPtr: BR_Dictionary_Layout_Ptr := I.Storage.Get_BR_Dictionary_Layout_Ptr (I.Value_Offset);
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Dictionary;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      DPtr.Reserved_32 := 0;
      DPtr.Item_Count := 0;

   end Create_BR_Dictionary;


   -- =================
   -- BR_Sequence
   -- =================

   function BR_Sequence_Item_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Sequence_Layout_Ptr := I.Storage.Get_BR_Sequence_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Item_Count;
   end BR_Sequence_Item_Count;

   procedure Set_BR_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Sequence_Layout_Ptr := I.Storage.Get_BR_Sequence_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Item_Count := Value;
   end Set_BR_Sequence_Item_Count;

   procedure Create_BR_Sequence (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      SPtr: BR_Sequence_Layout_Ptr := I.Storage.Get_BR_Sequence_Layout_Ptr (I.Value_Offset);
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Sequence;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      SPtr.Reserved_32 := 0;
      SPtr.Item_Count := 0;

   end Create_BR_Sequence;


   -- =================
   -- Table
   -- =================

   function BR_Table_Row_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return 0;
   end BR_Table_Row_Count;

   function BR_Table_Column_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return 0;
   end BR_Table_Column_Count;

   function BR_Table_Column_Descriptor_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
   begin
      return 0;
   end BR_Table_Column_Descriptor_Offset;

   function BR_Table_Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8 is
   begin
      return 0;
   end BR_Table_Column_Name_Byte_Count;

   function BR_Table_Column_Name_CRC (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_16 is
   begin
      return 0;
   end BR_Table_Column_Name_CRC;

   procedure Get_BR_Table_Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32; Value: out String) is
   begin
      null;
   end Get_BR_Table_Column_Name;

   procedure Set_BR_Table_Row_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      null;
   end Set_BR_Table_Row_Count;

   procedure Set_BR_Table_Column_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      null;
   end Set_BR_Table_Column_Count;

   procedure Create_BR_Table (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_BR_Table;


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


   -- =================================================
   -- Set the name of an item and its field byte count.
   -- =================================================

   procedure Set_Name (I: Item_Access'Class; Name: Item_Name_Assistent) is

      Ptr: Item_Header_Ptr := I.Storage.Get_Item_Header_Ptr (I.Offset);
      NF_Ptr: Item_Name_Field_Layout_Ptr;

   begin

      Ptr.Name_Field_Byte_Count := Name.Name_Field_Byte_Count;

      if Name.Name_Field_Byte_Count > 0 then

         declare

            subtype Name_Array is Array_Of_Unsigned_8 (1 .. Unsigned_32 (Name.Name_Field_Byte_Count));
            type Name_Array_Ptr is access Name_Array;
            function To_Name_Array_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Name_Array_Ptr);
            Ascii_Ptr: Name_Array_Ptr;

         begin

            NF_Ptr := I.Storage.Get_Item_Name_Field_Layout_Ptr (I.Offset + Item_Header'Size);
            NF_Ptr.CRC_16 := Name.CRC_16;
            NF_Ptr.Byte_Count := Name.Ascii_Code_Count;
            Ascii_Ptr := To_Name_Array_Ptr (NF_Ptr.Ascii_Code'Access);
            Ascii_Ptr.all := Name.Ascii_Code;

         end;

      end if;

   end Set_Name;

end Item;

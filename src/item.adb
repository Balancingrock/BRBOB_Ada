with Crc_Package; use Crc_Package;

package body Item is


   -- ===================
   -- BR_Null
   -- ===================

   procedure Create_Null (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

      Ptr: Item_Header_Ptr := I.Storage.Get_Item_Header_Ptr (I.Offset);
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Null;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;

      I.Set_Name (Assistent);

      Ptr.Small_Value := 0;

   end Create_Null;


   -- ====================
   -- BR_Bool
   -- ====================

   function Value (I: Item_Access'Class) return Boolean is
   begin
      return I.Storage.Get_Bool (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Boolean) is
   begin
      I.Storage.Set_Bool (I.Small_Value_Offset, Value);
   end Set_Value_To;

   procedure Create_Bool (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Item_Name_Assistent := Create_Item_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Bool;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;

      I.Set_Name (Assistent);
      I.Set_Value_To (Value);

   end Create_Bool;


   -- =======================
   -- BR_Int8
   -- =======================

   function Value (I: Item_Access'Class) return Integer_8 is
   begin
      return I.Storage.Get_Integer_8 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Integer_8) is
   begin
      I.Storage.Set_Integer_8 (I.Small_Value_Offset, Value);
   end Set_Value_To;

   procedure Create_Integer_8 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Integer_8;



   -- =======================
   -- BR_Int16
   -- =======================

   function Value (I: Item_Access'Class) return Integer_16 is
   begin
      return I.Storage.Get_Integer_16 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Integer_16) is
   begin
      I.Storage.Set_Integer_16 (I.Small_Value_Offset, Value);
   end Set_Value_To;

   procedure Create_Integer_16 (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Integer_16;


   -- ========================
   -- BR_Int32
   -- ========================

   function Value  (I: Item_Access'Class) return Integer_32 is
   begin
      return I.Storage.Get_Integer_32 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Integer_32) is
   begin
      I.Storage.Set_Integer_32 (I.Small_Value_Offset, Value);
   end Set_Value_To;


   procedure Create_Integer_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Integer_32;


   -- ======================
   -- BR_Int64
   -- ======================

   function Value (I: Item_Access'Class) return Integer_64 is
   begin
      return I.Storage.Get_Integer_64 (I.Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Integer_64) is
   begin
      I.Storage.Set_Integer_64 (Offset => I.Value_Offset, Value => Value);
   end Set_Value_To;


   procedure Create_Integer_64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Integer_64;


   -- =======================
   -- BR_UInt8
   -- =======================

   function Value (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Value_To;


   procedure Create_Unsigned_8 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Unsigned_8;


   -- ======================
   -- BR_UInt16
   -- ======================

   function Value (I: Item_Access'Class) return Unsigned_16 is
   begin
      return I.Storage.Get_Unsigned_16 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Value_To;


   procedure Create_Unsigned_16 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Unsigned_16;


   -- ======================
   -- BR_UInt32
   -- ======================

   function Value (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Value_To;


   procedure Create_Unsigned_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Unsigned_32;


   -- =======================
   -- BR_UInt64
   -- =======================

   function Value (I: Item_Access'Class) return Unsigned_64 is
   begin
      return I.Storage.Get_Unsigned_64 (I.Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: Unsigned_64) is
   begin
      I.Storage.Set_Unsigned_64 (Offset => I.Value_Offset, Value => Value);
   end Set_Value_To;

   procedure Create_Unsigned_64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0) is

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
      I.Set_Value_To (Value);

   end Create_Unsigned_64;


   -- ======================
   -- BR_Float32
   -- ======================

   function Value (I: Item_Access'Class) return IEEE_Float_32 is
   begin
      return I.Storage.Get_Float_32 (I.Small_Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Value_To;

   procedure Create_Float_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0) is

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
      I.Set_Value_To (Value);

   end Create_Float_32;


   -- ====================
   -- BR_Float64
   -- ====================

   function Value (I: Item_Access'Class) return IEEE_Float_64 is
   begin
      return I.Storage.Get_Float_64 (I.Value_Offset);
   end Value;

   procedure Set_Value_To (I: Item_Access'Class; Value: IEEE_Float_64) is
   begin
      I.Storage.Set_Float_64 (Offset => I.Value_Offset, Value => Value);
   end Set_Value_To;

   procedure Create_Float_64 (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0) is

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
      I.Set_Value_To (Value);

   end Create_Float_64;


   -- ======================
   -- BR_String
   -- ======================

   procedure Get_String (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Value_Offset + Unsigned_32 (BR_String_Byte_Code_Offset), Value);
   end Get_String;

   procedure Set_String (I: Item_Access'Class; Value: String) is
      Ptr: BR_String_Layout_Ptr := I.Storage.Get_BR_String_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_String (I.Value_Offset + Unsigned_32 (BR_String_Byte_Code_Offset), Value);
   end Set_String;

   procedure Create_String (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is

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
      I.Set_String (Value);

   end Create_String;


   -- ======================
   -- BR_Crc_String
   -- ======================

   procedure Set_CRC_String (I: Item_Access'Class; Value: String) is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.CRC_32 := Calculate_CRC_32 (Value);
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_String (I.Value_Offset + Unsigned_32 (BR_CRC_String_Byte_Code_Offset), Value);
   end Set_CRC_String;

   function Get_CRC_String_CRC (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.CRC_32;
   end Get_CRC_String_CRC;

   function Get_CRC_String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_String_Layout_Ptr := I.Storage.Get_BR_CRC_String_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Byte_Count;
   end Get_CRC_String_Byte_Count;

   procedure Get_CRC_String (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Value_Offset + Unsigned_32 (BR_CRC_String_Byte_Code_Offset), Value);
   end Get_CRC_String;

   procedure Create_CRC_String (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is

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
      I.Set_CRC_String (Value);

   end Create_CRC_String;


   -- ======================
   -- BR_Binary
   -- ======================

   function Get_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Byte_Count;
   end Get_Binary_Byte_Count;

   procedure Set_Binary (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_Binary_Data_Offset), Value);
   end Set_Binary;

   procedure Get_Binary (I: Item_Access'Class; Value: out Array_Of_Unsigned_8) is
      Ptr: BR_Binary_Layout_Ptr := I.Storage.Get_BR_Binary_Layout_Ptr (I.Value_Offset);
   begin
      I.Storage.Get_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_Binary_Data_Offset), Value);
   end Get_Binary;

   procedure Create_Binary (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is

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
      I.Set_Binary (Value);

   end Create_Binary;


   -- =====================
   -- CRC Binary
   -- =====================

   function Get_CRC_Binary_CRC (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.CRC_32;
   end Get_CRC_Binary_CRC;

   function Get_CRC_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Byte_Count;
   end Get_CRC_Binary_Byte_Count;

   procedure Set_CRC_Binary (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
      Ptr: BR_CRC_Binary_Layout_Ptr := I.Storage.Get_BR_CRC_Binary_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.CRC_32 := Calculate_CRC_32 (Value);
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_CRC_Binary_Data_Offset), Value);
   end Set_CRC_Binary;

   procedure Get_CRC_Binary (I: Item_Access'Class; Value: out Array_Of_Unsigned_8) is
   begin
      I.Storage.Get_Unsigned_8_Array (I.Value_Offset + Unsigned_32 (BR_CRC_Binary_Data_Offset), Value);
   end Get_CRC_Binary;

   procedure Create_CRC_Binary (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is

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
      I.Set_CRC_Binary (Value);

   end Create_CRC_Binary;


   -- ====================
   -- Array
   -- ====================

   function Get_Array_Element_Type (I: Item_Access'Class) return BR_Item_Type is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Element_Type;
   end Get_Array_Element_Type;

   function Get_Array_Element_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Element_Count;
   end Get_Array_Element_Count;

   function Get_Array_Element_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Element_Byte_Count;
   end Get_Array_Element_Byte_Count;

   function Get_Array_Element_Offset (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32 is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      return I.Value_Offset + Unsigned_32 (BR_Array_Element_Base_Offset) + Ptr.Element_Byte_Count * (Index - 1);
   end Get_Array_Element_Offset;

   procedure Set_Array_Element_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Element_Count := Value;
   end Set_Array_Element_Count;

   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Array_Layout_Ptr := I.Storage.Get_BR_Array_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Element_Byte_Count := Value;
   end Set_Array_Element_Byte_Count;

   procedure Create_Array (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128) is

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

   end Create_Array;


   -- =================
   -- BR_Dictionary
   -- =================

   function Get_Dictionary_Item_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Dictionary_Layout_Ptr := I.Storage.Get_BR_Dictionary_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Item_Count;
   end Get_Dictionary_Item_Count;

   procedure Set_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Dictionary_Layout_Ptr := I.Storage.Get_BR_Dictionary_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Item_Count := Value;
   end Set_Dictionary_Item_Count;

   procedure Create_Dictionary (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

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

   end Create_Dictionary;


   -- =================
   -- BR_Sequence
   -- =================

   function Get_Sequence_Item_Count (I: Item_Access'Class) return Unsigned_32 is
      Ptr: BR_Sequence_Layout_Ptr := I.Storage.Get_BR_Sequence_Layout_Ptr (I.Value_Offset);
   begin
      return Ptr.Item_Count;
   end Get_Sequence_Item_Count;

   procedure Set_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
      Ptr: BR_Sequence_Layout_Ptr := I.Storage.Get_BR_Sequence_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Item_Count := Value;
   end Set_Sequence_Item_Count;

   procedure Create_Sequence (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

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

   end Create_Sequence;


   -- =================
   -- Table
   -- =================

   function Get_Table_Row_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return 0;
   end Get_Table_Row_Count;

   function Get_Table_Column_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return 0;
   end Get_Table_Column_Count;

   function Get_Table_Column_Descriptor_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
   begin
      return 0;
   end Get_Table_Column_Descriptor_Offset;

   function Get_Table_Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8 is
   begin
      return 0;
   end Get_Table_Column_Name_Byte_Count;

   function Get_Table_Column_Name_CRC (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_16 is
   begin
      return 0;
   end Get_Table_Column_Name_CRC;

   procedure Get_Table_Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32; Value: out String) is
   begin
      null;
   end Get_Table_Column_Name;

   procedure Set_Table_Row_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      null;
   end Set_Table_Row_Count;

   procedure Set_Table_Column_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      null;
   end Set_Table_Column_Count;

   procedure Create_Table (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Table;


   -- ===================
   -- BR_UUID
   -- ===================

   procedure Get_UUID (I: Item_Access'Class; Value: out UUID) is
   begin
      I.Storage.Get_Unsigned_8_Array (I.Value_Offset, Array_Of_Unsigned_8 (Value));
   end Get_UUID;

   procedure Set_UUID (I: Item_Access'Class; Value: UUID) is
   begin
      I.Storage.Set_Unsigned_8_Array (I.Value_Offset, Array_Of_Unsigned_8 (Value));
   end Set_UUID;

   procedure Create_UUID (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID) is

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

      I.Set_UUID (Value);

   end Create_UUID;


   -- =================
   -- BR_Color
   -- =================

   function Get_Color (I: Item_Access'Class) return Color is
   begin
      return To_Color (I.Storage.Get_Unsigned_32 (I.Value_Offset));
   end Get_Color;

   procedure Set_Color (I: Item_Access'Class; Value: Color) is
   begin
      I.Storage.Set_Unsigned_32 (I.Value_Offset, To_Unsigned_32 (Value));
   end Set_Color;

   procedure Create_Color (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black) is

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

      I.Set_Color (Value);

   end Create_Color;


   -- ====================
   -- BR_Font
   -- ====================

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

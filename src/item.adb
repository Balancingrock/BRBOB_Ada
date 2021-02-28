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

   function Get_Bool (I: Item_Access'Class) return Boolean is
   begin
      return I.Storage.Get_Bool (I.Small_Value_Offset);
   end Get_Bool;

   procedure Set_Bool (I: Item_Access'Class; Value: Boolean) is
   begin
      I.Storage.Set_Bool (I.Small_Value_Offset, Value);
   end Set_Bool;

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
      I.Set_Bool (Value);

   end Create_Bool;


   -- =======================
   -- BR_Int8
   -- =======================

   procedure Set_Integer_8 (I: Item_Access'Class; Value: Integer_8) is
   begin
      I.Storage.Set_Integer_8 (I.Small_Value_Offset, Value);
   end Set_Integer_8;

   function Get_Integer_8 (I: Item_Access'Class) return Integer_8 is
   begin
      return I.Storage.Get_Integer_8 (I.Small_Value_Offset);
   end Get_Integer_8;

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
      I.Set_Integer_8 (Value);

   end Create_Integer_8;



   -- =======================
   -- BR_Int16
   -- =======================

   procedure Set_Integer_16 (I: Item_Access'Class; Value: Integer_16) is
   begin
      I.Storage.Set_Integer_16 (I.Small_Value_Offset, Value);
   end Set_Integer_16;

   function Get_Integer_16 (I: Item_Access'Class) return Integer_16 is
   begin
      return I.Storage.Get_Integer_16 (I.Small_Value_Offset);
   end Get_Integer_16;

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
      I.Set_Integer_16 (Value);

   end Create_Integer_16;


   -- ========================
   -- BR_Int32
   -- ========================

   procedure Set_Integer_32 (I: Item_Access'Class; Value: Integer_32) is
   begin
      I.Storage.Set_Integer_32 (I.Small_Value_Offset, Value);
   end Set_Integer_32;

   function Get_Integer_32 (I: Item_Access'Class) return Integer_32 is
   begin
      return I.Storage.Get_Integer_32 (I.Small_Value_Offset);
   end Get_Integer_32;

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
      I.Set_Integer_32 (Value);

   end Create_Integer_32;


   -- ======================
   -- BR_Int64
   -- ======================

   procedure Set_Integer_64 (I: Item_Access'Class; Value: Integer_64) is
   begin
      I.Storage.Set_Integer_64 (Offset => I.Value_Offset, Value => Value);
   end Set_Integer_64;

   function Get_Integer_64 (I: Item_Access'Class) return Integer_64 is
   begin
      return I.Storage.Get_Integer_64 (I.Value_Offset);
   end Get_Integer_64;

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
      I.Set_Integer_64 (Value);

   end Create_Integer_64;


   -- =======================
   -- BR_UInt8
   -- =======================

   procedure Set_Unsigned_8 (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Unsigned_8;

   function Get_Unsigned_8 (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Small_Value_Offset);
   end Get_Unsigned_8;

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
      I.Set_Unsigned_8 (Value);

   end Create_Unsigned_8;


   -- ======================
   -- BR_UInt16
   -- ======================

   procedure Set_Unsigned_16 (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Unsigned_16;

   function Get_Unsigned_16 (I: Item_Access'Class) return Unsigned_16 is
   begin
      return I.Storage.Get_Unsigned_16 (I.Small_Value_Offset);
   end Get_Unsigned_16;

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
      I.Set_Unsigned_16 (Value);

   end Create_Unsigned_16;


   -- ======================
   -- BR_UInt32
   -- ======================

   procedure Set_Unsigned_32 (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Unsigned_32;

   function Get_Unsigned_32 (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Small_Value_Offset);
   end Get_Unsigned_32;

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
      I.Set_Unsigned_32 (Value);

   end Create_Unsigned_32;


   -- =======================
   -- BR_UInt64
   -- =======================

   procedure Set_Unsigned_64 (I: Item_Access'Class; Value: Unsigned_64) is
   begin
      I.Storage.Set_Unsigned_64 (Offset => I.Value_Offset, Value => Value);
   end Set_Unsigned_64;

   function Get_Unsigned_64 (I: Item_Access'Class) return Unsigned_64 is
   begin
      return I.Storage.Get_Unsigned_64 (I.Value_Offset);
   end Get_Unsigned_64;

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
      I.Set_Unsigned_64 (Value);

   end Create_Unsigned_64;


   -- ======================
   -- BR_Float32
   -- ======================

   procedure Set_Float_32 (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (Offset => I.Small_Value_Offset, Value => Value);
   end Set_Float_32;

   function Get_Float_32 (I: Item_Access'Class) return IEEE_Float_32 is
   begin
      return I.Storage.Get_Float_32 (I.Small_Value_Offset);
   end Get_Float_32;

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
      I.Set_Float_32 (Value);

   end Create_Float_32;


   -- ====================
   -- BR_Float64
   -- ====================

   procedure Set_Float_64 (I: Item_Access'Class; Value: IEEE_Float_64) is
   begin
      I.Storage.Set_Float_64 (Offset => I.Value_Offset, Value => Value);
   end Set_Float_64;

   function Get_Float_64 (I: Item_Access'Class) return IEEE_Float_64 is
   begin
      return I.Storage.Get_Float_64 (I.Value_Offset);
   end Get_Float_64;

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
      I.Set_Float_64 (Value);

   end Create_Float_64;


   -- ======================
   -- BR_String
   -- ======================

   procedure Set_String (I: Item_Access'Class; Value: String) is
      Ptr: BR_String_Layout_Ptr := I.Storage.Get_BR_String_Layout_Ptr (I.Value_Offset);
   begin
      Ptr.Byte_Count := Value'Length;
      I.Storage.Set_String (I.Value_Offset + 4, Value);
   end Set_String;

   procedure Get_String (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Get_Item_Value_Offset + String_Value_Offset, Value);
   end Get_String;

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


   -- ==========================
   -- String Value
   -- ==========================

   procedure Set_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_String_UTF_8_Code;

   procedure Get_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_String_UTF_8_Code;


   -- ===========================
   -- CRC String Value
   -- ===========================

   procedure Set_CRC_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_CRC_String_UTF_8_Code;

   procedure Get_CRC_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_CRC_String_UTF_8_Code;


   -- ===========================
   -- Binary Value
   -- ===========================

   procedure Set_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_Binary_Data;

   procedure Get_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_Binary_Data;


   -- ===========================
   -- CRC Binary Value
   -- ===========================

   procedure Set_CRC_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_CRC_Binary_Data;

   procedure Get_CRC_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_CRC_Binary_Data;


   -- ============================
   -- Array Value
   -- ============================



   -- ============================
   -- Item Creation

   function Create_Item_Access (S: Storage_Area_Ptr; O: Unsigned_32) return Item_Access is
   begin
      return (S, O, S.Get_Item_Header_Ptr (O));
   end Create_Item_Access;


   -- Set the name of an item and its field byte count.

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



   -- Crc String
   --
   CRC_String_CRC_Offset: Unsigned_32 := 0;
   CRC_String_Byte_Count_Offset: Unsigned_32 := 4;
   CRC_String_Value_Offset: Unsigned_32 := 8;
   --
   procedure Set_CRC_String_CRC (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, Value);
   end Set_CRC_String_CRC;
   --
   procedure Set_CRC_String_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + CRC_String_Byte_Count_Offset, Value);
   end Set_CRC_String_Byte_Count;
   --
   procedure Set_CRC_String_Value (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_String (I.Get_Item_Value_Offset + CRC_String_Value_Offset, Value);
   end Set_CRC_String_Value;
   --
   procedure Set_CRC_String (I: Item_Access'Class; Value: String) is
   begin
      I.Set_CRC_String_CRC (Calculate_CRC_32 (Value));
      I.Set_CRC_String_Byte_Count (Value'Length);
      I.Set_CRC_String_Value (Value);
   end Set_CRC_String;
   --
   --
   function Get_CRC_String_CRC (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset);
   end Get_CRC_String_CRC;
   --
   function Get_CRC_String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset + CRC_String_Byte_Count_Offset);
   end Get_CRC_String_Byte_Count;
   --
   procedure Get_CRC_String (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Get_Item_Value_Offset + CRC_String_Value_Offset, Value);
   end Get_CRC_String;
   --
   procedure Create_CRC_String (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Binary
   --
   Binary_Byte_Count_Offset: Unsigned_32 := 0;
   Binary_Value_Offset: Unsigned_32 := 4;
   --
   procedure Set_Binary_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, Value);
   end Set_Binary_Byte_Count;
   --
   procedure Set_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8_Array (I.Get_Item_Value_Offset + Binary_Value_Offset, Value);
   end Set_Binary_Value;
   --
   procedure Set_Binary (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
   begin
      I.Set_Binary_Byte_Count (Value'Length);
      I.Set_Binary_Value (Value);
   end Set_Binary;
   --
   --
   function Get_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset);
   end Get_Binary_Byte_Count;
   --
   procedure Get_Binary (I: Item_Access'Class; Value: out Array_Of_Unsigned_8) is
   begin
      I.Storage.Get_Unsigned_8_Array (I.Get_Item_Value_Offset + Binary_Value_Offset, Value);
   end Get_Binary;
   --
   procedure Create_Binary (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- CRC Binary
   --
   CRC_Binary_CRC_Offset: Unsigned_32 := 0;
   CRC_Binary_Byte_Count_Offset: Unsigned_32 := 4;
   CRC_Binary_Value_Offset: Unsigned_32 := 8;
   --
   --
   function Get_CRC_Binary_CRC (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset);
   end Get_CRC_Binary_CRC;
   --
   function Get_CRC_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset + CRC_Binary_Byte_Count_Offset);
   end Get_CRC_Binary_Byte_Count;
   --
   procedure Set_CRC_Binary_CRC (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, Value);
   end Set_CRC_Binary_CRC;
   --
   procedure Set_CRC_Binary_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + CRC_Binary_Byte_Count_Offset, Value);
   end Set_CRC_Binary_Byte_Count;
   --
   procedure Set_CRC_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8_Array (I.Get_Item_Value_Offset + CRC_Binary_Value_Offset, Value);
   end Set_CRC_Binary_Value;
   --
   procedure Set_CRC_Binary (I: Item_Access'Class; Value: Array_Of_Unsigned_8) is
   begin
      I.Set_CRC_String_CRC (Calculate_CRC_32 (Value));
      I.Set_CRC_String_Byte_Count (Value'Length);
      I.Set_CRC_Binary_Value (Value);
   end Set_CRC_Binary;
   --
   procedure Get_CRC_Binary (I: Item_Access'Class; Value: out Array_Of_Unsigned_8) is
   begin
      I.Storage.Get_Unsigned_8_Array (I.Get_Item_Value_Offset + CRC_Binary_Value_Offset, Value);
   end Get_CRC_Binary;
   --
   procedure Create_CRC_Binary (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Array
   --
   Array_Reserved_Offset: Unsigned_32 := 0;
   Array_Element_Type_Offset: Unsigned_32 := 4;
   Array_Zero_8_Offset: Unsigned_32 := 5;
   Array_Zero_16_Offset: Unsigned_32 := 6;
   Array_Element_Count_Offset: Unsigned_32 := 8;
   Array_Element_Byte_Count_Offset: Unsigned_32 := 12;
   Array_Start_Offset: Unsigned_32 := 16;
   --
   function Get_Array_Element_Type (I: Item_Access'Class) return BR_Item_Type is
      Raw_Type: Unsigned_8 := I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Array_Element_Type_Offset);
   begin
      if Raw_Type > BR_Item_Type'Pos (BR_Item_Type'Last) then raise BRBON.Illegal_Item_Type; end if;
      return BR_Item_Type'Val (Raw_Type);
   end Get_Array_Element_Type;
   --
   function Get_Array_Element_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset + Array_Element_Count_Offset);
   end Get_Array_Element_Count;
   --
   function Get_Array_Element_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset + Array_Element_Byte_Count_Offset);
   end Get_Array_Element_Byte_Count;
   --
   function Get_Array_Element_Offset (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32 is
      Element_Size: Unsigned_32 := Get_Array_Element_Byte_Count(I);
   begin
      return I.Get_Item_Value_Offset + Array_Start_Offset + Element_Size * (Index - 1);
   end Get_Array_Element_Offset;
   --
   procedure Set_Array_Zeros (I: Item_Access'Class) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + Array_Reserved_Offset, 0);
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Array_Zero_8_Offset, 0);
      I.Storage.Set_Unsigned_16 (I.Get_Item_Value_Offset + Array_Zero_16_Offset, 0);
   end Set_Array_Zeros;
   --
   procedure Set_Array_Element_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + Array_Element_Count_Offset, Value);
   end Set_Array_Element_Count;
   --
   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + Array_Element_Byte_Count_Offset, Value);
   end Set_Array_Element_Byte_Count;
   --
   procedure Set_Array_Element_Type (I: Item_Access'Class; Value: BR_Item_Type) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + Array_Element_type_Offset, BR_Item_Type'Pos(Value));
   end Set_Array_Element_Type;
   --
   procedure Create_Array (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
   begin

      Ptr.Item_Type := BR_Array;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Set_Array_Zeros;
      I.Set_Array_Element_Type (Element_Type);
      I.Set_Array_Element_Count (0);
      I.Set_Array_Element_Byte_Count (Element_Byte_Count);

   end Create_Array;


   -- Dictionary
   --
   Dictionary_Reserved_Offset: Unsigned_32 := 0;
   Dictionary_Item_Count_Offset: Unsigned_32 := 4;
   Dictionary_Item_Start_Offset: Unsigned_32 := 8;
   --
   function Get_Dictionary_Item_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset + Dictionary_Item_Count_Offset);
   end Get_Dictionary_Item_Count;
   --
   function Get_Dictionary_Item_Start_Offset  (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Get_Item_Value_Offset + Dictionary_Item_Start_Offset;
   end Get_Dictionary_Item_Start_Offset;
   --
   procedure Set_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + Dictionary_Item_Count_Offset, Value);
   end Set_Dictionary_Item_Count;
   --
   procedure Set_Dictionary_Zeros (I: Item_Access'Class) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, 0);
   end Set_Dictionary_Zeros;
   --
   procedure Create_Dictionary (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
   begin

      Ptr.Item_Type := BR_Dictionary;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Set_Dictionary_Zeros;
      I.Set_Dictionary_Item_Count (0);

   end Create_Dictionary;


   -- Sequence
   --
   Sequence_Reserved_Offset: Unsigned_32 := 0;
   Sequence_Item_Count_Offset: Unsigned_32 := 4;
   Sequence_Item_Start_Offset: Unsigned_32 := 8;
   --
   function Get_Sequence_Item_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset + Sequence_Item_Count_Offset);
   end Get_Sequence_Item_Count;
   --
   function Get_Sequence_Item_Start_Offset (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Get_Item_Value_Offset + Sequence_Item_Start_Offset;
   end Get_Sequence_Item_Start_Offset;
   --
   procedure Set_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset + Sequence_Item_Count_Offset, Value);
   end Set_Sequence_Item_Count;
   --
   procedure Set_Sequence_Zeros (I: Item_Access'Class) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, 0);
   end Set_Sequence_Zeros;
   --
   procedure Create_Sequence (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
   begin

      Ptr.Item_Type := BR_Sequence;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Set_Sequence_Zeros;
      I.Set_Sequence_Item_Count (0);

   end Create_Sequence;


   -- Table
   --
   function Get_Column_Descriptor_Layout_Ptr (I: Item_Access'Class; Column_Index: Unsigned_32) return BR_Table_Column_Name_Layout_Ptr is
      Table: BR_Table_Layout_Ptr := I.Get_Table_Layout_Ptr;
   begin
      return null;
   end Get_Column_Descriptor_Layout_Ptr;
   --
   function Get_Column_Name_Layout_Ptr (I: Item_Access'Class) return BR_Table_Column_Name_Layout_Ptr is
   begin
      return null;
   end Get_Column_Name_Layout_Ptr;
   --
   procedure Create_Table (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Table;


   -- UUID
   --
   procedure Get_UUID (I: Item_Access'Class; Value: out UUID) is
   begin
      I.Storage.Get_Unsigned_8_Array (I.Get_Item_Value_Offset, Array_Of_Unsigned_8 (Value));
   end Get_UUID;
   --
   procedure Set_UUID (I: Item_Access'Class; Value: UUID) is
   begin
      I.Storage.Set_Unsigned_8_Array (I.Get_Item_Value_Offset, Array_Of_Unsigned_8 (Value));
   end Set_UUID;
   --
   procedure Create_UUID (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Color
   --
   Color_Red_Offset: Unsigned_32 := 0;
   Color_Green_Offset: Unsigned_32 := 1;
   Color_Blue_Offset: Unsigned_32 := 2;
   Color_Alpha_Offset: Unsigned_32 := 3;
   --
   function Get_Alpha (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Color_Alpha_Offset);
   end Get_Alpha;
   --
   function Get_Red (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Color_Red_Offset);
   end Get_Red;
   --
   function Get_Green (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Color_Green_Offset);
   end Get_Green;
   --
   function Get_Blue (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Color_Blue_Offset);
   end Get_Blue;
   --
   function Get_Color (I: Item_Access'Class) return Color is
   begin
      return To_Color (I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset));
   end Get_Color;
   --
   procedure Set_Alpha (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Color_Alpha_Offset, Value);
   end Set_Alpha;
   --
   procedure Set_Red (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Color_Red_Offset, Value);
   end Set_Red;
   --
   procedure Set_Green (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Color_Green_Offset, Value);
   end Set_Green;
   --
   procedure Set_Blue (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Color_Blue_Offset, Value);
   end Set_Blue;
   --
   procedure Set_Color (I: Item_Access'Class; Value: Color) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, To_Unsigned_32 (Value));
   end Set_Color;
   --
   procedure Create_Color (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Font
   --
   Font_Size_Offset: Unsigned_32 := 0;
   Font_Family_Byte_Count_Offset: Unsigned_32 := 4;
   Font_Name_Byte_Count_Offset: Unsigned_32 := 5;
   Font_Value_Start_Offset: Unsigned_32 := 6;
   --
   function Get_Size (I: Item_Access'Class) return IEEE_Float_32 is
   begin
      return I.Storage.Get_Float_32 (I.Get_Item_Value_Offset);
   end Get_Size;
   --
   function Get_Family_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Font_Family_Byte_Count_Offset);
   end Get_Family_Name_Byte_Count;
   --
   function Get_Font_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Value_Offset + Font_Name_Byte_Count_Offset);
   end Get_Font_Name_Byte_Count;
   --
   procedure Get_Family_Name (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Get_Item_Value_Offset + Font_Value_Start_Offset, Value);
   end Get_Family_Name;
   --
   procedure Get_Font_Name (I: Item_Access'Class; Value: out String) is
      Family_Name_Length: Unsigned_32 := Unsigned_32 (I.Get_Family_Name_Byte_Count);
   begin
      I.Storage.Get_String (I.Get_Item_Value_Offset + Font_Value_Start_Offset + Family_Name_Length, Value);
   end Get_Font_Name;
   --
   procedure Set_Size (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (I.Get_Item_Value_Offset, Value);
   end Set_Size;
   --
   procedure Set_Family_Name_Byte_Count (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Font_Family_Byte_Count_Offset, Value);
   end Set_Family_Name_Byte_Count;
   --
   procedure Set_Font_Name_Byte_Count (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (I.Get_Item_Value_Offset + Font_Name_Byte_Count_Offset, Value);
   end Set_Font_Name_Byte_Count;
   --
   procedure Set_Family_Name_Value (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_String (I.Get_Item_Value_Offset + Font_Value_Start_Offset, Value);
   end Set_Family_Name_Value;
   --
   procedure Set_Font_Name_Value (I: Item_Access'Class; Value: String) is
      Family_Name_Length: Unsigned_32 := Unsigned_32 (I.Get_Family_Name_Byte_Count);
   begin
      I.Storage.Set_String (I.Get_Item_Value_Offset + Font_Value_Start_Offset + Family_Name_Length, Value);
   end Set_Font_Name_Value;
   --
   procedure Set_Family_Name (I: Item_Access'Class; Value: String) is
   begin
      I.Set_Family_Name_Byte_Count (Value'Length);
      I.Set_Family_Name_Value (Value);
   end Set_Family_Name;
   --
   procedure Set_Font_Name (I: Item_Access'Class; Value: String) is
   begin
      I.Set_Font_Name_Byte_Count (Value'Length);
      I.Set_Font_Name_Value (Value);
   end Set_Font_Name;
   --
   procedure Set_Font (I: Item_Access'Class; Value: Font) is
   begin
      I.Set_Size (Value.Points);
      I.Set_Family_Name (Bounded_String_256.To_String (Value.Family));
      I.Set_Font_Name (Bounded_String_256.To_String (Value.Name));
   end Set_Font;
   --
   procedure Create_Font (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


end Item;

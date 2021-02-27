with Crc_Package; use Crc_Package;

package body Item is


   function Create_Name_Assistent (S: Item_Name) return Name_Assistent is

      Name: String := Item_Name_Bounded_String.To_String (S);
      Assistent: Name_Assistent (Name'Length);
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

   end Create_Name_Assistent;


   procedure Item_Type (Ptr: Unsigned_8_Ptr; Value: BR_Item_Type) is
      IPtr: Item_Header_Ptr := To_Item_Header_Ptr (Ptr);
   begin
      IPtr.Item_Type := Value;
   end Item_Type;


   function Item_Type (Ptr: Unsigned_8_Ptr) return BR_Item_Type is
      Raw_Type: Unsigned_8 := Ptr.all;
   begin
      if Raw_Type > Unsigned_8 (BR_Item_Type'Pos (BR_Item_Type'Last_Valid)) then raise BRBON.Enum_Mapping_Failed; end if;
      return BR_Item_Type'Val (Raw_Type);
   end Item_Type;


   function Is_Valid_Item_Type (Ptr: Unsigned_8_Ptr) return Boolean is
      Raw_Type: Unsigned_8 := Ptr.all;
   begin
      return Raw_Type <= Unsigned_8 (BR_Item_Type'Pos (BR_Item_Type'Last_Valid));
   end Is_Valid_Item_Type;


   function Get_Value_Area_Ptr (Ptr: Item_Header_Ptr) return Unsigned_8_Ptr is
   begin
      return new Unsigned_8;
   end Get_Value_Area_Ptr;


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
      return (S, O);
   end Create_Item_Access;


   -- Set the name of an item and its field byte count.

   procedure Set_Name (I: Item_Access'Class; Name: Name_Assistent) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      NF_Ptr: Item_Name_Field_ptr;
      Ascii_Offset: Unsigned_32;
   begin
      Ptr.Name_Field_Byte_Count := Name.Name_Field_Byte_Count;
      if Name.Name_Field_Byte_Count > 0 then
         NF_Ptr := I.Get_Item_Name_Field_Ptr;
         Ascii_Offset := I.Offset + Item_Name_Field_Ascii_Code_Offset;
         NF_Ptr.CRC_16 := Name.CRC_16;
         NF_Ptr.Ascii_Count := Name.Ascii_Code_Count;
         I.Storage.Data (Ascii_Offset .. Ascii_Offset + Unsigned_32 (Name.Ascii_Code_Count - 1)) := Name.Ascii_Code;
      end if;
   end Set_Name;


   -- Create_Null
   --
   procedure Create_Null (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);

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


   -- Bool
   --
   procedure Set_Bool (I: Item_Access'Class; Value: Boolean) is
   begin
      I.Storage.Set_Bool (I.Get_Item_Small_Value_Offset, Value);
   end Set_Bool;
   --
   function Get_Bool (I: Item_Access'Class) return Boolean is
   begin
      return I.Storage.Get_Bool (I.Get_Item_Small_Value_Offset);
   end Get_Bool;
   --
   procedure Create_Bool (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Int 8
   --
   procedure Set_Integer_8 (I: Item_Access'Class; Value: Integer_8) is
   begin
      I.Storage.Set_Integer_8 (I.Get_Item_Small_Value_Offset, Value);
   end Set_Integer_8;
   --
   function Get_Integer_8 (I: Item_Access'Class) return Integer_8 is
   begin
      return I.Storage.Get_Integer_8 (I.Get_Item_Small_Value_Offset);
   end Get_Integer_8;
   --
   procedure Create_Integer_8 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Int 16
   --
   procedure Set_Integer_16 (I: Item_Access'Class; Value: Integer_16) is
   begin
      I.Storage.Set_Integer_16 (I.Get_Item_Small_Value_Offset, Value);
   end Set_Integer_16;
   --
   function Get_Integer_16 (I: Item_Access'Class) return Integer_16 is
   begin
      return I.Storage.Get_Integer_16 (I.Get_Item_Small_Value_Offset);
   end Get_Integer_16;
   --
   procedure Create_Integer_16 (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Int 32
   --
   procedure Set_Integer_32 (I: Item_Access'Class; Value: Integer_32) is
   begin
      I.Storage.Set_Integer_32 (I.Get_Item_Small_Value_Offset, Value);
   end Set_Integer_32;
   --
   function Get_Integer_32 (I: Item_Access'Class) return Integer_32 is
   begin
      return I.Storage.Get_Integer_32 (I.Get_Item_Small_Value_Offset);
   end Get_Integer_32;
   --
   procedure Create_Integer_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Int 64
   --
   procedure Set_Integer_64 (I: Item_Access'Class; Value: Integer_64) is
   begin
      I.Storage.Set_Integer_64 (Offset => I.Get_Item_Value_Offset, Value => Value);
   end Set_Integer_64;
   --
   function Get_Integer_64 (I: Item_Access'Class) return Integer_64 is
   begin
      return I.Storage.Get_Integer_64 (I.Get_Item_Value_Offset);
   end Get_Integer_64;
   --
   procedure Create_Integer_64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- UInt 8
   --
   procedure Set_Unsigned_8 (I: Item_Access'Class; Value: Unsigned_8) is
   begin
      I.Storage.Set_Unsigned_8 (Offset => I.Get_Item_Small_Value_Offset, Value => Value);
   end Set_Unsigned_8;
   --
   function Get_Unsigned_8 (I: Item_Access'Class) return Unsigned_8 is
   begin
      return I.Storage.Get_Unsigned_8 (I.Get_Item_Small_Value_Offset);
   end Get_Unsigned_8;
   --
   procedure Create_Unsigned_8 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- UInt 16
   --
   procedure Set_Unsigned_16 (I: Item_Access'Class; Value: Unsigned_16) is
   begin
      I.Storage.Set_Unsigned_16 (Offset => I.Get_Item_Small_Value_Offset, Value => Value);
   end Set_Unsigned_16;
   --
   function Get_Unsigned_16 (I: Item_Access'Class) return Unsigned_16 is
   begin
      return I.Storage.Get_Unsigned_16 (I.Get_Item_Small_Value_Offset);
   end Get_Unsigned_16;
   --
   procedure Create_Unsigned_16 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- UInt32
   --
   procedure Set_Unsigned_32 (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (Offset => I.Get_Item_Small_Value_Offset, Value => Value);
   end Set_Unsigned_32;
   --
   function Get_Unsigned_32 (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Small_Value_Offset);
   end Get_Unsigned_32;
   --
   procedure Create_Unsigned_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- UInt64
   --
   procedure Set_Unsigned_64 (I: Item_Access'Class; Value: Unsigned_64) is
   begin
      I.Storage.Set_Unsigned_64 (Offset => I.Get_Item_Value_Offset, Value => Value);
   end Set_Unsigned_64;
   --
   function Get_Unsigned_64 (I: Item_Access'Class) return Unsigned_64 is
   begin
      return I.Storage.Get_Unsigned_64 (I.Get_Item_Value_Offset);
   end Get_Unsigned_64;
   --
   procedure Create_Unsigned_64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Float 32
   --
   procedure Set_Float_32 (I: Item_Access'Class; Value: IEEE_Float_32) is
   begin
      I.Storage.Set_Float_32 (Offset => I.Get_Item_Small_Value_Offset, Value => Value);
   end Set_Float_32;
   --
   function Get_Float_32 (I: Item_Access'Class) return IEEE_Float_32 is
   begin
      return I.Storage.Get_Float_32 (I.Get_Item_Small_Value_Offset);
   end Get_Float_32;
   --
   procedure Create_Float_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- Float 64
   --
   procedure Set_Float_64 (I: Item_Access'Class; Value: IEEE_Float_64) is
   begin
      I.Storage.Set_Float_64 (Offset => I.Get_Item_Small_Value_Offset, Value => Value);
   end Set_Float_64;
   --
   function Get_Float_64 (I: Item_Access'Class) return IEEE_Float_64 is
   begin
      return I.Storage.Get_Float_64 (I.Get_Item_Small_Value_Offset);
   end Get_Float_64;
   --
   procedure Create_Float_64 (I: Item_Access'Class;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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


   -- String
   --
   String_Byte_Count_Offset: Unsigned_32 := 0;
   String_Value_Offset: Unsigned_32 := 4;
   --
   procedure Set_String_Byte_Count (I: Item_Access'Class; Value: Unsigned_32) is
   begin
      I.Storage.Set_Unsigned_32 (I.Get_Item_Value_Offset, Value);
   end Set_String_Byte_Count;
   --
   procedure Set_String_Value (I: Item_Access'Class; Value: String) is
   begin
      I.Storage.Set_String (I.Get_Item_Value_Offset + String_Value_Offset, Value);
   end Set_String_Value;
   --
   procedure Set_String (I: Item_Access'Class; Value: String) is
   begin
      I.Set_String_Byte_Count (Value'Length);
      I.Set_String_Value (Value);
   end Set_String;
   --
   function Get_String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
   begin
      return I.Storage.Get_Unsigned_32 (I.Get_Item_Value_Offset);
   end Get_String_Byte_Count;
   --
   procedure Get_String (I: Item_Access'Class; Value: out String) is
   begin
      I.Storage.Get_String (I.Get_Item_Value_Offset + String_Value_Offset, Value);
   end Get_String;
   --
   procedure Create_String (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
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
   procedure Create_Sequence (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Sequence;

   procedure Create_Table (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Table;

   procedure Create_UUID (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_UUID;

   procedure Create_Color (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Color;

   procedure Create_Font (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Font;


end Item;

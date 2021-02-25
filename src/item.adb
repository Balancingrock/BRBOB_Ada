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
   procedure Create_Null (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is

      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);

   begin

      Ptr.Item_Type := BR_Null;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      Ptr.Small_Value := 0;

   end Create_Null;



   -- Create Bool
   --
   procedure Create_Bool (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False) is
      Ptr: Item_Header_Ptr := I.Header_Ptr;
      Assistent: Name_Assistent := Create_Name_Assistent (Name);
   begin

      Ptr.Item_Type := BR_Null;
      Ptr.Options := No_Options;
      Ptr.Flags := No_Flags;
      Ptr.Parent_Offset := Parent_Offset;
      Ptr.Small_Value := 0;
      Ptr.Byte_Count := Byte_Count;
      Ptr.Parent_Offset := Parent_Offset;

      I.Set_Name (Assistent);

      I.Storage.Set_Unsigned_32 (Offset => I.Get_Item_Small_Value_Offset, Value => 0);

   end Create_Bool;

   procedure Create_Integer_8 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Integer_8;

   procedure Create_Integer_16 (I: Item_Access;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Integer_16;

   procedure Create_Integer_32 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Integer_32;

   procedure Create_Integer_64 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Integer_64;

   procedure Create_Unsigned_8 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Unsigned_8;

   procedure Create_Unsigned_16 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Unsigned_16;

   procedure Create_Unsigned_32 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Unsigned_32;

   procedure Create_Unsigned_64 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Unsigned_64;

   procedure Create_Float_32 (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Float_32;

   procedure Create_Float_64 (I: Item_Access;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Float_64;

   procedure Create_String (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is
   begin
      raise BRBON.Incomplete_Code;
   end Create_String;

   procedure Create_CRC_String (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "") is
   begin
      raise BRBON.Incomplete_Code;
   end Create_CRC_String;

   procedure Create_Binary (I: Item_Access;  Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Binary;

   procedure Create_CRC_Binary (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_CRC_Binary;

   procedure Create_Array (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Array;

   procedure Create_Dictionary (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Dictionary;

   procedure Create_Sequence (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Sequence;

   procedure Create_Table (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Table;

   procedure Create_UUID (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_UUID;

   procedure Create_Color (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Color;

   procedure Create_Font (I: Item_Access; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font) is
   begin
      raise BRBON.Incomplete_Code;
   end Create_Font;


end Item;

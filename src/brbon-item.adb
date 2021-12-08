with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with CRC_Package;

with BRBON.Utils;
with BRBON.Types; use BRBON.Types;


package body BRBON.Item is

   -- Item offsets
   --
   Type_Offset: constant Unsigned_32 := 0;                          -- 1 byte
   Options_Offset: constant Unsigned_32 := 1;                       -- 1 byte
   Flags_Offset: constant Unsigned_32 := 2;                         -- 1 byte
   Name_Field_Byte_Count_Offset: constant Unsigned_32 := 3;         -- 1 byte
   Byte_Count_Offset: constant Unsigned_32 := 4;                    -- 4 bytes
   Parent_Offset_Offset: constant Unsigned_32 := 8;                 -- 4 bytes
   Small_Value_Offset: constant Unsigned_32 := 12;                  -- 4 bytes
                     
   -- TTOOFFNC BBBBBBBB
   -- PPPPPPPP SSSSSSSS                   
   
   --
   Name_Field_CRC_Offset: constant Unsigned_32 := 16;
   Name_Field_ASCII_Byte_Count_Offset: constant Unsigned_32 := 18;
   Name_Field_ASCII_Code_Offset: constant Unsigned_32 := 19;        -- Up to 248 bytes


   -- Internal specifications
   


   -- ==========================================================================
   -- API
   -- ==========================================================================

   
   procedure Create_Item
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     Of_Type: Types.Item_Type;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    ) is

      T: Types.Item_Type renames Of_Type;
      C: Container.Instance renames In_Container;
      O: Unsigned_32 renames At_Offset;
      N: Name_Field_Assistent renames With_Name;
      B: Unsigned_32 renames Using_Byte_Count;
      P: Unsigned_32 renames Parent_Offset;

      Value_Offset: Unsigned_32 := At_Offset + Types.Minimum_Item_Byte_Count + Unsigned_32 (With_Name.Name_Field_Byte_Count);
      
   begin

      if T = Types.Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create illegal item type");
      end if;
      
      Item.Set_Item_Type (C, O, T);
      Item.Set_Item_Options (C, O, Types.No_Item_Options);
      Item.Set_Item_Flags (C, O, Types.No_Item_Flags);
      Item.Set_Item_Byte_Count (C, O, B);
      Item.Set_Item_Parent_Offset (C, O, P);
      Item.Set_Item_Name (C, O, N);
      
      case T is
         when Illegal => null; -- Cannot happen
         when Types.Null_Type | Types.Bool_Type | Types.Int_8_Type | Types.Int_16_Type | Types.Int_32_Type | Types.UInt_8_Type | Types.UInt_16_Type | Types.UInt_32_Type | Types.Float_32_Type | Types.RGBA_Type =>
            Item.Set_Item_Small_Value (C, O, 0);
         when Types.Int_64_Type =>
            C.Set_Integer_64 (Value_Offset, 0);
         when Types.UInt_64_Type =>
            C.Set_Unsigned_64 (Value_Offset, 0);
         when Types.Float_64_Type =>
            C.Set_Float_64 (Value_Offset, 0.0);
         when Types.String_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
         when Types.Crc_String_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
            C.Set_Unsigned_32 (Value_Offset + 4, 0);
         when Types.Binary_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
         when Types.Crc_Binary_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
            C.Set_Unsigned_32 (Value_Offset + 4, 0);
         when Types.Array_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
            C.Set_Unsigned_32 (Value_Offset + 4, 0);
            C.Set_Unsigned_32 (Value_Offset + 8, 0);
            C.Set_Unsigned_32 (Value_Offset + 12, 0);
         when Types.Dictionary_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
            C.Set_Unsigned_32 (Value_Offset + 4, 0);
         when Types.Sequence_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
            C.Set_Unsigned_32 (Value_Offset + 4, 0);
         when Types.Table_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
            C.Set_Unsigned_32 (Value_Offset + 4, 0);
            C.Set_Unsigned_32 (Value_Offset + 8, 0);
            C.Set_Unsigned_32 (Value_Offset + 12, 0);
         when Types.UUID_Type =>
            C.Set_Unsigned_64 (Value_Offset, 0);
         when Types.Font_Type =>
            C.Set_Unsigned_32 (Value_Offset, 0);
      end case;

   end Create_Item;

   
   function Get_Item_Type (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Type is
   begin
      return Types.To_Item_Type (C.Get_Unsigned_8 (Item_Offset + Type_Offset));
   end Get_Item_Type;
   
   
   function Get_Item_Options (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Options is
   begin
      return Types.To_Item_Options (C.Get_Unsigned_8 (Item_Offset + Options_Offset));
   end Get_Item_Options;
   
   
   function Get_Item_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Flags is
   begin
      return Types.To_Item_Flags (C.Get_Unsigned_8 (Item_Offset + Flags_Offset));
   end Get_Item_Flags;
   
   
   function Get_Item_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return C.Get_Unsigned_8 (Item_Offset + Name_Field_Byte_Count_Offset);
   end Get_Item_Name_Field_Byte_Count;
   
   
   function Get_Item_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return C.Get_Unsigned_32 (Item_Offset + Byte_Count_Offset);
   end Get_Item_Byte_Count;
   
   
   function Get_Item_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return C.Get_Unsigned_32 (Item_Offset + Small_Value_Offset);
   end Get_Item_Small_Value;
   
   
   function Get_Item_Parent_Offset (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return C.Get_Unsigned_32 (Item_Offset + Parent_Offset_Offset);
   end Get_Item_Parent_Offset;
   
   
   function Get_Item_Name_CRC (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return C.Get_Unsigned_16 (Item_Offset + Name_Field_CRC_Offset);
   end Get_Item_Name_CRC;
   
   
   function Get_Item_Name_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return C.Get_Unsigned_8 (Item_Offset + Name_Field_ASCII_Byte_Count_Offset);
   end Get_Item_Name_Byte_Count;
   
   
   function Get_Item_Name_String (C: in out Container.Instance; Item_Offset: Unsigned_32) return String is
      Length: Unsigned_32 := Unsigned_32 (Get_Item_Name_Byte_Count (C, Item_Offset));
   begin
      return C.Get_String (Item_Offset + Name_Field_ASCII_Code_Offset, Length);
   end Get_Item_Name_String;
   
   
   function Get_Item_Name_Quick_Check_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return C.Get_Unsigned_32 (Item_Offset + Name_Field_CRC_Offset);
   end Get_Item_Name_Quick_Check_Value;
   
   
   procedure Set_Item_Type (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Type) is
   begin
      C.Set_Unsigned_8 (Item_Offset + Type_Offset, Types.To_Unsigned_8 (Value));
   end Set_Item_Type;

   
   procedure Set_Item_Options (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Options) is
   begin
      C.Set_Unsigned_8 (Item_Offset + Options_Offset, Types.To_Unsigned_8 (Value));
   end Set_Item_Options;

   
   procedure Set_Item_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Flags) is
   begin
      C.Set_Unsigned_8 (Item_Offset + Flags_Offset, Types.To_Unsigned_8 (Value));
   end Set_Item_Flags;

   
   procedure Set_Item_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      C.Set_Unsigned_8 (Item_Offset + Name_Field_Byte_Count_Offset, Value);
   end Set_Item_Name_Field_Byte_Count;

   
   procedure Set_Item_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      C.Set_Unsigned_32 (Item_Offset + Byte_Count_Offset, Value);
   end Set_Item_Byte_Count;

   
   procedure Set_Item_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      C.Set_Unsigned_32 (Item_Offset + Small_Value_Offset, Value);
   end Set_Item_Small_Value;

   
   procedure Set_Item_Parent_Offset (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      C.Set_Unsigned_32 (Item_Offset + Parent_Offset_Offset, Value);
   end Set_Item_Parent_Offset;

   
   procedure Set_Item_Name (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Name_Field_Assistent) is
   begin
      if Value.Name_Field_Byte_Count > 0 then
         C.Set_Unsigned_8 (Item_Offset + Name_Field_Byte_Count_Offset, Value.Name_Field_Byte_Count);
         C.Set_Unsigned_16 (Item_Offset + Name_Field_CRC_Offset, Value.CRC);
         C.Set_Unsigned_8 (Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Unsigned_8 (Value.Ascii_Code'Length));
         C.Set_Unsigned_8_Array (Item_Offset + Name_Field_ASCII_Code_Offset, Value.Ascii_Code (1 .. Unsigned_32 (Value.Ascii_Byte_Count)));
      end if;
   end Set_Item_Name;
   
   
   procedure Set_Item_Name_CRC (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      C.Set_Unsigned_16 (Item_Offset + Name_Field_CRC_Offset, Value);
   end Set_Item_Name_CRC;

   
   procedure Set_Item_Name_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      C.Set_Unsigned_8 (Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Value);
   end Set_Item_Name_Byte_Count;

   
   procedure Set_Item_Name_String (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: String) is
   begin
      C.Set_String (Item_Offset + Name_Field_ASCII_Code_Offset, Value);
   end Set_Item_Name_String;
   
   
   --
   function Get_Minimum_Item_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_32 is
   begin
      return Unsigned_32 (NFA.Name_Field_Byte_Count) + Types.Minimum_Item_Byte_Count;
   end Get_Minimum_Item_Byte_Count;
   
   function Create_Name_Field_Assistent (S: String) return Name_Field_Assistent is

      Assistent: Name_Field_Assistent;
      Index: Unsigned_32 := 1;
      
   begin
      
      -- Avoid empty names
      --
      if S'Length = 0 then
         Assistent.CRC := 0;
         Assistent.Name_Field_Byte_Count := 0;
         Assistent.Ascii_Byte_Count := 0;
         return Assistent;
      end if;
      
      -- Don't accept too long names
      --
      if S'Length > Types.Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Types.Max_Name_Length'Image & ")");
      end if;

      -- Set the actual name length
      --
      Assistent.Ascii_Byte_Count := Unsigned_8 (S'Length);
      
      -- Set the CRC
      --
      Assistent.CRC := CRC_Package.Calculate_CRC_16 (S);

      -- Set the byte code
      -- Note: Unchecked conversion assignments don't seem to work
      --
      for C of S loop
         Assistent.Ascii_Code (Index) := Character'Pos (C);
         Index := Index + 1;
      end loop;

      -- Set the field size
      --
      Assistent.Name_Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + S'Length);

      return Assistent;

   end Create_Name_Field_Assistent;

   
   function Get_Quick_Check_Value (NFA: Name_Field_Assistent) return Unsigned_32 is
   begin
      raise Implementation;
      return 0;
   end Get_Quick_Check_Value;
   
   
   -- ==========================================================================
   -- Internal bodies
   -- ==========================================================================
   
   
end BRBON.Item;

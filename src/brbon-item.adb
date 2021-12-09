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

   
   procedure Create_Item_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     Of_Type: Types.Item_Type;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    ) is

      T: Types.Item_Type renames Of_Type;
      C: Container.Instance renames In_Container;
      O: Unsigned_32 renames At_Offset;
      N: Name_Field_Assistent.Instance renames With_Name;
      B: Unsigned_32 renames Using_Byte_Count;
      P: Unsigned_32 renames Parent_Offset;

      Value_Offset: Unsigned_32 := At_Offset + Types.Minimum_Item_Byte_Count + Unsigned_32 (With_Name.Name_Field_Byte_Count);
      
   begin

      if T = Types.Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create illegal item type");
      end if;
      
      Item.Set_Type (C, O, T);
      Item.Set_Options (C, O, Types.No_Item_Options);
      Item.Set_Flags (C, O, Types.No_Item_Flags);
      Item.Set_Byte_Count (C, O, B);
      Item.Set_Parent_Offset (C, O, P);
      Item.Set_Name (C, O, N);
      
      case T is
         when Illegal => null; -- Cannot happen
         when Types.Null_Type | Types.Bool_Type | Types.Int_8_Type | Types.Int_16_Type | Types.Int_32_Type | Types.UInt_8_Type | Types.UInt_16_Type | Types.UInt_32_Type | Types.Float_32_Type | Types.RGBA_Type =>
            Item.Set_Small_Value (C, O, 0);
         when Types.Int_64_Type =>
            Container.Set_Integer_64 (C, Value_Offset, 0);
         when Types.UInt_64_Type =>
            Container.Set_Unsigned_64 (C, Value_Offset, 0);
         when Types.Float_64_Type =>
            Container.Set_Float_64 (C, Value_Offset, 0.0);
         when Types.String_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
         when Types.Crc_String_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 4, 0);
         when Types.Binary_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
         when Types.Crc_Binary_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 4, 0);
         when Types.Array_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 4, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 8, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 12, 0);
         when Types.Dictionary_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 4, 0);
         when Types.Sequence_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 4, 0);
         when Types.Table_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 4, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 8, 0);
            Container.Set_Unsigned_32 (C, Value_Offset + 12, 0);
         when Types.UUID_Type =>
            Container.Set_Unsigned_64 (C, Value_Offset, 0);
         when Types.Font_Type =>
            Container.Set_Unsigned_32 (C, Value_Offset, 0);
      end case;

   end Create_Item_Layout;

   
   function Get_Type (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Type is
   begin
      return Types.To_Item_Type (Container.Get_Unsigned_8 (C, Item_Offset + Type_Offset));
   end Get_Type;
   
   
   function Get_Options (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Options is
   begin
      return Types.To_Item_Options (Container.Get_Unsigned_8 (C, Item_Offset + Options_Offset));
   end Get_Options;
   
   
   function Get_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Flags is
   begin
      return Types.To_Item_Flags (Container.Get_Unsigned_8 (C, Item_Offset + Flags_Offset));
   end Get_Flags;
   
   
   function Get_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (C, Item_Offset + Name_Field_Byte_Count_Offset);
   end Get_Name_Field_Byte_Count;
   
   
   function Get_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (C, Item_Offset + Byte_Count_Offset);
   end Get_Byte_Count;
   
   
   function Get_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (C, Item_Offset + Small_Value_Offset);
   end Get_Small_Value;
   
   
   function Get_Parent_Offset (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (C, Item_Offset + Parent_Offset_Offset);
   end Get_Parent_Offset;
   
   
   function Get_Name_CRC (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (C, Item_Offset + Name_Field_CRC_Offset);
   end Get_Name_CRC;
   
   
   function Get_Name_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (C, Item_Offset + Name_Field_ASCII_Byte_Count_Offset);
   end Get_Name_Byte_Count;
   
   
   function Get_Name_String (C: in out Container.Instance; Item_Offset: Unsigned_32) return String is
      Length: Unsigned_32 := Unsigned_32 (Get_Name_Byte_Count (C, Item_Offset));
   begin
      return Container.Get_String (C, Item_Offset + Name_Field_ASCII_Code_Offset, Length);
   end Get_Name_String;
   
   
   function Get_Name_Quick_Check_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (C, Item_Offset + Name_Field_CRC_Offset);
   end Get_Name_Quick_Check_Value;
   
   
   procedure Set_Type (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Type) is
   begin
      Container.Set_Unsigned_8 (C, Item_Offset + Type_Offset, Types.To_Unsigned_8 (Value));
   end Set_Type;

   
   procedure Set_Options (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Options) is
   begin
      Container.Set_Unsigned_8 (C, Item_Offset + Options_Offset, Types.To_Unsigned_8 (Value));
   end Set_Options;

   
   procedure Set_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Flags) is
   begin
      Container.Set_Unsigned_8 (C, Item_Offset + Flags_Offset, Types.To_Unsigned_8 (Value));
   end Set_Flags;

   
   procedure Set_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (C, Item_Offset + Name_Field_Byte_Count_Offset, Value);
   end Set_Name_Field_Byte_Count;

   
   procedure Set_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (C, Item_Offset + Byte_Count_Offset, Value);
   end Set_Byte_Count;

   
   procedure Set_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (C, Item_Offset + Small_Value_Offset, Value);
   end Set_Small_Value;

   
   procedure Set_Parent_Offset (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (C, Item_Offset + Parent_Offset_Offset, Value);
   end Set_Parent_Offset;

   
   procedure Set_Name (C: Container.Instance; Item_Offset: Unsigned_32; Value: Name_Field_Assistent.Instance) is
   begin
      if Value.Name_Field_Byte_Count > 0 then
         Container.Set_Unsigned_8 (C, Item_Offset + Name_Field_Byte_Count_Offset, Value.Name_Field_Byte_Count);
         Container.Set_Unsigned_16 (C, Item_Offset + Name_Field_CRC_Offset, Value.CRC);
         Container.Set_Unsigned_8 (C, Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Unsigned_8 (Value.Ascii_Code'Length));
         Container.Set_Unsigned_8_Array (C, Item_Offset + Name_Field_ASCII_Code_Offset, Value.Ascii_Code (1 .. Unsigned_32 (Value.Ascii_Byte_Count)));
      end if;
   end Set_Name;
   
   
   procedure Set_Name_CRC (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (C, Item_Offset + Name_Field_CRC_Offset, Value);
   end Set_Name_CRC;

   
   procedure Set_Name_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (C, Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Value);
   end Set_Name_Byte_Count;

   
   procedure Set_Name_String (C: Container.Instance; Item_Offset: Unsigned_32; Value: String) is
   begin
      Container.Set_String (C, Item_Offset + Name_Field_ASCII_Code_Offset, Value);
   end Set_Name_String;
      
end BRBON.Item;

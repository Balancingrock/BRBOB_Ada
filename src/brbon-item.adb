with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with CRC_Package;

with BRBON.Utils;


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
   --
   Name_Field_CRC_Offset: constant Unsigned_32 := 16;
   Name_Field_ASCII_Byte_Count_Offset: constant Unsigned_32 := 18;
   Name_Field_ASCII_Code_Offset: constant Unsigned_32 := 19;        -- Up to 248 bytes


   -- Internal specifications

   procedure Assign_Item_Name (C: in out Container.Instance; O: Unsigned_32; N: Name_Field_Assistent);
   
   procedure Create_Null_Type (C: in out Container.Instance; O: Unsigned_32; N: Name_Field_Assistent; B: Unsigned_32; P: Unsigned_32);
   


   -- ==========================================================================
   -- API
   -- ==========================================================================

   
   procedure Create_Item
    (
     Of_Type: Types.Item_Type;
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
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

   begin

      case T is
         when Types.Illegal =>
            Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "");
         when Types.Null_Type =>
            Create_Null_Type (C, O, N, B, P);
         when others =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Not implemented yet");
      end case;

   end Create_Item;

   
   function Create_Name_Field_Assistent (S: String) return Name_Field_Assistent is

      Assistent: Name_Field_Assistent (S'Length);
      Index: Unsigned_32 := 1;
      
   begin
      
      -- Don't accept too long names
      --
      if S'Length > Types.Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Types.Max_Name_Length'Image & ")");
      end if;

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
      if S'Length > 0 then
         Assistent.Name_Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + S'Length);
      else
         Assistent.Name_Field_Byte_Count := 0;
      end if;

      return Assistent;

   end Create_Name_Field_Assistent;

   
   function Item_Byte_Count_For_Minimum_Length_Payload (T: Types.Item_Type; N: Name_Field_Assistent) return Unsigned_32 is
      Byte_Count: Unsigned_32 := Types.Minimum_Item_Byte_Count;
   begin
      Byte_Count := Byte_Count + Unsigned_32 (N.Name_Field_Byte_Count);
      case T is
         when Types.Illegal => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot determine minimum byte count for illegal item-type.");
         when Types.Null_Type => Byte_Count := Byte_Count + 0;
         when Types.Bool_Type => Byte_Count := Byte_Count + 0;
         when Types.Int_8_Type => Byte_Count := Byte_Count + 0;
         when Types.Int_16_Type => Byte_Count := Byte_Count + 0;
         when Types.Int_32_Type => Byte_Count := Byte_Count + 0;
         when Types.Int_64_Type => Byte_Count := Byte_Count + 8;
         when Types.UInt_8_Type => Byte_Count := Byte_Count + 0;
         when Types.UInt_16_Type => Byte_Count := Byte_Count + 0;
         when Types.UInt_32_Type => Byte_Count := Byte_Count + 0;
         when Types.UInt_64_Type => Byte_Count := Byte_Count + 8;
         when Types.Float_32_Type => Byte_Count := Byte_Count + 0;
         when Types.Float_64_Type => Byte_Count := Byte_Count + 8;
         when Types.String_Type => Byte_Count := Byte_Count + 8;
         when Types.Crc_String_Type => Byte_Count := Byte_Count + 16;
         when Types.Binary_Type => Byte_Count := Byte_Count + 8;
         when Types.Crc_Binary_Type => Byte_Count := Byte_Count + 16;
         when Types.Array_Type => Byte_Count := Byte_Count + 24;
         when Types.Dictionary_Type => Byte_Count := Byte_Count + 16;
         when Types.Sequence_Type => Byte_Count := Byte_Count + 16;
         when Types.Table_Type => Byte_Count := Byte_Count + 40;
         when Types.UUID_Type => Byte_Count := Byte_Count + 16;
         when Types.RGBA_Type => Byte_Count := Byte_Count + 0;
         when Types.Font_Type => Byte_Count := Byte_Count + 16;
      end case;
      return Byte_Count;            
   end Item_Byte_Count_For_Minimum_Length_Payload;
   
   
   -- ==========================================================================
   -- Internal bodies
   -- ==========================================================================

   
   procedure Assign_Item_Name (C: in out Container.Instance; O: Unsigned_32; N: Name_Field_Assistent) is
   begin
      C.Set_Unsigned_8 (O + Name_Field_Byte_Count_Offset, N.Name_Field_Byte_Count);
      C.Set_Unsigned_16 (O + Name_Field_CRC_Offset, N.CRC);
      C.Set_Unsigned_8 (O + Name_Field_ASCII_Byte_Count_Offset, N.Ascii_Code'Length);
      C.Set_Unsigned_8_Array (O + Name_Field_ASCII_Code_Offset, N.Ascii_Code);
   end Assign_Item_Name;
   
   
   procedure Create_Null_Type
    (
     C: in out Container.Instance;
     O: Unsigned_32;
     N: Name_Field_Assistent;
     B: Unsigned_32;
     P: Unsigned_32
    ) is
    
   begin
      C.Set_Item_Type (O + Type_Offset, Types.Null_Type);
      C.Set_Item_Options (O + Options_Offset, Types.No_Item_Options);
      C.Set_Item_Flags (O + Flags_Offset, Types.No_Item_Flags);
      C.Set_Unsigned_8 (O + Name_Field_Byte_Count_Offset, N.Name_Field_Byte_Count);
      C.Set_Unsigned_32 (O + Byte_Count_Offset, B);
      C.Set_Unsigned_32 (O + Parent_Offset_Offset, P);
      C.Set_Unsigned_32 (O + Small_Value_Offset, 0);
   end Create_Null_Type;

end BRBON.Item;

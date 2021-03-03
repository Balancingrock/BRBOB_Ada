with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.UTF_Encoding;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Interfaces.C.Pointers;

with BRBON; use BRBON;
with Storage_Area; use Storage_Area;
with UUID_Package; use UUID_Package;
with Color_Package; use Color_Package;
with Font_Package; use Font_Package;
with Pointer_Math; use Pointer_Math;

package Item is


   -- Item header offsets and sizes
   --
   Item_Type_Offset: constant Unsigned_32 := 0;                   -- 1 byte
   Item_Options_Offset: constant Unsigned_32 := 1;                -- 1 byte
   Item_Flags_Offset: constant Unsigned_32 := 2;                  -- 1 byte
   Item_Name_Field_Byte_Count_Offset: constant Unsigned_32 := 3;  -- 1 byte
   Item_Byte_Count_Offset: constant Unsigned_32 := 4;             -- 4 bytes
   Item_Parent_Offset_Offset: constant Unsigned_32 := 8;          -- 4 bytes
   Item_Small_Value: constant Unsigned_32 := 12;                  -- 4 bytes
                                                                  --
   Item_Header_Byte_Count: constant Unsigned_32 := 16;            --
                                                                  --
   Item_Name_CRC_16_Offset: constant Unsigned_32 := 16;           -- 2 bytes
   Item_Name_Byte_Count_Offset: constant Unsigned_32 := 19;       -- 1 byte
   Item_Name_ASCII_Code_Offset: constant Unsigned_32 := 20;       -- Up to 248 bytes.


   type Item_Name_Assistent (String_Length: Unsigned_32) is private;

   function Create_Item_Name_Assistent (S: BR_Item_Name) return Item_Name_Assistent;


   -- Item Access
   --
   type Item_Access is tagged
      record
         Storage: Storage_Area_Ptr;
         Offset: Unsigned_32;
      end record;


   -- =========================
   -- Offsets for value access
   -- =========================

   function Small_Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + 12);
   pragma Inline (Small_Value_Offset);

   function Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + 16 + Unsigned_32 (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count_Offset)));
   pragma Inline (Value_Offset);


   -- ======================
   -- Header common access
   -- ======================

   procedure Assign_Item_Type    (I: Item_Access'Class; Value: BR_Item_Type); pragma Inline (Assign_Item_Type);
   function Is_Valid_Item_Type   (I: Item_Access'Class) return Boolean is (I.Storage.Valid_Item_Type (I.Offset + Item_Type_Offset)); pragma Inline (Is_Valid_Item_Type);
   function Item_Type            (I: Item_Access'Class) return BR_Item_Type is (I.Storage.Get_Item_Type (I.Offset + Item_Type_Offset)); pragma Inline (Item_Type);

   procedure Assign_Item_Options (I: Item_Access'Class; Value: BR_Item_Options); pragma Inline (Assign_Item_Options);
   function Item_Options         (I: Item_Access'Class) return BR_Item_Options is (I.Storage.Get_Item_Options (I.Offset + Item_Options_Offset)); pragma Inline (Item_Options);

   procedure Assign_Item_Flags   (I: Item_Access'Class; Value: BR_Item_Flags); pragma Inline (Assign_Item_Flags);
   function Item_Flags           (I: Item_Access'Class) return BR_Item_Flags is (I.Storage.Get_Item_Flags (I.Offset + Item_Flags_Offset)); pragma Inline (Item_Flags);

   procedure Assign_Item_Name_Field_Byte_Count (I: Item_Access'Class; Value: Unsigned_8); pragma Inline (Assign_Item_Name_Field_Byte_Count);
   function Item_Name_Field_Byte_Count         (I: Item_Access'Class) return Unsigned_8 is (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count_Offset)); pragma Inline (Item_Name_Field_Byte_Count);

   procedure Assign_Item_Byte_Count    (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Assign_Item_Byte_Count);
   function Item_Byte_Count            (I: Item_Access'Class) return Unsigned_32 is (I.Storage.Get_Unsigned_32 (I.Offset + Item_Byte_Count_Offset)); pragma Inline (Item_Byte_Count);

   procedure Assign_Item_Parent_Offset (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Assign_Item_Parent_Offset);
   function Item_Parent_Offset         (I: Item_Access'Class) return Unsigned_32 is (I.Storage.Get_Unsigned_32 (I.Offset + Item_Parent_Offset_Offset)); pragma Inline (Item_Parent_Offset);


   -- ======================
   -- Item name access
   -- ======================

   procedure Assign_Item_Name          (I: Item_Access'Class; Value: BR_Item_Name);
   function Item_Name                  (I: Item_Access'Class) return String;
   function Item_Name_CRC              (I: Item_Access'Class) return Unsigned_16 is (I.Storage.Get_Unsigned_16 (I.Offset + Item_Name_CRC_16_Offset)); pragma Inline (Item_Name_CRC);
   function Item_Name_Byte_Count       (I: Item_Access'Class) return Unsigned_8 is (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Byte_Count_Offset)); pragma Inline (Item_Name_CRC);


   -- ===============
   -- Item Types
   -- ===============

   procedure Create_Null_Item         (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Boolean_Value             (I: Item_Access'Class) return Boolean is (I.Storage.Get_Bool (I.Small_Value_Offset)); pragma Inline (Boolean_Value);
   procedure Assign_Boolean_Value     (I: Item_Access'Class; Value: Boolean); pragma Inline (Assign_Boolean_Value);
   procedure Create_Boolean_Item      (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False);

   function Integer_8_Value           (I: Item_Access'Class) return Integer_8 is (I.Storage.Get_Integer_8 (I.Small_Value_Offset)); pragma Inline (Integer_8_Value);
   procedure Assign_Integer_8_Value   (I: Item_Access'Class; Value: Integer_8); pragma Inline (Assign_Integer_8_Value);
   procedure Create_Integer_8_Item    (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0);

   function Integer_16_Value          (I: Item_Access'Class) return Integer_16; pragma Inline (Integer_16_Value);
   procedure Assign_Integer_16_Value  (I: Item_Access'Class; Value: Integer_16); pragma Inline (Assign_Integer_16_Value);
   procedure Create_Integer_16_Item   (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0);

   function Integer_32_Value          (I: Item_Access'Class) return Integer_32; pragma Inline (Integer_32_Value);
   procedure Assign_Integer_32_Value  (I: Item_Access'Class; Value: Integer_32); pragma Inline (Assign_Integer_32_Value);
   procedure Create_Integer_32_Item   (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0);

   function Integer_64_Value          (I: Item_Access'Class) return Integer_64; pragma Inline (Integer_64_Value);
   procedure Assign_Integer_64_Value  (I: Item_Access'Class; Value: Integer_64); pragma Inline (Assign_Integer_64_Value);
   procedure Create_Integer_64_Item   (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0);

   function Unsigned_8_Value          (I: Item_Access'Class) return Unsigned_8; pragma Inline (Unsigned_8_Value);
   procedure Assign_Unsigned_8_Value  (I: Item_Access'Class; Value: Unsigned_8); pragma Inline (Assign_Unsigned_8_Value);
   procedure Create_Unsigned_8_Item   (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0);

   function Unsigned_16_Value         (I: Item_Access'Class) return Unsigned_16; pragma Inline (Unsigned_16_Value);
   procedure Assign_Unsigned_16_Value (I: Item_Access'Class; Value: Unsigned_16); pragma Inline (Assign_Unsigned_16_Value);
   procedure Create_Unsigned_16_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0);

   function Unsigned_32_Value         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Unsigned_32_Value);
   procedure Assign_Unsigned_32_Value (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Assign_Unsigned_32_Value);
   procedure Create_Unsigned_32_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0);

   function Unsigned_64_Value         (I: Item_Access'Class) return Unsigned_64; pragma Inline (Unsigned_64_Value);
   procedure Assign_Unsigned_64_Value (I: Item_Access'Class; Value: Unsigned_64); pragma Inline (Assign_Unsigned_64_Value);
   procedure Create_Unsigned_64_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0);

   function Float_32_Value            (I: Item_Access'Class) return IEEE_Float_32; pragma Inline (Float_32_Value);
   procedure Assign_Float_32_Value    (I: Item_Access'Class; Value: IEEE_Float_32); pragma Inline (Assign_Float_32_Value);
   procedure Create_Float_32_Item     (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0);

   function Float_64_Value            (I: Item_Access'Class) return IEEE_Float_64; pragma Inline (Float_64_Value);
   procedure Assign_Float_64_Value    (I: Item_Access'Class; Value: IEEE_Float_64); pragma Inline (Assign_Float_64_Value);
   procedure Create_Float_64_Item     (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0);


   String_Value_Byte_Count_Offset: constant Unsigned_32 := 0;   -- 4 bytes
   String_Value_UTF8_Code_Offset: constant Unsigned_32 := 4;    -- N bytes
   --
   function String_Value         (I: Item_Access'Class) return String; pragma Inline (String_Value);
   function String_Byte_Count    (I: Item_Access'Class) return Unsigned_32; pragma Inline (String_Byte_Count);
   procedure Assign_String_Value (I: Item_Access'Class; Value: String); pragma Inline (Assign_String_Value);
   procedure Create_String_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");


   CRC_String_Value_CRC_Offset: constant Unsigned_32 := 0;         -- 4 bytes
   CRC_String_Value_Byte_Count_Offset: constant Unsigned_32 := 4;  -- 4 bytes
   CRC_String_Value_UTF8_Code_Offset: constant Unsigned_32 := 8;   -- N bytes
   --
   function CRC_String_CRC           (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_String_CRC);
   function CRC_String_Byte_Count    (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_String_Byte_Count);
   function CRC_String_Value         (I: Item_Access'Class) return String; pragma Inline (CRC_String_Value);
   procedure Assign_CRC_String_Value (I: Item_Access'Class; Value: String); pragma Inline (Assign_CRC_String_Value);
   procedure Create_CRC_String_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");


   Binary_Value_Byte_Count_Offset: constant Unsigned_32 := 0;   -- 4 bytes
   Binary_Value_Bytes_Offset: constant Unsigned_32 := 4;        -- N bytes
   --
   function Binary_Value         (I: Item_Access'Class) return Array_Of_Unsigned_8; pragma Inline (Binary_Value);
   function Binary_Byte_Count    (I: Item_Access'Class) return Unsigned_32; pragma Inline (Binary_Byte_Count);
   procedure Assign_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Assign_Binary_Value);
   procedure Create_Binary_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);


   CRC_Binary_Value_CRC_Offset: constant Unsigned_32 := 0;         -- 4 bytes
   CRC_Binary_Value_Byte_Count_Offset: constant Unsigned_32 := 4;  -- 4 bytes
   CRC_Binary_Value_Bytes_Offset: constant Unsigned_32 := 8;       -- N bytes
   --
   function CRC_Binary_CRC            (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_Binary_CRC);
   function CRC_Binary_Byte_Count     (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_Binary_Byte_Count);
   function CRC_Binary_Value          (I: Item_Access'Class) return Array_Of_Unsigned_8; pragma Inline (CRC_Binary_Value);
   procedure Assign_CRC_Binary_Value  (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Assign_CRC_Binary_Value);
   procedure Create_CRC_Binary_Item   (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);


   Array_Value_Reserved_32_Offset: constant Unsigned_32 := 0;          -- 4 bytes
   Array_Value_Element_Type_Offset: constant Unsigned_32 := 4;         -- 1 bytes
   Array_Value_Reserved_8_Offset: constant Unsigned_32 := 5;           -- 1 byte
   Array_Value_Reserved_16_Offset: constant Unsigned_32 := 6;          -- 2 bytes
   Array_Value_Element_Count_Offset: constant Unsigned_32 := 8;        -- 4 bytes
   Array_Value_Element_Byte_Count_Offset: constant Unsigned_32 := 12;  -- 4 bytes
   Array_Value_Element_Base_Offset: constant Unsigned_32 := 16;        -- N bytes
   --
   function Array_Element_Type             (I: Item_Access'Class) return BR_Item_Type; pragma Inline (Array_Element_Type);
   function Array_Element_Count            (I: Item_Access'Class) return Unsigned_32; pragma Inline (Array_Element_Count);
   function Array_Element_Byte_Count       (I: Item_Access'Class) return Unsigned_32; pragma Inline (Array_Element_Byte_Count);
   function Array_Element_Offset           (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32; pragma Inline (Array_Element_Offset);
   procedure Set_Array_Element_Count       (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Count);
   procedure Set_Array_Element_Byte_Count  (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Byte_Count);
   procedure Create_Array_Item             (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128);


   Dictionary_Value_Reserved_32_Offset: constant Unsigned_32 := 0;   -- 4 bytes
   Dictionary_Value_Count_Offset: constant Unsigned_32 := 4;         -- 4 bytes
   Dictionary_Value_Items_Base_Offset: constant Unsigned_32 := 8;    -- N bytes.
   --
   function Dictionary_Item_Count      (I: Item_Access'Class) return Unsigned_32; pragma Inline (Dictionary_Item_Count);
   procedure Set_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Dictionary_Item_Count);
   procedure Create_Dictionary_Item    (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);


   Sequence_Value_Reserved_32_Offset: constant Unsigned_32 := 0;   -- 4 bytes
   Sequence_Value_Count_Offset: constant Unsigned_32 := 4;         -- 4 bytes
   Sequence_Value_Items_Base_Offset: constant Unsigned_32 := 8;    -- N bytes.
   --
   function Sequence_Item_Count      (I: Item_Access'Class) return Unsigned_32; pragma Inline (Sequence_Item_Count);
   procedure Set_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Sequence_Item_Count);
   procedure Create_Sequence_Item    (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);


   Table_Value_Row_Count_Offset: constant Unsigned_32 := 0;                 -- 4 bytes
   Table_Value_Column_Count_Offset: constant Unsigned_32 := 4;              -- 4 bytes
   Table_Value_Fields_Offset: constant Unsigned_32 := 8;                    -- 4 bytes
   Table_Value_Row_Byte_Count_Offset: constant Unsigned_32 := 12;           -- 4 bytes
   Table_Value_Column_Descriptor_Base_Offset: constant Unsigned_32 := 16;   -- N bytes
   --
   Table_Column_Descriptor_Name_CRC_16_Offset: constant Unsigned_32 := 0;           -- 2 bytes
   Table_Column_Descriptor_Name_Field_Byte_Count_Offset: constant Unsigned_32 := 2; -- 1 byte
   Table_Column_Descriptor_Field_Type_Offset: constant Unsigned_32 := 3;            -- 1 byte
   Table_Column_Descriptor_Name_Field_Offset_Offset: constant Unsigned_32 := 4;     -- 4 bytes
   Table_Column_Descriptor_Field_Offset_Offset: constant Unsigned_32 := 8;          -- 4 bytes
   Table_Column_Descriptor_Field_Byte_Count_Offset: constant Unsigned_32 := 12;     -- 4 bytes
   --
   Table_Column_Descriptor_Byte_Count: constant Unsigned_32 := 16;
   --
   Table_Column_Name_Field_ASCII_Byte_Count_Offset: constant Unsigned_32 := 0;  -- 1 byte
   Table_Column_Name_Field_Reserved_8_Offset: constant Unsigned_32 := 1;        -- 1 byte
   Table_Column_Name_Field_Reserved_16_Offset: constant Unsigned_32 := 2;       -- 2 byte
   Table_Column_Name_Field_ASCII_Code_Offset: constant Unsigned_32 := 4;        -- N bytes
   --
   function Table_Row_Count                    (I: Item_Access'Class) return Unsigned_32; pragma Inline (Table_Row_Count);
   function Table_Column_Count                 (I: Item_Access'Class) return Unsigned_32; pragma Inline (Table_Column_Count);
   function Table_Column_Descriptor_Offset     (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32; pragma Inline (Table_Column_Descriptor_Offset);
   function Table_Column_Name_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8; pragma Inline (Table_Column_Name_Field_Byte_Count);
   function Table_Column_Name_CRC              (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_16; pragma Inline (Table_Column_Name_CRC);
   function Table_Column_Name                  (I: Item_Access'Class; Column_Index: Unsigned_32) return String; pragma Inline (Table_Column_Name);
   function Table_Column_Type                  (I: Item_Access'Class; Column_Index: Unsigned_32) return BR_Item_Type; pragma Inline (Table_Column_Type);
   function Table_Field_Offset                 (I: Item_Access'Class; Column_Index: Unsigned_32; Row_Index: Unsigned_32) return Unsigned_32;
   procedure Set_Table_Row_Count               (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Table_Row_Count);
   procedure Set_Table_Column_Count            (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Table_Column_Count);
   procedure Assign_Table_Column_Name          (I: Item_Access'Class; Column_Index: Unsigned_32; Value: String); pragma Inline (Assign_Table_Column_Name);
   procedure Create_Table_Item                 (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function UUID_Value                     (I: Item_Access'Class) return UUID; pragma Inline (UUID_Value);
   procedure Assign_UUID_Value             (I: Item_Access'Class; Value: UUID); pragma Inline (Assign_UUID_Value);
   procedure Create_UUID_Item              (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID);

   function Color_Value                    (I: Item_Access'Class) return Color; pragma Inline (Color_Value);
   procedure Assign_Color_Value            (I: Item_Access'Class; Value: Color); pragma Inline (Assign_Color_Value);
   procedure Create_Color_Item             (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black);

   function Font_Value                     (I: Item_Access'Class) return Font; pragma Inline (Font_Value);
   procedure Assign_Font_Value             (I: Item_Access'Class; Value: Font); pragma Inline (Assign_Font_Value);
   procedure Create_Font_Item              (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font);


   function Create_Item_Access  (S: Storage_Area_Ptr; O: Unsigned_32) return Item_Access;

private

   type Item_Name_Assistent (String_Length: Unsigned_32) is tagged
      record
         CRC_16: Unsigned_16;
         Ascii_Code: Array_Of_Unsigned_8 (1 .. String_Length);
         Quick_Check: Unsigned_32;
         Name_Field_Byte_Count: Unsigned_8;
      end record;

   function Ascii_Code_Count (Assistent: Item_Name_Assistent) return Unsigned_8 is (Unsigned_8 (Assistent.Ascii_Code'Length));
   pragma Inline (Ascii_Code_Count);


end Item;

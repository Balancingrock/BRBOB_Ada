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
   Item_Type_Offset: constant Integer := 0;                   -- 1 byte
   Item_Options_Offset: constant Integer := 1;                -- 1 byte
   Item_Flags_Offset: constant Integer := 2;                  -- 1 byte
   Item_Name_Field_Byte_Count_Offset: constant Integer := 3;  -- 1 byte
   Item_Byte_Count_Offset: constant Integer := 4;             -- 4 bytes
   Item_Parent_Offset_Offset: constant Integer := 8;          -- 4 bytes
   Item_Small_Value: constant Integer := 12;                  -- 4 bytes
                                                              --
   Item_Header_Byte_Count: constant Integer := 16;            --
                                                              --
   Item_Name_CRC_16_Offset: constant Integer := 16;           -- 2 bytes
   Item_Name_Byte_Count_Offset: constant Integer := 19;       -- 1 byte
   Item_Name_ASCII_Code_Offset: constant Integer := 20;       -- Up to 248 bytes.


   type Item_Name_Assistent (String_Length: Unsigned_32) is private;

   function Create_Item_Name_Assistent (S: Item_Name) return Item_Name_Assistent;


   -- Item Access
   --
   type Item_Access is tagged
      record
         Storage: Storage_Area_Ptr;
         Offset: Unsigned_32;
         Header_Ptr: Item_Header_Ptr;
      end record;


   -- =========================
   -- Offsets for value access
   -- =========================

   function Small_Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + 12);
   pragma Inline (Small_Value_Offset);

   function Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + 16 + Unsigned_32 (I.Header_Ptr.Name_Field_Byte_Count));
   pragma Inline (Value_Offset);


   -- ======================
   -- Header common access
   -- ======================

   procedure Assign_Item_Type    (I: Item_Access'Class; Value: BR_Item_Type); pragma Inline (Assign_Item_Type);
   function Is_Valid_Item_Type   (I: Item_Access'Class) return Boolean is (I.Storage.Valid_Item_Type (I.Offset + Item_Type_Offset)); pragma Inline (Is_Valid_Item_Type);
   function Item_Type            (I: Item_Access'Class) return BR_Item_Type is (I.Storage.Get_Item_Type (I.Offset + Item_Type_Offset)); pragma Inline (Item_Type);

   procedure Assign_Item_Options (I: Item_Access'Class; Value: BR_Item_Options); pragma Inline (Assign_Item_Options);
   function Item_Options         (I: Item_Access'Class) return BR_Item_Options is (I.Storage.Get_Item_Type (I.Offset + Item_Options_Offset)); pragma Inline (Item_Options);

   procedure Assign_Item_Flags   (I: Item_Access'Class; Value: BR_Item_Flags); pragma Inline (Assign_Item_Flags);
   function Item_Flags           (I: Item_Access'Class) return BR_Item_Flags is (I.Storage.Get_Item_Type (I.Offset + Item_Flags_Offset)); pragma Inline (Item_Flags);

   procedure Assign_Item_Name_Field_Byte_Count (I: Item_Access'Class; Value: Unsigned_8); pragma Inline (Assign_Item_Name_Field_Byte_Count);
   function Item_Name_Field_Byte_Count         (I: Item_Access'Class) return Unsigned_8 is (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Byte_Count_Offset)); pragma Inline (Item_Name_Field_Byte_Count);

   procedure Assign_Item_Byte_Count    (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Assign_Item_Byte_Count);
   function Item_Byte_Count            (I: Item_Access'Class) return Unsigned_32 is (I.Storage.Get_Unsigned_32 (I.Offset + Item_Byte_Count_Offset)); pragma Inline (Item_Byte_Count);

   procedure Assign_Item_Parent_Offset (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Assign_Item_Parent_Offset);
   function Item_Parent_Offset         (I: Item_Access'Class) return Unsigned_32 is (I.Storage.Get_Unsigned_32 (I.Offset + Item_Parent_Offset_Offset)); pragma Inline (Item_Parent_Offset);


   -- ======================
   -- Item name access
   -- ======================

   procedure Assign_Item_Name          (I: Item_Access'Class; Value: BR_Item_Name_Bounded_String);
   function Item_Name                  (I: Item_Access'Class) return String;
   function Item_Name_CRC              (I: Item_Access'Class) return Unsigned_16 is (I.Storage.Get_Unsigned_16 (I.Offset + Item_Name_CRC_16_Offset)); pragma Inline (Item_Name_CRC);
   function Item_Name_Byte_Count       (I: Item_Access'Class) return Unsigned_16 is (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Byte_Count_Offset)); pragma Inline (Item_Name_CRC);


   -- ===============
   -- Item Types
   -- ===============

   procedure Create_Null_Item         (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Boolean_Value             (I: Item_Access'Class) return Boolean is (I.Storage.Get_Bool (I.Small_Value_Offset)); pragma Inline (Boolean_Value);
   procedure Assign_Boolean_Value     (I: Item_Access'Class; Value: Boolean); pragma Inline (Assign_Boolean_Value);
   procedure Create_Boolean_Item      (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False);

   function Integer_8_Value           (I: Item_Access'Class) return Integer_8 is (I.Storage.Get_Integer_8 (I.Small_Value_Offset)); pragma Inline (Integer_8_Value);
   procedure Assign_Integer_8_Value   (I: Item_Access'Class; Value: Integer_8); pragma Inline (Assign_Integer_8_Value);
   procedure Create_Integer_8_Item    (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0);

   function Integer_16_Value          (I: Item_Access'Class) return Integer_16; pragma Inline (Integer_16_Value);
   procedure Assign_Integer_16_Value  (I: Item_Access'Class; Value: Integer_16); pragma Inline (Assign_Integer_16_Value);
   procedure Create_Integer_16_Item   (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0);

   function Integer_32_Value          (I: Item_Access'Class) return Integer_32; pragma Inline (Integer_32_Value);
   procedure Assign_Integer_32_Value  (I: Item_Access'Class; Value: Integer_32); pragma Inline (Assign_Integer_32_Value);
   procedure Create_Integer_Item      (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0);

   function Integer_64_Value          (I: Item_Access'Class) return Integer_64; pragma Inline (Integer_64_Value);
   procedure Assign_Integer_64_Value  (I: Item_Access'Class; Value: Integer_64); pragma Inline (Assign_Integer_64_Value);
   procedure Create_Integer_64_Item   (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0);

   function Unsigned_8_Value          (I: Item_Access'Class) return Unsigned_8; pragma Inline (Unsigned_8_Value);
   procedure Assign_Unsigned_8_Value  (I: Item_Access'Class; Value: Unsigned_8); pragma Inline (Assign_Unsigned_8_Value);
   procedure Create_Unsigned_8_Item   (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0);

   function Unsigned_16_Value         (I: Item_Access'Class) return Unsigned_16; pragma Inline (Unsigned_16_Value);
   procedure Assign_Unsigned_16_Value (I: Item_Access'Class; Value: Unsigned_16); pragma Inline (Assign_Unsigned_16_Value);
   procedure Create_Unsigned_16_Item  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0);

   function Unsigned_32_Value         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Unsigned_32_Value);
   procedure Assign_Unsigned_32_Value (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Assign_Unsigned_32_Value);
   procedure Create_Unsigned_32_Item  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0);

   function Unsigned_64_Value         (I: Item_Access'Class) return Unsigned_64; pragma Inline (Unsigned_64_Value);
   procedure Assign_Unsigned_64_Value (I: Item_Access'Class; Value: Unsigned_64); pragma Inline (Assign_Unsigned_64_Value);
   procedure Create_Unsigned_64_Item  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0);

   function Float_32_Value            (I: Item_Access'Class) return IEEE_Float_32; pragma Inline (Float_32_Value);
   procedure Assign_Float_32_Value    (I: Item_Access'Class; Value: IEEE_Float_32); pragma Inline (Assign_Float_32_Value);
   procedure Create_Float_32_Item     (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0);

   function Float_64_Value            (I: Item_Access'Class) return IEEE_Float_64; pragma Inline (Float_64_Value);
   procedure Assign_Float_64_Value    (I: Item_Access'Class; Value: IEEE_Float_64); pragma Inline (Assign_Float_64_Value);
   procedure Create_Float_64_Item     (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0);

   function String_Value              (I: Item_Access'Class) return String; pragma Inline (String_Value);
   function String_Byte_Count         (I: Item_Access'Class) return Unsigned_32; pragma Inline (String_Value_Byte_Count);
   procedure Assign_String_Value      (I: Item_Access'Class; Value: String); pragma Inline (Assign_String_Value);
   procedure Create_String_Item       (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");

   function CRC_String_Value          (I: Item_Access'Class) return String; pragma Inline (CRC_String_Value);
   function CRC_String_CRC            (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_String_CRC);
   function CRC_String_Byte_Count     (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_String_Byte_Count);
   procedure Assign_CRC_String_Value  (I: Item_Access'Class; Value: String); pragma Inline (Assign_CRC_String_Value);
   procedure Create_CRC_String_Item   (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");

   function Binary_Value              (I: Item_Access'Class) return Array_Of_Unsigned_8; pragma Inline (Binary_Value);
   function Binary_Byte_Count         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Binary_Byte_Count);
   procedure Assign_BR_Binary_Value   (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Assign_BR_Binary);
   procedure Create_BR_Binary_Item    (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);

   function CRC_Binary_Value          (I: Item_Access'Class) return Array_Of_Unsigned_8; pragma Inline (CRC_Binary_Value);
   function CRC_Binary_CRC            (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_Binary_CRC);
   function CRC_Binary_Byte_Count     (I: Item_Access'Class) return Unsigned_32; pragma Inline (CRC_Binary_Byte_Count);
   procedure Assign_CRC_Binary_Value  (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Assign_CRC_Binary_Value);
   procedure Create_CRC_Binary_Item   (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);

   function Array_Element_Type             (I: Item_Access'Class) return BR_Item_Type; pragma Inline (Array_Element_Type);
   function Array_Element_Count            (I: Item_Access'Class) return Unsigned_32; pragma Inline (Array_Element_Count);
   function Array_Element_Byte_Count       (I: Item_Access'Class) return Unsigned_32; pragma Inline (Array_Element_Byte_Count);
   function Array_Element_Offset           (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32; pragma Inline (Array_Element_Offset);
   procedure Set_Array_Element_Count       (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Count);
   procedure Set_Array_Element_Byte_Count  (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Byte_Count);
   procedure Create_Array_Item             (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128);

   function Dictionary_Item_Count          (I: Item_Access'Class) return Unsigned_32; pragma Inline (Dictionary_Item_Count);
   procedure Set_Dictionary_Item_Count     (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Dictionary_Item_Count);
   procedure Create_Dictionary_Item        (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Sequence_Item_Count            (I: Item_Access'Class) return Unsigned_32; pragma Inline (Sequence_Item_Count);
   procedure Set_Sequence_Item_Count       (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Sequence_Item_Count);
   procedure Create_Sequence_Item          (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Table_Row_Count                (I: Item_Access'Class) return Unsigned_32; pragma Inline (Table_Row_Count);
   function Table_Column_Count             (I: Item_Access'Class) return Unsigned_32; pragma Inline (Table_Column_Count);
   function Table_Column_Descriptor_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32; pragma Inline (Table_Column_Descriptor_Offset);
   function Table_Column_Name_Byte_Count   (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8; pragma Inline (Table_Column_Name_Byte_Count);
   function Table_Column_Name_CRC          (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_16; pragma Inline (Table_Column_Name_CRC);
   function Table_Column_Name              (I: Item_Access'Class; Column_Index: Unsigned_32) return String; pragma Inline (Table_Column_Name);
   function Table_Column_Type              (I: Item_Access'Class; Column_Index: Unsigned_32) return BR_Item_Type; pragma Inline (Table_Column_Type);
   function Table_Field_Offset             (I: Item_Access'Class; Column_Index: Unsigned_32; Row_Index: Unsigned_32) return Unsigned_32;
   procedure Set_Table_Row_Count           (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Table_Row_Count);
   procedure Set_Table_Column_Count        (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Table_Column_Count);
   procedure Assign_Table_Column_Name      (I: Item_Access'Class; Column_Index: Unsigned_32; Value: String); pragma Inline (Assign_Table_Column_Name);
   procedure Create_Table_Item             (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function BR_UUID                  (I: Item_Access'Class) return UUID); pragma Inline (BR_UUID);
   procedure Assign_BR_UUID           (I: Item_Access'Class; Value: UUID); pragma Inline (Assign_BR_UUID);
   procedure Create_BR_UUID           (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID);

   function BR_Color                  (I: Item_Access'Class) return Color; pragma Inline (BR_Color);
   procedure Assign_BR_Color          (I: Item_Access'Class; Value: Color); pragma Inline (Assign_BR_Color);
   procedure Create_BR_Color          (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black);

   function BR_Font                   (I: Item_Access'Class) return BR_Font; pragma Inline (BR_Font);
   procedure Assign_BR_Font           (I: Item_Access'Class; Value: Font); pragma Inline (Assign_BR_Font);
   procedure Create_BR_Font           (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font);

   function Get_Item_Small_Value_Ptr (I: Item_Access'Class) return Unsigned_8_Ptr is (I.Storage.Data.all(0)'Access + 12);
   pragma inline (Get_Item_Small_Value_Ptr);
   -- Sets the name of an item, does nothing if the name assistent is empty.
   -- Note: Does not check for any conditions, it will simply write to the proper indexes and update the Item Name_Field_Byte_Count.
   --
   procedure Set_Name (I: Item_Access'Class; Name: Item_Name_Assistent);

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

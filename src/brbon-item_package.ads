with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

with Color_Package; use Color_Package;
with CRC_Package; use CRC_Package;

with BRBON.Name_Field_Assistent_Package; use BRBON.Name_Field_Assistent_Package;


private package BRBON.Item_Package is


   -----------------------------------------------------------------------------
   -- Properties look up tables

   Item_Is_Container: Array (Item_Type) of Boolean :=
     (
      False,   -- Illegal
      False,   -- Null_Type
      False,   -- Bool_Type
      False,   -- Int_8_Type
      False,   -- Int_16_Type
      False,   -- Int_32_Type
      False,   -- Int_64_Type
      False,   -- UInt_8_Type
      False,   -- UInt_16_Type
      False,   -- UInt_32_Type
      False,   -- UInt_64_Type
      False,   -- Float_32_Type
      False,   -- Float_64_Type
      False,   -- String_Type
      False,   -- CRC_String_Type
      False,   -- Binary_Type
      False,   -- CRC_Binary_Type
      True,    -- Array_Type
      True,    -- Dictionary_Type
      True,    -- Sequence_Type
      True,    -- Table_Type
      False,   -- UUID_Type
      False,   -- RGBA_Type
      False    -- Font_Type
     );

   Item_Overhead_Byte_Count: Array (Item_Type) of Unsigned_32 :=
     (
      0,  -- Illegal
      0,  -- Null
      0,  -- Bool
      0,  -- Int_8
      0,  -- Int_16
      0,  -- Int_32
      8,  -- Int_64
      0,  -- UInt_8
      0,  -- UInt_16
      0,  -- UInt_32
      8,  -- UInt_64
      0,  -- Float_32
      8,  -- Float_64
      4,  -- String
      8,  -- CRC_String
      4,  -- Binary
      8,  -- CRC_Binary
      32, -- Array
      8,  -- Dictionary
      8,  -- Sequence
      16, -- Table (no column descriptors included)
      16, -- UUID
      0,  -- RGBA
      6   -- Font
     );


   -----------------------------------------------------------------------------
   -- The fixed part of an item

   Item_Header_Byte_Count: constant Unsigned_32 := 16;

   function Get_Item_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Header_Ptr is (To_Item_Header_Ptr (S.Data (Item_Offset)'Access));
   pragma Inline (Get_Item_Header_Ptr);

   function Item_Header_Get_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type;
   procedure Item_Header_Set_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type);

   function Item_Header_Get_Options (S: Store; Item_Offset: Unsigned_32) return Item_Options;
   procedure Item_Header_Set_Options (S: Store; Item_Offset: Unsigned_32; Value: Item_Options);

   function Item_Header_Get_Flags (S: Store; Item_Offset: Unsigned_32) return Item_Flags;
   procedure Item_Header_Set_Flags (S: Store; Item_Offset: Unsigned_32; Value: Item_Flags);

   function Item_Header_Get_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;
   procedure Item_Header_Set_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);

   function Item_Header_Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   procedure Item_Header_Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Header_Get_Parent_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   procedure Item_Header_Set_Parent_Offset (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);


   -----------------------------------------------------------------------------
   -- For small value access

   Small_Value_Offset: constant Unsigned_32 := 12;

   function Small_Value_Get_Bool (S: Store; Item_Offset: Unsigned_32) return Boolean;
   pragma Inline (Small_Value_Get_Bool);

   procedure Small_Value_Set_Bool (S: Store; Item_Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Small_Value_Set_Bool);

   function Small_Value_Get_Int8 (S: Store; Item_Offset: Unsigned_32) return Integer_8;
   pragma Inline (Small_Value_Get_Int8);

   procedure Small_Value_Set_Int8 (S: Store; Item_Offset: Unsigned_32; Value: Integer_8);
   pragma Inline (Small_Value_Set_Int8);

   function Small_Value_Get_UInt8 (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Small_Value_Get_UInt8);

   procedure Small_Value_Set_UInt8 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Small_Value_Set_UInt8);

   function Small_Value_Get_Int16 (S: Store; Item_Offset: Unsigned_32) return Integer_16;
   pragma Inline (Small_Value_Get_Int16);

   procedure Small_Value_Set_Int16 (S: Store; Item_Offset: Unsigned_32; Value: Integer_16);
   pragma Inline (Small_Value_Set_Int16);

   function Small_Value_Get_UInt16 (S: Store; Item_Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Small_Value_Get_UInt16);

   procedure Small_Value_Set_UInt16 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Small_Value_Set_UInt16);

   function Small_Value_Get_Int32 (S: Store; Item_Offset: Unsigned_32) return Integer_32;
   pragma Inline (Small_Value_Get_Int32);

   procedure Small_Value_Set_Int32 (S: Store; Item_Offset: Unsigned_32; Value: Integer_32);
   pragma Inline (Small_Value_Set_Int32);

   function Small_Value_Get_UInt32 (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Small_Value_Get_UInt32);

   procedure Small_Value_Set_UInt32 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Small_Value_Set_UInt32);

   function Small_Value_Get_Float32 (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32;
   pragma Inline (Small_Value_Get_Float32);

   procedure Small_Value_Set_Float32 (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32);
   pragma Inline (Small_Value_Set_Float32);

   function Small_Value_Get_Color (S: Store; Item_Offset: Unsigned_32) return Color;
   pragma Inline (Small_Value_Get_Color);

   procedure Small_Value_Set_Color (S: Store; Item_Offset: Unsigned_32; Value: Color);
   pragma Inline (Small_Value_Set_Color);


   -----------------------------------------------------------------------------
   -- Item Name Field (optional)

   Item_Name_Offset: constant Unsigned_32 := Item_Header_Byte_count;
   Item_Name_Ascii_Code_Offset: constant Unsigned_32 := Item_Name_Offset + 3;

   type Item_Name_Field is
      record
         CRC: Unsigned_16;
         Byte_Count: Unsigned_8;
         ASCII_Code: aliased Unsigned_8; -- Followed by up to 244 characters
      end record;

   for Item_Name_Field'Size use 16 + 8 + 8;

   for Item_Name_Field use
      record
         CRC        at 0 range 0..15;
         Byte_Count at 2 range 0..7;
         ASCII_Code at 3 range 0..7;
      end record;

   type Item_Name_Field_Ptr is access Item_Name_Field;

   function To_Item_Name_Field_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Name_Field_Ptr);

   function Get_Item_Name_Field_Offset (Item_Offset: Unsigned_32) return Unsigned_32 is ( Item_Offset + Item_Name_Offset );
   pragma Inline (Get_Item_Name_Field_Offset);

   function Get_Item_Name_Field_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Name_Field_Ptr is (To_Item_Name_Field_Ptr (S.Data (Get_Item_Name_Field_Offset (Item_Offset))'Access));
   pragma Inline (Get_Item_Name_Field_Ptr);

   function Item_Name_Get_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_16;

   procedure Item_Name_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_16);

   function Item_Name_Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is (Get_Item_Name_Field_Ptr (S, Item_Offset).Byte_Count);
   pragma Inline (Item_Name_Get_Byte_Count);

   procedure Item_Name_Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);

   function Item_Name_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String;

   procedure Item_Name_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String);


   -----------------------------------------------------------------------------
   -- Value access


   -- Returns the offset to the value field for > 32 bit (not small) values from the start of the item.
   --
   function Get_Value_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is (Item_Header_Byte_Count + Unsigned_32 (Item_Header_Get_Name_Field_Byte_Count (S, Item_Offset)));
   pragma Inline (Get_Value_Offset);


   -- Returns a pointer to the first byte of the value.
   --
   function Get_Value_Ptr (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Ptr is (S.Data (Item_Offset + Get_Value_Offset (S, Item_Offset))'Access);
   pragma Inline (Get_Value_Ptr);


   -----------------------------------------------------------------------------
   -- Item Value: String

   type Item_Value_String_Header is
      record
         Count: Unsigned_32;
      end record;

   for Item_Value_String_Header'Size use 32;

   for Item_Value_String_Header use
      record
         Count     at 0 range 0..31;
      end record;

   Item_Value_String_Header_Byte_Count: constant Unsigned_32 := 4;

   type Item_Value_String_Header_Ptr is access Item_Value_String_Header;

   function To_Item_Value_String_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_String_Header_Ptr);

   function Get_Item_Value_String_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_String_Header_Ptr is (To_Item_Value_String_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String;

   procedure Item_Value_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String);


   -----------------------------------------------------------------------------
   -- Item Value: CRC String

   type Item_Value_CRC_String_Header is
      record
         CRC: CRC_32;
         Count: Unsigned_32;
      end record;

   for Item_Value_CRC_String_Header'Size use 32 + 32;

   for Item_Value_CRC_String_Header use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
      end record;

   Item_Value_CRC_String_Header_Byte_count: constant Unsigned_32 := Unsigned_32 (Item_Value_CRC_String_Header'Size / 8);

   type Item_Value_CRC_String_Header_Ptr is access Item_Value_CRC_String_Header;

   function To_Item_Value_CRC_String_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_CRC_String_Header_Ptr);

   function Get_Item_Value_CRC_String_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_CRC_String_Header_Ptr is (To_Item_Value_CRC_String_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_CRC_String_Get_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_32;

   procedure Item_Value_CRC_String_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_32);

   function Item_Value_CRC_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_CRC_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_CRC_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String;

   procedure Item_Value_CRC_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String);


   -----------------------------------------------------------------------------
   -- Item Value: Binary

   type Item_Value_Binary_Header is
      record
         Count: Unsigned_32;
      end record;

   for Item_Value_Binary_Header'Size use 32;

   for Item_Value_Binary_Header use
      record
         Count at 0 range 0..31;
      end record;

   Item_Value_Binary_Header_Byte_count: constant Unsigned_32 := Unsigned_32 (Item_Value_Binary_Header'Size / 8);

   type Item_Value_Binary_Header_Ptr is access Item_Value_Binary_Header;

   function To_Item_Value_Binary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Binary_Header_Ptr);

   function Get_Item_Value_Binary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Binary_Header_Ptr is (To_Item_Value_Binary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array;

   procedure Item_Value_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array);


   -----------------------------------------------------------------------------
   -- Item Value: CRC Binary

   type Item_Value_CRC_Binary_Header is
      record
         CRC: CRC_32;
         Count: Unsigned_32;
      end record;

   for Item_Value_CRC_Binary_Header'Size use 32 + 32;

   for Item_Value_CRC_Binary_Header use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
      end record;

   Item_Value_CRC_Binary_Header_Byte_count: constant Unsigned_32 := Unsigned_32 (Item_Value_CRC_Binary_Header'Size / 8);

   type Item_Value_CRC_Binary_Header_Ptr is access Item_Value_CRC_Binary_Header;

   function To_Item_Value_CRC_Binary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_CRC_Binary_Header_Ptr);

   function Get_Item_Value_CRC_Binary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_CRC_Binary_Header_Ptr is (To_Item_Value_CRC_Binary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_CRC_Binary_Get_CRC (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_CRC_Binary_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_CRC_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_CRC_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_CRC_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array;

   procedure Item_Value_CRC_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array);


   -----------------------------------------------------------------------------
   -- Item Value: Array

   type Item_Value_Array_Header is
      record
         Reserved_1: Unsigned_32;
         Element_Type: Item_Type;
         Reserved_2: Unsigned_8;
         Reserved_3: Unsigned_16;
         Element_Count: Unsigned_32;
         Element_Byte_Count: Unsigned_32;
      end record;

   for Item_Value_Array_Header'Size use 32 + 8 + 8 + 16 + 32 + 32 + 8;

   for Item_Value_Array_Header use
      record
         Reserved_1         at 0  range 0..31;
         Element_Type       at 4  range 0..7;
         Reserved_2         at 5  range 0..7;
         Reserved_3         at 6  range 0..15;
         Element_Count      at 8  range 0..31;
         Element_Byte_Count at 12 range 0..31;
      end record;

   Item_Value_Array_Header_Byte_Count: constant Unsigned_32 := Item_Value_Array_Header'Size / 8;

   type Item_Value_Array_Header_Ptr is access Item_Value_Array_Header;

   function To_Item_Value_Array_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Array_Header_Ptr);

   function Get_Item_Value_Array_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Array_Header_Ptr is (To_Item_Value_Array_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Array_Get_Element_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type;

   procedure Item_Value_Array_Set_Element_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type);

   function Item_Value_Array_Get_Element_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Array_Set_Element_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Array_Get_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Array_Set_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Array_Get_First_Element_Offset (S:Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Sequence

   type Item_Value_Sequence_Header is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
      end record;

   for Item_Value_Sequence_Header'Size use 32 + 32;

   for Item_Value_Sequence_Header use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
      end record;

   Item_Value_Sequence_Header_Byte_Count: constant Unsigned_32 := Item_Value_Sequence_Header'Size / 8;

   type Item_Value_Sequence_Header_Ptr is access Item_Value_Sequence_Header;

   function To_Item_Value_Sequence_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Sequence_Header_Ptr);

   function Get_Item_Value_Sequence_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Sequence_Header_Ptr is (To_Item_Value_Sequence_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Sequence_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Sequence_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Sequence_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Dictionary

   type Item_Value_Dictionary_Header is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
      end record;

   for Item_Value_Dictionary_Header'Size use 32 + 32;

   for Item_Value_Dictionary_Header use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
      end record;

   Item_Value_Dictionary_Header_Byte_Count: constant Unsigned_32 := Item_Value_Dictionary_Header'Size / 8;

   type Item_Value_Dictionary_Header_Ptr is access Item_Value_Dictionary_Header;

   function To_Item_Value_Dictionary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Dictionary_Header_Ptr);

   function Get_Item_Value_Dictionary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Dictionary_Header_Ptr is (To_Item_Value_Dictionary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Dictionary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Dictionary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Dictionary_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Table

   type Item_Value_Table_Header is
      record
         Row_Count: Unsigned_32;
         Column_Count: Unsigned_32;
         Fields_Start: Unsigned_32;
         Row_Byte_Count: Unsigned_32;
      end record;

   for Item_Value_Table_Header'Size use 32 + 32 + 32 + 32;

   for Item_Value_Table_Header use
      record
         Row_Count at 0 range 0..31;
         Column_Count at 4 range 0..31;
         Fields_Start at 8 range 0..31;
         Row_Byte_Count at 12 range 0..31;
      end record;

   Item_Value_Table_Header_Byte_Count: constant Unsigned_32 := Item_Value_Table_Header'Size / 8;

   type Item_Value_Table_Header_Ptr is access Item_Value_Table_Header;

   function To_Item_Value_Table_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Table_Header_Ptr);

   function Get_Item_Value_Table_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Table_Header_Ptr is (To_Item_Value_Table_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   function Item_Value_Table_Get_Row_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Table_Set_Row_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Table_Get_Column_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Table_Set_Column_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Table_Get_Fields_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   function Item_Value_Table_Get_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Table_Set_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Table_Get_Column_Descriptor_Start_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Table - Column Descriptor

   type Column_Descriptor is
      record
         Name_CRC: Unsigned_16;
         Name_Byte_Count: Unsigned_8;
         Field_Type: Item_Type;
         Name_Offset: Unsigned_32;
         Field_Offset: Unsigned_32;
         Field_Byte_Count: Unsigned_32;
      end record;

   for Column_Descriptor'Size use 16 + 8 + 8 + 32 + 32 + 32;

   for Column_Descriptor use
      record
         Name_CRC         at 0  range 0..15;
         Name_Byte_Count  at 2  range 0..7;
         Field_Type       at 3  range 0..7;
         Name_Offset      at 4  range 0..31;
         Field_Offset     at 8  range 0..31;
         Field_Byte_Count at 12 range 0..31;
      end record;

   type Column_Descriptor_Ptr is access Column_Descriptor;

   function To_Column_Descriptor_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Column_Descriptor_Ptr);

   function Column_Descriptor_Get_Name_CRC (S: Store; Descriptor_Offset: Unsigned_32) return CRC_16;

   procedure Column_Descriptor_Set_Name_CRC (S: Store; Descriptor_Offset: Unsigned_32; Value: CRC_16);

   function Column_Descriptor_Get_Name_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_8;

   procedure Column_Descriptor_Set_Name_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_8);

   function Column_Descriptor_Get_Field_Type (S: Store; Descriptor_Offset: Unsigned_32) return Item_Type;

   procedure Column_Descriptor_Set_Field_Type (S: Store; Descriptor_Offset: Unsigned_32; Value: Item_Type);

   function Column_Descriptor_Get_Name_Offset (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Name_Offset (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_32);

   function Column_Descriptor_Get_Field_Offset (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Field_Offset (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_32);

   function Column_Descriptor_Get_Field_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Field_Byte_Count (S: Store; Descriptor_Offset: Unsigned_32; Value: Unsigned_32);


   -----------------------------------------------------------------------------
   -- Small Item Value: RGBA (Color)

   type Item_Value_Header_RGBA is
      record
         Red: Unsigned_8;
         Green: Unsigned_8;
         Blue: Unsigned_8;
         Alpha: Unsigned_8;
      end record;

   for Item_Value_Header_RGBA'Size use 32;

   for Item_Value_Header_RGBA use
      record
         Red   at 0 range 0..7;
         Green at 1 range 0..7;
         Blue  at 2 range 0..7;
         Alpha at 3 range 0..7;
      end record;

   type Item_Value_Header_RGBA_Ptr is access Item_Value_Header_RGBA;

   function To_Item_Value_Header_RGBA_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_RGBA_Ptr);


   -----------------------------------------------------------------------------
   -- Item Value: Font

   type Item_Value_Header_Font is
      record
         Size: IEEE_Float_32;
         Family_Name_Byte_Count: Unsigned_8;
         Font_Name_Byte_Count: Unsigned_8;
      end record;

   for Item_Value_Header_Font'Size use 32 + 8 + 8;

   for Item_Value_Header_Font use
      record
         Size at 0 range 0..31;
         Family_Name_Byte_Count at 4 range 0..7;
         Font_Name_Byte_count at 5 range 0..7;
      end record;

   type Item_Value_Header_Font_Ptr is access Item_Value_Header_Font;

   function To_Item_Value_Header_Font_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_Font_Ptr);

   function Item_Value_Font_Get_Size (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32;

   procedure Item_Value_Font_Set_Size (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32);

   function Item_Value_Font_Get_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Font_Set_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Item_Value_Font_Get_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Item_Value_Font_Set_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   -- Returns the offset of the first family name character relative to the start of the item
   --
   function Item_Value_Font_Get_Family_Name_Offset return Unsigned_32;

   -- Returns the offset of the first font name character relative to the start of the item
   --
   function Item_Value_Font_Get_Font_Name_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Creates the layout for the requested type in the container at the requested offset.

   procedure Create_Layout
    (
     S: Store;
     At_Offset: Unsigned_32;
     Of_Type: Item_Type;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );


   -----------------------------------------------------------------------------
   -- Create a new array layout at the given offset

   function Create_Array_Layout
     (
      S: Store;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent;
      For_Element_Type: Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal;


end BRBON.Item_Package;

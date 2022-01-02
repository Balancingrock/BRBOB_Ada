with Ada.Unchecked_Conversion;

with System;
with Interfaces; use Interfaces;

private with BRBON.Types;
with BRBON.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item_Package is


private

   -- ==========================================================================
   -- The fixed part of an item
   -- ==========================================================================

   Item_Header_Byte_Count: constant Unsigned_32 := 16;

   type Item_Header is
      record
         Type_Field: Item_Type;
         Options_Field: Item_Options;
         Flags_Field: Item_Flags;
         Name_Field_Byte_Count_Field: Unsigned_8;
         Byte_Count_Field: Unsigned_32;
         Parent_Offset_Field: Unsigned_32;
         Small_Value_Field: Unsigned_32;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Header'Size use Item_Header_Byte_Count * 8 + 8;

   for Item_Header use
      record
         Type_Field                  at 0  range 0..7;
         Options_Field               at 1  range 0..7;
         Flags_Field                 at 2  range 0..7;
         Name_Field_Byte_Count_Field at 3  range 0..7;
         Byte_Count_Field            at 4  range 0..31;
         Parent_Offset_Field         at 8  range 0..31;
         Small_Value_Field           at 12 range 0..31;
         Next_Byte                   at 16 range 0..8;
      end record;

   type Item_Header_Ptr is access Item_Header;

   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_Ptr);


   -- ==========================================================================
   -- Item Name Field (may or may not be present
   -- ==========================================================================

   type Item_Name is
      record
         CRC: Unsigned_16;
         Byte_Count: Unsigned_8;
         Next_Byte: aliased Unsigned_8; -- Followed by up to 244 characters
      end record;

   for Item_Name'Size use 4 * 8;

   for Item_Name use
      record
         CRC        at 0 range 0..15;
         Byte_Count at 2 range 0..7;
         Next_Byte  at 3 range 0..7;
      end record;

   type Item_Name_Ptr is access Item_Name;

   function To_Item_Name_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Name_Ptr);


   -- --------------------------------------------------------------------------
   -- Value Offsets, relative to value start
   -- --------------------------------------------------------------------------

   -- String Type, relative to value start
   --
   type Item_Value_Header_String is
      record
         Count: Unsigned_32;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Value_Header_String'Size use 32 + 8;

   for Item_Value_Header_String use
      record
         Count     at 0 range 0..31;
         Next_Byte at 4 range 0..7;
      end record;

   Item_Value_Header_String_Byte_Count: constant Unsigned_32 := 4;

   type Item_Value_Header_String_Ptr is access Item_Value_String;

   function To_Item_Value_Header_String_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_String_Ptr);


   -- CRC String Type, relative to value start
   --
   type Item_Value_Header_CRC_String is
      record
         CRC: Unsigned_32;
         Count: Unsigned_32;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Value_Header_CRC_String'Size use 32 + 32 + 8;

   for Item_Value_Header_CRC_String use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
         Next_Byte at 8 range 0..7;
      end record;

   type Item_Value_Header_CRC_String_Ptr is access Item_Value_Header_CRC_String;

   function To_Item_Value_Header_CRC_String_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_CRC_String);


   -- Binary, relative to value start
   --
   type Item_Value_Binary is
      record
         Count: Unsigned_32;
         Next_Byte: Unsigned_8;
      end record;

   for Item_Value_Binary'Size use 32 + 8;

   for Item_Value_Binary use
      record
         Count at 0 range 0..31;
         Next_Byte at 4 range 0..7;
      end record;

   type Item_Value_Binary_Ptr is access Item_Value_Binary;

   function To_Item_Value_Header_Binary_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_Binary_Ptr);


   -- CRC Binary Type, relative to value start
   --
   type Item_Value_CRC_Binary is
      record
         CRC: Unsigned_32;
         Count: Unsigned_32;
         Next_Byte: Unsigned_8;
      end record;

   for Item_Value_CRC_Binary'Size use 32 + 32 + 8;

   for Item_Value_CRC_Binary use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
         Next_Byte at 8 range 0..7;
      end record;

   type Item_Value_CRC_Binary_Ptr is access Item_Value_CRC_Binary;

   function To_Item_Value_Header_CRC_Binary_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_CRC_Binary_Ptr);


   -- Array, relative to value start
   --
   type Item_Value_Array is
      record
         Reserved_1: Unsigned_32;
         Element_Type: Item_Type;
         Reserved_2: Unsigned_8;
         Reserved_3: Unsigned_16;
         Element_Count: Unsigned_32;
         Element_Byte_Count: Unsigned_32;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Value_Array'Size use 32 + 8 + 8 + 16 + 32 + 32 + 8;

   for Item_Value_Array use
      record
         Reserved_1         at 0  range 0..31;
         Element_Type       at 4  range 0..7;
         Reserved_2         at 5  range 0..7;
         Reserved_3         at 6  range 0..15;
         Element_Count      at 8  range 0..31;
         Element_Byte_Count at 12 range 0..31;
         Next_Byte          at 16 range 0..7;
      end record;

   type Item_Value_Array_Ptr is access Item_Value_Array;

   function To_Item_Value_Array_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Array_Ptr);


   -- Sequence, relative to value start
   --
   type Item_Value_Sequence is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Value_Sequence'Size use 32 + 32 + 8;

   for Item_Value_Sequence use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
         Next_Byte at 8 range 0..7;
      end record;

   type Item_Value_Sequence_Ptr is access Item_Value_Sequence;

   function To_Item_Value_Sequence_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Sequence_Ptr);


   -- Dictionary, relative to value start
   --
   type Item_Value_Dictionary is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Value_Dictionary'Size use 32 + 32 + 8;

   for Item_Value_Dictionary use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
         Next_Byte at 8 range 0..7;
      end record;

   type Item_Value_Dictionary_Ptr is access Item_Value_Dictionary;

   function To_Item_Value_Dictionary_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Dictionary_Ptr);


   -- Table, relative to value start
   --
   Table_Row_Count_Offset: Unsigned_32 := 0;
   Table_Column_Count_Offset: Unsigned_32 := 4;
   Table_Fields_Start_Offset: Unsigned_32 := 8;
   Table_Row_Byte_Count_Offset: Unsigned_32 := 12;
   Table_Column_Descriptors_Start_Offset: Unsigned_32 := 16;
   type Item_Value_Table is
      record
         Row_Count: Unsigned_32;
         Column_Count: Unsigned_32;
         Fields_Start: Unsigned_32;
         Row_Byte_Count: Unsigned_32;
         Column_Descriptors_Start: aliased Unsigned_8;
      end record;

   for Item_Value_Table'Size use 32 + 32 + 32 + 32 + 8;

   for Item_Value_Table use
      record
         Row_Count at 0 range 0..31;
         Column_Count at 4 range 0..31;
         Fields_Start at 8 range 0..31;
         Row_Byte_Count at 12 range 0..31;
         Column_Descriptors_Start at 16 range 0..7;
      end record;


   -- Table - Column Descriptor, relative to start of descriptor
   --
   type Table_Column_Descriptor is
      record
         Name_CRC: Unsigned_16;
         Name_Byte_Count: Unsigned_8;
         Field_Type: Item_Type;
         Name_Offset: Unsigned_32;
         Field_Offset: Unsigned_32;
         Field_Byte_Count: Unsigned_32;
      end record;

   for Table_Column_Descriptor'Size use 16 + 8 + 8 + 32 + 32 + 32;

   for Table_Column_Descriptor use
      record
         Name_CRC         at 0  range 0..15;
         Name_Byte_Count  at 2  range 0..7;
         Field_Type       at 3  range 0..7;
         Name_Offset      at 4  range 0..31;
         Field_Offset     at 8  range 0..31;
         Field_Byte_Count at 12 range 0..31;
      end record;


   -- Color, relative to start of small-value
   --
   type Item_Value_RGBA is
      record
         Red: Unsigned_8;
         Green: Unsigned_8;
         Blue: Unsigned_8;
         Alpha: Unsigned_8;
      end record;

   for Item_Value_RGBA'Size use 32;

   for Item_Value_RGBA use
      record
         Red   at 0 range 0..7;
         Green at 1 range 0..7;
         Blue  at 2 range 0..7;
         Alpha at 3 range 0..7;
      end record;


   -- Font, relative to start of value
   --
   Font_Size_Offset: Unsigned_32 := 0;
   Font_Family_Byte_Count_Offset: Unsigned_32 := 4;
   Font_Name_Byte_Count_Offset: Unsigned_32 := 5;
   Font_Family_Byte_Code_Start_Offset: Unsigned_32 := 6;
   type Item_Value_Font is
      record
         Size: Float_32;
         Family_Name_Byte_Count: Unsigned_8;
         Font_Name_Byte_Count: Unsigned_8;
         Next_Byte: aliased Unsigned_8;
      end record;

   for Item_Value_Font'Size use 32 + 8 + 8 + 8;

   for Item_Value_Font use
      record
         Size at 0 range 0..31;
         Family_Name_Byte_Count at 4 range 0..7;
         Font_Name_Byte_count at 5 range 0..7;
         Next_Byte at 6 range 0..7;
      end record;

   type Item_Value_Font_Ptr is access Item_Value_Font;

   function To_Item_Value_Font_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Font_Ptr);


   -- Creates the layout for the requested type in the container at the requested offset.
   --
   procedure Create_Layout
    (
     S: Store;
     At_Offset: Unsigned_32;
     Of_Type: Item_Type;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );

   -- Returns the offset of the item value. This is either the small-value or the payload.
   -- Note: The offset returned is the offset from the beginning of the array!
   --
   function Value_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Value_Offset);


   -- Create a new array layout at the given offset
   --
   function Create_Array_Layout
     (
      S: Storer;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      For_Element_Type: Types.Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal.Instance;


   -- Fixed layout

   function Get_Type (Item_Ptr: Types.Unsigned_8_Ptr) return Types.Item_Type;
   pragma Inline (Get_Type);

   function Get_Options (S: Store; Item_Offset: Unsigned_32) return Types.Item_Options;
   pragma Inline (Get_Options);

   function Get_Flags (S: Store; Item_Offset: Unsigned_32) return Types.Item_Flags;
   pragma Inline (Get_Flags);

   function Get_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Name_Field_Byte_Count);

   function Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Byte_Count);

   function Get_Small_Value (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Small_Value);

   function Get_Parent_Offset (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Parent_Offset);

   procedure Set_Type (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Types.Item_Type);
   pragma Inline (Set_Type);

   procedure Set_Options (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Types.Item_Options);
   pragma Inline (Set_Options);

   procedure Set_Flags (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Types.Item_Flags);
   pragma Inline (Set_Flags);

   procedure Set_Name_Field_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Name_Field_Byte_Count);

   procedure Set_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Byte_Count);

   procedure Set_Small_Value (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Small_Value);

   procedure Set_Parent_Offset (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Parent_Offset);


   -- Name Field access

   procedure Set_Name (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Name_Field_Assistent.Instance);

   function Get_Name_Quick_Check_Value (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Name_Quick_Check_Value);

   function Get_Name_CRC (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_Name_CRC);

   function Get_Name_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Name_Byte_Count);

   function Get_Name_String (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return String;
   pragma Inline (Get_Name_String);

   procedure Set_Name_CRC (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Name_CRC);

   procedure Set_Name_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Name_Byte_Count);

   procedure Set_Name_String (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: String);
   pragma Inline (Set_Name_String);


end BRBON.Item_Package;

with Ada.Unchecked_Conversion;

with System;
with Interfaces; use Interfaces;

with BRBON.Types;
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
         Type_Field: Types.Item_Type;
         Options_Field: Types.Item_Options;
         Flags_Field: Types.Item_Flags;
         Name_Field_Byte_Count_Field: Unsigned_8;
         Byte_Count_Field: Unsigned_32;
         Parent_Offset_Field: Unsigned_32;
         Small_Value_Field: Unsigned_32;
      end record;

   for Item_Header'Size use Item_Header_Byte_Count * 8;

   for Item_Header use
      record
         Type_Field at 0 range 0..7;
         Options_Field at 1 range 0..7;
         Flags_Field at 2 range 0..7;
         Name_Field_Byte_Count_Field at 3 range 0..7;
         Byte_Count_Field at 4 range 0..31;
         Parent_Offset_Field at 8 range 0..31;
         Small_Value_Field at 12 range 0..31;
      end record;

   type Item_Header_Ptr is access Item_Header;

   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Types.Unsigned_8_Ptr, Item_Header_Ptr);


   -- ==========================================================================
   -- Item Name Field (may or may not be present
   -- ==========================================================================

   type Item_Name is
      record
         CRC_16: Unsigned_16;
         ASCII_Count: Unsigned_8;
         ASCII_Start: Unsigned_8; -- Followed by up to 244 characters
      end record;

   for Item_Name'Size use 4 * 8;

   for Item_Name use
      record
         CRC_16 at 0 range 0..15;
         ASCII_Count at 2 range 0..7;
         ASCII_Start at 3 range 0..7;
      end record;

   type Item_Name_Ptr is access Item_Name;



   -- --------------------------------------------------------------------------
   -- Value Offsets, relative to value start
   -- --------------------------------------------------------------------------

   -- String Type, relative to value start
   --
   String_Byte_Count_Offset: Unsigned_32 := 0;
   String_Byte_Code_Offset: Unsigned_32 := 4;

   -- CRC String Type, relative to value start
   --
   CRC_String_CRC_Offset: Unsigned_32 := 0;
   CRC_String_Byte_Count_Offset: Unsigned_32 := 4;
   CRC_String_Byte_Code_Offset: Unsigned_32 := 8;

   -- Binary, relative to value start
   --
   Binary_Byte_Count_Offset: Unsigned_32 := 0;
   Binary_Byte_Code_Offset: Unsigned_32 := 4;

   -- CRC Binary Type, relative to value start
   --
   CRC_Binary_CRC_Offset: Unsigned_32 := 0;
   CRC_Binary_Byte_Count_Offset: Unsigned_32 := 4;
   CRC_Binary_Byte_Code_Offset: Unsigned_32 := 8;

   -- Array, relative to value start
   --
   Array_Reserved_1_Offset: Unsigned_32 := 0;
   Array_Element_Type_Offset: Unsigned_32 := 4;
   Array_Reserved_2_Offset: Unsigned_32 := 5;
   Array_Reserved_3_Offset: Unsigned_32 := 6;
   Array_Element_Count_Offset: Unsigned_32 := 8;
   Array_Element_Byte_Count_Offset: Unsigned_32 := 12;
   Array_Element_Start_Offset: Unsigned_32 := 16;

   -- Sequence, relative to value start
   --
   Sequence_Reserved_Offset: Unsigned_32 := 0;
   Sequence_Item_Count_Offset: Unsigned_32 := 4;
   Sequence_Items_Start_Offset: Unsigned_32 := 8;

   -- Dictionary, relative to value start
   --
   Dictionary_Reserved_Offset: Unsigned_32 := 0;
   Dictionary_Item_Count_Offset: Unsigned_32 := 4;
   Dictionary_Items_Start_Offset: Unsigned_32 := 8;

   -- Table, relative to value start
   --
   Table_Row_Count_Offset: Unsigned_32 := 0;
   Table_Column_Count_Offset: Unsigned_32 := 4;
   Table_Fields_Start_Offset: Unsigned_32 := 8;
   Table_Row_Byte_Count_Offset: Unsigned_32 := 12;
   Table_Column_Descriptors_Start_Offset: Unsigned_32 := 16;

   -- Table - Column Descriptor, relative to start of descriptor
   --
   Table_Column_Descriptor_Name_CRC_Offset: Unsigned_32 := 0;
   Table_Column_Descriptor_Name_Field_Byte_Count_Offset: Unsigned_32 := 2;
   Table_Column_Descriptor_Field_Type_Offset: Unsigned_32 := 3;
   Table_Column_Descriptor_Name_Field_Offset_Offset: Unsigned_32 := 4;
   Table_Column_Descriptor_Field_Offset_Offset: Unsigned_32 := 8;
   Table_Column_Descriptor_Field_Byte_Count_Offset: Unsigned_32 := 12;

   -- Color, relative to start of small-value
   --
   Color_Red_Offset: Unsigned_32 := 0;
   Color_Green_Offset: Unsigned_32 := 1;
   Color_Blue_Offset: Unsigned_32 := 2;
   Color_Alpha_Offset: Unsigned_32 := 3;

   -- Font, relative to start of value
   --
   Font_Size_Offset: Unsigned_32 := 0;
   Font_Family_Byte_Count_Offset: Unsigned_32 := 4;
   Font_Name_Byte_Count_Offset: Unsigned_32 := 5;
   Font_Family_Byte_Code_Start_Offset: Unsigned_32 := 6;


   -- Creates the layout for the requested type in the container at the requested offset.
   --
   procedure Create_Layout
    (
     CPtr: Container.Instance_Ptr;
     At_Offset: Unsigned_32;
     Of_Type: Types.Item_Type;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );

   -- Returns the offset of the item value. This is either the small-value or the payload.
   -- Note: The offset returned is the offset from the beginning of the array!
   --
   function Value_Offset (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Value_Offset);


   -- Create a new array layout at the given offset
   --
   function Create_Array_Layout
     (
      CPtr: Container.Instance_Ptr;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      For_Element_Type: Types.Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal.Instance;


   -- Fixed layout

   function Get_Type (Item_Ptr: Types.Unsigned_8_Ptr) return Types.Item_Type;
   pragma Inline (Get_Type);

   function Get_Options (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Types.Item_Options;
   pragma Inline (Get_Options);

   function Get_Flags (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Types.Item_Flags;
   pragma Inline (Get_Flags);

   function Get_Name_Field_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Name_Field_Byte_Count);

   function Get_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Byte_Count);

   function Get_Small_Value (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32;
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

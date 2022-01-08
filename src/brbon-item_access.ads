with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

with Color_Package; use Color_Package;
with CRC_Package; use CRC_Package;

with BRBON.Name_Field_Assistent_Package; use BRBON.Name_Field_Assistent_Package;


private package BRBON.Item_Access is


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

--   Item_Overhead_Byte_Count: Array (Item_Type) of Unsigned_32 :=
--     (
--      0,  -- Illegal
--      0,  -- Null
--      0,  -- Bool
--      0,  -- Int_8
--      0,  -- Int_16
--      0,  -- Int_32
--      8,  -- Int_64
--      0,  -- UInt_8
--      0,  -- UInt_16
--      0,  -- UInt_32
--      8,  -- UInt_64
--      0,  -- Float_32
--      8,  -- Float_64
--      4,  -- String
--      8,  -- CRC_String
--      4,  -- Binary
--      8,  -- CRC_Binary
--      32, -- Array
--      8,  -- Dictionary
--      8,  -- Sequence
--      16, -- Table (no column descriptors included)
--      16, -- UUID
--      0,  -- RGBA
--      6   -- Font
--     );


   -----------------------------------------------------------------------------
   -- The fixed part of an item

   Item_Header_Byte_Count: constant Unsigned_32 := 16;

--   function Get_Item_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Header_Ptr is (To_Item_Header_Ptr (S.Data (Item_Offset)'Access));
--   pragma Inline (Get_Item_Header_Ptr);

   function Get_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type;
   procedure Set_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type);

   function Get_Options (S: Store; Item_Offset: Unsigned_32) return Item_Options;
   procedure Set_Options (S: Store; Item_Offset: Unsigned_32; Value: Item_Options);

   function Get_Flags (S: Store; Item_Offset: Unsigned_32) return Item_Flags;
   procedure Set_Flags (S: Store; Item_Offset: Unsigned_32; Value: Item_Flags);

   function Get_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;
   procedure Set_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);

   function Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   procedure Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Get_Parent_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   procedure Set_Parent_Offset (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);


   -----------------------------------------------------------------------------
   -- For small value access

   Small_Value_Offset: constant Unsigned_32 := 12;

   function Get_Bool (S: Store; Item_Offset: Unsigned_32) return Boolean;
   pragma Inline (Get_Bool);

   procedure Set_Bool (S: Store; Item_Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Set_Bool);

   function Get_Int8 (S: Store; Item_Offset: Unsigned_32) return Integer_8;
   pragma Inline (Get_Int8);

   procedure Set_Int8 (S: Store; Item_Offset: Unsigned_32; Value: Integer_8);
   pragma Inline (Set_Int8);

   function Get_UInt8 (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_UInt8);

   procedure Set_UInt8 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_UInt8);

   function Get_Int16 (S: Store; Item_Offset: Unsigned_32) return Integer_16;
   pragma Inline (Get_Int16);

   procedure Set_Int16 (S: Store; Item_Offset: Unsigned_32; Value: Integer_16);
   pragma Inline (Set_Int16);

   function Get_UInt16 (S: Store; Item_Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_UInt16);

   procedure Set_UInt16 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_UInt16);

   function Get_Int32 (S: Store; Item_Offset: Unsigned_32) return Integer_32;
   pragma Inline (Get_Int32);

   procedure Set_Int32 (S: Store; Item_Offset: Unsigned_32; Value: Integer_32);
   pragma Inline (Set_Int32);

   function Get_UInt32 (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_UInt32);

   procedure Set_UInt32 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_UInt32);

   function Get_Float32 (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32;
   pragma Inline (Get_Float32);

   procedure Set_Float32 (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32);
   pragma Inline (Set_Float32);

   function RGBA_Get_Color (S: Store; Item_Offset: Unsigned_32) return Color;
   pragma Inline (RGBA_Get_Color);

   procedure RGBA_Set_Color (S: Store; Item_Offset: Unsigned_32; Value: Color);
   pragma Inline (RGBA_Set_Color);


   -----------------------------------------------------------------------------
   -- Item Name Field (optional)

   function Get_Name_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_16;

   procedure Set_Name_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_16);

   function Get_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Byte_Count);

   procedure Set_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);

   function Get_Name_Byte_Code (S: Store; Item_Offset: Unsigned_32) return String;

   procedure Set_Name_Byte_Code (S: Store; Item_Offset: Unsigned_32; Value: Item_Name);

   function Name_Equals (S: Store; Item_Offset: Unsigned_32; Name_Assistent: Name_Field_Assistent) return Boolean;


   -----------------------------------------------------------------------------
   -- Item Value: String

   function String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function String_Get_String (S: Store; Item_Offset: Unsigned_32) return String;

   procedure String_Set_String (S: Store; Item_Offset: Unsigned_32; Value: String);


   -----------------------------------------------------------------------------
   -- Item Value: CRC String

   function CRC_String_Get_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_32;

   procedure CRC_String_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_32);

   function CRC_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure CRC_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function CRC_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String;

   procedure CRC_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String);


   -----------------------------------------------------------------------------
   -- Item Value: Binary

   function Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array;

   procedure Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array);


   -----------------------------------------------------------------------------
   -- Item Value: CRC Binary

   function CRC_Binary_Get_CRC (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure CRC_Binary_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function CRC_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure CRC_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function CRC_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array;

   procedure CRC_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array);


   -----------------------------------------------------------------------------
   -- Item Value: Array

   function Array_Get_Element_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type;

   procedure Array_Set_Element_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type);

   function Array_Get_Element_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Array_Set_Element_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Array_Get_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Array_Set_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Array_Get_First_Element_Offset (S:Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Sequence

   function Sequence_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Sequence_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Sequence_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Dictionary

   function Dictionary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Dictionary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Dictionary_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Item Value: Table

   function Table_Get_Row_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Table_Set_Row_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Table_Get_Column_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Table_Set_Column_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Table_Get_Fields_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   function Table_Get_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   procedure Table_Set_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32);

   function Table_Get_Column_Descriptor_Start_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


   -----------------------------------------------------------------------------
   -- Table - Column Descriptor

   function Column_Descriptor_Get_Name_CRC (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return CRC_16;

   procedure Column_Descriptor_Set_Name_CRC (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: CRC_16);

   function Column_Descriptor_Get_Name_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_8;

   procedure Column_Descriptor_Set_Name_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_8);

   function Column_Descriptor_Get_Field_Type (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Item_Type;

   procedure Column_Descriptor_Set_Field_Type (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Item_Type);

   function Column_Descriptor_Get_Name_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Name_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_32);

   function Column_Descriptor_Get_Field_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Field_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_32);

   function Column_Descriptor_Get_Field_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_32;

   procedure Column_Descriptor_Set_Field_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_32);


   -----------------------------------------------------------------------------
   -- Item Value: Font

   function Font_Get_Size (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32;

   procedure Font_Set_Size (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32);

   function Font_Get_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;

   procedure Font_Set_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);

   function Font_Get_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8;

   procedure Font_Set_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8);

   -- Returns the offset of the first family name character relative to the start of the item
   --
   function Font_Get_Family_Name_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;

   -- Returns the offset of the first font name character relative to the start of the item
   --
   function Font_Get_Font_Name_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32;


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

   procedure Create_Array_Layout
     (
      S: Store;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent;
      For_Element_Type: Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     );


end BRBON.Item_Access;

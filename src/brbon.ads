with Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Byte_Swapping;
with Interfaces; use Interfaces;
with System;


package BRBON is

   -- The endianness to be used to store data.
   --
   type Endianness is (Big, Little);


   -- Most of the data manipulations are done byte wise
   --
   type Array_Of_Unsigned_8 is array (Unsigned_32 range <>) of aliased Unsigned_8;
   --
   type Array_Of_Unsigned_8_Ptr is access Array_Of_Unsigned_8;
   --
   procedure Deallocate_Array_Of_Unsigned_8 is new Ada.Unchecked_Deallocation (Array_Of_Unsigned_8, Array_Of_Unsigned_8_Ptr);

   Short_Array_Of_Unsigned_8: constant Array_Of_Unsigned_8 (1..1) := (others => 0);


   -- All types available for storage into an Item_Manager.
   --
   type BR_Item_Type is (
                      BR_Null,         -- A null has no associated value, it simply exists.
                      BR_Bool,         -- Corresponding to Standard.Boolean.
                      BR_Int8,         -- An integer with a size of 8 bits (Byte, char).
                      BR_Int16,        -- An integer with a size of 16 bits.
                      BR_Int32,        -- An integer with a size of 32 bits.
                      BR_Int64,        -- An integer with a size of 64 bits.
                      BR_UInt8,        -- An integer with a size of 8 bits and range 0 .. 2**8-1.
                      BR_UInt16,       -- An integer with a size of 16 bits and range 0 .. 2**16-1.
                      BR_UInt32,       -- An integer with a size of 32 bits and range 0 .. 2**32-1.
                      BR_UInt64,       -- An integer with a size of 64 bits and range of 0 .. 2**64-1.
                      BR_Float32,      -- An IEEE 754 32 bit float. Accurate to about 6 decimals, range approx 1.1e-38 to 3.4e38.
                      BR_Float64,      -- An IEEE 754 64 bit float. Accurate to about 15 digits, range aprox 2.2e-308 to 1.7e+308.
                      BR_String,       -- Corresponds to Standard.String.
                      BR_CRC_String,   -- A string with an associated CRC-16 code for fast searches.
                      BR_Binary,       -- A series of bytes, corresponds to array (1..Count) of Br_UInt8.
                      BR_CRC_Binary,   -- A binary with associated CRC-16 code fro fast searches.
                      BR_Array,        -- An array of Item_Types.
                      BR_Dictionary,   -- A dictionary associates a key (string) with a value (Brbon.Item_Type).
                      BR_Sequence,     -- A sequence of Brbon.Item_Type's.
                      BR_Table,        -- A 2 dimension array of Brbon.Item_Type's addressed by column (string or index) and row (index).
                      BR_UUID,         -- A UUID, an array of 16 Br_UInt8 values returned as array or string.
                      BR_Color,        -- A RGBA (Red Green Blue Alpha) for color specifications.
                      BR_Font          -- A font specification (family name and font name).
                     );
   for BR_Item_Type'Size use 8;
   for BR_Item_Type use (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

   -- Minimum items sizes for items without a name, including the value length if the value length is fixed and assuming empty when the value size is variable.
   -- Always a multiple of 8.
   --
   Minimum_Item_Byte_Count: Array (BR_Item_Type'Range) of Unsigned_32 :=
     (
      BR_Null => 32,
      BR_Bool => 32,
      BR_Int8 => 32,
      BR_Int16 => 32,
      BR_Int32 => 32,
      BR_Int64 => 32 + 8,
      BR_UInt8 => 32,
      BR_UInt16 => 32,
      BR_UInt32 => 32,
      BR_UInt64 => 32 + 8,
      BR_Float32 => 32,
      BR_Float64 => 32 + 8,
      BR_String => 32 + 8,
      BR_CRC_String => 32 + 8,
      BR_Binary => 32 + 4,
      BR_CRC_Binary => 32 + 8,
      BR_Array => 32 + 16,
      BR_Sequence => 32 + 8,
      BR_Dictionary => 32 + 8,
      BR_Table => 32 + 16,
      BR_UUID => 32 + 16,
      BR_Color => 32,
      BR_Font => 32 + 8);

   -- A packed array of 8 bits, used for Options and Flags.
   --
   type Bits_8 is array (Integer range 0..7) of Boolean with Pack;
   --
   function To_Bits_8 is new Ada.Unchecked_Conversion (Unsigned_8, Bits_8);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Bits_8, Unsigned_8);



   -- Access to bytes in the storage
   --
   type Unsigned_8_Ptr is access all Unsigned_8;


   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);
   function Swap_Unsigned_64 is new GNAT.Byte_Swapping.Swapped8 (Unsigned_64);
   function Swap_Integer_16 is new GNAT.Byte_Swapping.Swapped2 (Integer_16);
   function Swap_Integer_32 is new GNAT.Byte_Swapping.Swapped4 (Integer_32);
   function Swap_Integer_64 is new GNAT.Byte_Swapping.Swapped8 (Integer_64);
   function Swap_Float_32 is new GNAT.Byte_Swapping.Swapped4 (IEEE_Float_32);
   function Swap_Float_64 is new GNAT.Byte_Swapping.Swapped8 (IEEE_Float_64);


   subtype Two_Bytes is Array_Of_Unsigned_8 (1 .. 2);
   subtype Four_Bytes is Array_Of_Unsigned_8 (1 .. 4);
   subtype Eight_Bytes is Array_Of_Unsigned_8 (1 .. 8);

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Two_Bytes, Unsigned_16);
   function To_Integer_16 is new Ada.Unchecked_Conversion (Two_Bytes, Integer_16);
   function To_Unsigned_32 is new Ada.Unchecked_Conversion (Four_Bytes, Unsigned_32);
   function To_Integer_32 is new Ada.Unchecked_Conversion (Four_Bytes, Integer_32);
   function To_Float_32 is new Ada.Unchecked_Conversion (Four_Bytes, IEEE_Float_32);
   function To_Unsigned_64 is new Ada.Unchecked_Conversion (Eight_Bytes, Unsigned_64);
   function To_Integer_64 is new Ada.Unchecked_Conversion (Eight_Bytes, Integer_64);
   function To_Float_64 is new Ada.Unchecked_Conversion (Eight_Bytes, IEEE_Float_64);

   function To_Two_Bytes is new Ada.Unchecked_Conversion (Unsigned_16, Two_Bytes);
   function To_Two_Bytes is new Ada.Unchecked_Conversion (Integer_16, Two_Bytes);
   function To_Four_Bytes is new Ada.Unchecked_Conversion (Unsigned_32, Four_Bytes);
   function To_Four_Bytes is new Ada.Unchecked_Conversion (Integer_32, Four_Bytes);
   function To_Four_Bytes is new Ada.Unchecked_Conversion (IEEE_Float_32, Four_Bytes);
   function To_Eight_Bytes is new Ada.Unchecked_Conversion (Unsigned_64, Eight_Bytes);
   function To_Eight_Bytes is new Ada.Unchecked_Conversion (Integer_64, Eight_Bytes);
   function To_Eight_Bytes is new Ada.Unchecked_Conversion (IEEE_Float_64, Eight_Bytes);

   function To_Integer_8 is new Ada.Unchecked_Conversion (Unsigned_8, Integer_8);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Integer_8, Unsigned_8);

   type String_Ptr is access all String;

   function Max (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A > B then A else B);
   function Min (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A < B then A else B);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_32) return Unsigned_32 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#FFFFFFF8#);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_8) return Unsigned_8 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#F8#);


   -- Option associated with stored items (currently unused)
   --
   type Item_Options is new Bits_8;
   function To_Item_Options is new Ada.Unchecked_Conversion (Unsigned_8, Item_Options);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Options, Unsigned_8);
   No_Options : Item_Options := (False, False, False, False, False, False, False, False);


   -- Item flags for transitionary events to be recorded in an item (currently unused)
   --
   type Item_Flags is new Bits_8;
   function To_Item_Flags is new Ada.Unchecked_Conversion (Unsigned_8, Item_Flags);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Flags, Unsigned_8);
   No_Flags : Item_Flags := (False, False, False, False, False, False, False, False);

   -- Item Name
   --
   subtype Item_Name_Index is Integer range 1..245;
   package Item_Name_Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length (Item_Name_Index'Last);
   subtype Item_Name is Item_Name_Bounded_String.Bounded_String;
   --
   No_Name: constant Item_Name := Item_Name_Bounded_String.To_Bounded_String("");


   -- Layout of an item header
   --
   type Item_Header is
      record
         Item_Type: BR_Item_Type;
         Options: Item_Options;
         Flags: Item_Flags;
         Name_Field_Byte_Count: Unsigned_8;
         Byte_Count: Unsigned_32;
         Parent_Offset: Unsigned_32;
         Small_Value: Unsigned_32;
      end record;
   for Item_Header use
      record
         Item_Type             at 0  range 0..7;
         Options               at 1  range 0..7;
         Flags                 at 2  range 0..7;
         Name_Field_Byte_Count at 3  range 0..7;
         Byte_Count            at 4  range 0..31;
         Parent_Offset         at 8  range 0..31;
         Small_Value           at 12 range 0..31;
      end record;
   --
   type Item_Header_Ptr is access Item_Header;
   --
   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_Ptr);


   -- Layout of a name
   --
   type Item_Name_Layout is
      record
         CRC_16: Unsigned_16;
         Byte_Count: Unsigned_8;
         Ascii_Code: aliased Unsigned_8;
      end record;
   for Item_Name_Layout use
      record
         CRC_16 at 0 range 0..15;
         Byte_Count at 2 range 0..7;
         Ascii_Code at 3 range 0..7;
      end record;
   --
   type Item_Name_Layout_Ptr is access Item_Name_Layout;
   --
   function To_Item_Name_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Name_Layout_Ptr);


   -- Layout of the String value
   --
   type BR_String_Layout is
      record
         Byte_Count: Unsigned_32;
         UTF8_Byte_Code: aliased Unsigned_8;
      end record;
   for BR_String_Layout use
      record
         Byte_Count at 0 range 0..31;
         UTF8_Byte_Code at 4 range 0..7;
      end record;
   --
   type BR_String_Layout_Ptr is access BR_String_Layout;
   --
   function To_BR_String_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_String_Layout_Ptr);


   -- Layout of the CRC String value
   --
   type BR_CRC_String_Layout is
      record
         CRC_32: Unsigned_32;
         Byte_Count: Unsigned_32;
         UTF8_Byte_Code: aliased Unsigned_8;
      end record;
   for BR_CRC_String_Layout use
      record
         CRC_32 at 0 range 0..31;
         Byte_Count at 4 range 0..31;
         UTF8_Byte_Code at 8 range 0..7;
      end record;
   --
   type BR_CRC_String_Layout_Ptr is access BR_CRC_String_Layout;
   --
   function To_BR_CRC_String_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_CRC_String_Layout_Ptr);


   -- Layout of the Binary value
   --
   type BR_Binary_Layout is
      record
         Byte_Count: Unsigned_32;
         Binary: aliased Unsigned_8;
      end record;
   for BR_Binary_Layout use
      record
         Byte_Count at 0 range 0..31;
         Binary at 4 range 0..7;
      end record;
   --
   type BR_Binary_Layout_Ptr is access BR_Binary_Layout;
   --
   function To_BR_Binary_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Binary_Layout_Ptr);


   -- Layout of the CRC Binary value
   --
   type BR_CRC_Binary_Layout is
      record
         CRC_32: Unsigned_32;
         Byte_Count: Unsigned_32;
         Binary: aliased Unsigned_8;
      end record;
   for BR_CRC_Binary_Layout use
      record
         CRC_32 at 0 range 0..31;
         Byte_Count at 4 range 0..31;
         Binary at 8 range 0..7;
      end record;
   --
   type BR_CRC_Binary_Layout_Ptr is access BR_CRC_Binary_Layout;
   --
   function To_BR_CRC_Binary_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_CRC_Binary_Layout_Ptr);


   -- Layout of the Array value
   --
   type BR_Array_Layout is
      record
         Reserved_32: Unsigned_32;
         Element_Type: BR_Item_Type;
         Reserved_8: Unsigned_8;
         Reserved_16: Unsigned_16;
         Element_Count: Unsigned_32;
         Element_Byte_Count: Unsigned_32;
         Element_Base: aliased Unsigned_8;
      end record;
   for BR_Array_Layout use
      record
         Reserved_32 at 0 range 0..31;
         Element_Type at 4 range 0..7;
         Reserved_8 at 5 range 0..7;
         Reserved_16 at 6 range 0..15;
         Element_Count at 8 range 0..31;
         Element_Byte_Count at 12 range 0..31;
         Element_Base at 16 range 0..7;
      end record;
   --
   type BR_Array_Layout_Ptr is access BR_Array_Layout;
   --
   function To_BR_Array_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Array_Layout_Ptr);


   -- Layout of the Dictionary
   --
   type BR_Dictionary_Layout is
      record
         Reserved_32: Unsigned_32;
         Item_Count: Unsigned_32;
         Items_Base: aliased Unsigned_8;
      end record;
   for BR_Dictionary_Layout use
      record
         Reserved_32 at 0 range 0..31;
         Item_Count at 4 range 0..31;
         Items_Base at 8 range 0..7;
      end record;
   --
   type BR_Dictionary_Layout_Ptr is access BR_Dictionary_Layout;
   --
   function To_BR_Dictionary_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Dictionary_Layout_Ptr);


   -- Layout of the Sequence
   --
   type BR_Sequence_Layout is
      record
         Reserved_32: Unsigned_32;
         Item_Count: Unsigned_32;
         Items_Base: aliased Unsigned_8;
      end record;
   for BR_Sequence_Layout use
      record
         Reserved_32 at 0 range 0..31;
         Item_Count at 4 range 0..31;
         Items_Base at 8 range 0..7;
      end record;
   --
   type BR_Sequence_Layout_Ptr is access BR_Sequence_Layout;
   --
   function To_BR_Sequence_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Sequence_Layout_Ptr);


   -- Table access
   --
   type BR_Table_Layout is
      record
         Row_Count: Unsigned_32;
         Column_Count: Unsigned_32;
         Rows_Offset: Unsigned_32;
         Row_Byte_Count: Unsigned_32;
      end record;
   for BR_Table_Layout use
      record
         Row_Count at 0 range 0..31;
         Column_Count at 4 range 0..31;
         Rows_Offset at 8 range 0..31;
         Row_Byte_Count at 12 range 0..31;
      end record;
   --
   type BR_Table_Layout_Ptr is access BR_Table_Layout;
   --
   function To_BR_Table_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Table_Layout_Ptr);


   type BR_Table_Column_Descriptor_Layout is
      record
         Name_CRC_16: Unsigned_16;
         Name_Field_Byte_Count: Unsigned_8;
         Field_Type: BR_Item_Type;
         Name_Ascii_Offset: Unsigned_32;
         Field_Offset: Unsigned_32;
         Field_Byte_Count: Unsigned_32;
      end record;
   for BR_Table_Column_Descriptor_Layout use
      record
         Name_CRC_16 at 0 range 0..15;
         Name_Field_Byte_Count at 2 range 0..7;
         Field_Type at 3 range 0..7;
         Name_Ascii_Offset at 4 range 0..31;
         Field_Offset at 8 range 0..31;
         Field_Byte_Count at 12 range 0..31;
      end record;
   --
   type BR_Table_Column_Descriptor_Layout_Ptr is access BR_Table_Column_Descriptor_Layout;
   --
   function To_BR_Table_Column_Descriptor_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Table_Column_Descriptor_Layout_Ptr);


   type BR_Table_Column_Name_Layout is
      record
         Byte_Count: Unsigned_8;
         Unused_8: Unsigned_8;
         Unused_16: Unsigned_16;
         ASCII_Code: aliased Unsigned_8;
      end record;
   for BR_Table_Column_Name_Layout use
      record
         Byte_Count at 0 range 0..7;
         Unused_8 at 1 range 0..7;
         Unused_16 at 2 range 0..15;
         ASCII_Code at 4 range 0..7;
      end record;
   --
   type BR_Table_Column_Name_Layout_Ptr is access BR_Table_Column_Name_Layout;
   --
   function To_BR_Table_Column_Name_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, BR_Table_Column_Name_Layout_Ptr);


   -- Possible exceptions
   -- ===================

   -- Raised when an operation is attempted on the wrong type
   --
   Type_Conflict: exception;


   -- Raised when the storage area runs out of space.
   --
   Storage_Error: exception;

   -- Raised when a BRBON structure contains an illegal type pattern.
   --
   Illegal_Item_Type: exception;

   -- Raised when a string to Item_Name conversion failed.
   --
   Item_Name_Too_Long: exception;

   -- Raised when an attempt is made to execute an incompletely coded routine
   --
   Incomplete_Code: exception;

   -- This exception is raised when a bit pattern in the raw data could not be mapped to a corresponding enum.
   --
   Enum_Mapping_Failed: exception;

end BRBON;

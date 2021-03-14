with Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System;


package BRBON.Types is

   -- The endianness to be used to store data.
   --
   type Endianness is (Big, Little);


   -- Most of the data manipulations are done byte wise
   --
   type Array_Of_Unsigned_8 is array (Unsigned_32 range <>) of Unsigned_8;
   --
   type Array_Of_Unsigned_8_Ptr is access Array_Of_Unsigned_8;
   --
   procedure Deallocate_Array_Of_Unsigned_8 is new Ada.Unchecked_Deallocation (Array_Of_Unsigned_8, Array_Of_Unsigned_8_Ptr);

   Short_Array_Of_Unsigned_8: constant Array_Of_Unsigned_8 (1..1) := (others => 0);


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

   -- All types available for storage into an Item_Manager.
   --
   type BR_Item_Type is
     (
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

   function To_BR_Item_Type is new Ada.Unchecked_Conversion (Unsigned_8, BR_Item_Type);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (BR_Item_Type, Unsigned_8);

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


   -- The type of blocks available
   --
   type BR_Block_Type is
     (
      Illegal,
      Single_Item_File
     );
   for BR_Block_Type'Size use 8;
   for BR_Block_Type use
     (
      Illegal => 0,
      Single_Item_File => 1
     );

   function To_Unsigned_8 is new Ada.Unchecked_Conversion (BR_Block_Type, Unsigned_8);
   function To_BR_Block_Type is new Ada.Unchecked_Conversion (Unsigned_8, BR_Block_Type);

   Minimum_Block_Byte_Count: array (BR_Block_Type'Range) of Unsigned_32 :=
     (
      Illegal => 0,
      Single_Item_File => 8 + 8 + 8 + 8
     );


   -- The block options
   --
   type BR_Block_Options is
      record
         Endianness: Boolean;
         Bit_6: Boolean;
         Bit_5: Boolean;
         Bit_4: Boolean;
         Bit_3: Boolean;
         Bit_2: Boolean;
         Bit_1: Boolean;
         Bit_0: Boolean;
      end record;
   for BR_Block_Options use
      record
         Endianness at 0 range 7..7;
         Bit_6 at 0 range 6..6;
         Bit_5 at 0 range 5..5;
         Bit_4 at 0 range 4..4;
         Bit_3 at 0 range 3..3;
         Bit_2 at 0 range 2..2;
         Bit_1 at 0 range 1..1;
         Bit_0 at 0 range 0..0;
      end record;
   for BR_Block_Options'Size use 8;

   function To_Unsigned_8 is new Ada.Unchecked_Conversion (BR_Block_Options, Unsigned_8);
   function To_BR_Block_Options is new Ada.Unchecked_Conversion (Unsigned_8, BR_Block_Options);


   -- Access to bytes in the byte store
   --
   type Unsigned_8_Ptr is access all Unsigned_8;


   type String_Ptr is access all String;

   function Max (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A > B then A else B);
   function Min (A: Unsigned_32; B: Unsigned_32) return Unsigned_32 is (if A < B then A else B);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_32) return Unsigned_32 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#FFFFFFF8#);
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_8) return Unsigned_8 is (if (A and 16#07#) = 0 then A else (A + 8) and 16#F8#);


   -- Option associated with stored items (currently unused)
   --
   type BR_Item_Options is new Bits_8;
   function To_BR_Item_Options is new Ada.Unchecked_Conversion (Unsigned_8, BR_Item_Options);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (BR_Item_Options, Unsigned_8);
   No_Options : BR_Item_Options := (False, False, False, False, False, False, False, False);


   -- Item flags for transitionary events to be recorded in an item (currently unused)
   --
   type BR_Item_Flags is new Bits_8;
   function To_BR_Item_Flags is new Ada.Unchecked_Conversion (Unsigned_8, BR_Item_Flags);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (BR_Item_Flags, Unsigned_8);
   No_Flags : BR_Item_Flags := (False, False, False, False, False, False, False, False);


   -- Item Name
   --
   subtype BR_Item_Name_Index is Integer range 1..245;
   package BR_Item_Name_Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length (BR_Item_Name_Index'Last);
   subtype BR_Item_Name is BR_Item_Name_Bounded_String.Bounded_String;
   --
   No_Name: constant BR_Item_Name := BR_Item_Name_Bounded_String.To_Bounded_String("");


   -- Table column descriptor
   --
   type Table_Column_Specification is
      record
         Column_Name: BR_Item_Name;
         Column_Type: BR_Item_Type;
         Field_Byte_Count: Unsigned_32;
         Name_Offset: Unsigned_32 := 0;
         Field_Offset: Unsigned_32 := 0;
      end record;
   --
   type Array_Of_Table_Column_Specification is array (Integer range <>) of Table_Column_Specification;


end BRBON.Types;

with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Storage_Area; use Storage_Area;


package Item is


   -- All types available for storage into an Item_Manager.
   --
   type BR_Item_Type is (
                      BR_Illegal,      -- Used for error detection, cannot be used by the user.
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
                      BR_RBGA,         -- A RGBA (Red Green Blue Alpha) for color specifications.
                      BR_Font          -- A font specification (family name and font name).
                     );
   for BR_Item_Type'Size use 8;
   for BR_Item_Type use (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);


   -- A packed array of 8 bits, used for Options and Flags.
   --
   type Bits_8 is array (Integer range 0..7) of Boolean with Pack;
   --
   function To_Bits_8 is new Ada.Unchecked_Conversion (Unsigned_8, Bits_8);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Bits_8, Unsigned_8);


   -- Option associated with stored items (currently unused)
   --
   type Item_Options is new Bits_8;
   function To_Item_Options is new Ada.Unchecked_Conversion (Unsigned_8, Item_Options);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Options, Unsigned_8);


   -- Item flags for transitionary events to be recorded in an item (currently unused)
   --
   type Item_Flags is new Bits_8;
   function To_Item_Flags is new Ada.Unchecked_Conversion (Unsigned_8, Item_Flags);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Flags, Unsigned_8);


   -- Item Name
   --
   subtype Item_Name_Index is Integer range 1..245;
   package Item_Name_Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length (Item_Name_Index'Last);
   subtype Item_Name is Item_Name_Bounded_String.Bounded_String;
   --
   No_Item_Name: constant Item_Name := Item_Name_Bounded_String.To_Bounded_String("");


   -- Layout of an item header
   --
   type Item_Header (Item_Type: BR_Item_Type) is
      record
         Options: Item_Options;
         Flags: Item_Flags;
         Name_Field_Byte_Count: Unsigned_8;
         Byte_Count: Unsigned_32;
         Parent_Offset: Unsigned_32;

         case Item_Type is

            when BR_Bool =>
               Value_Bool: Unsigned_8;

            when BR_Int8 =>
               Value_Int8: Integer_8;

            when BR_Int16 =>
               Value_Int16: Integer_16;

            when BR_Int32 =>
               Value_Int32: Integer_32;

            when BR_UInt8 =>
               Value_UInt8: Unsigned_8;

            when BR_UInt16 =>
               Value_UInt16: Unsigned_16;

            when BR_UInt32 =>
               Value_UInt32: Unsigned_32;

            when BR_Float32 =>
               Value_Float32: IEEE_Float_32;

            when BR_Illegal | BR_Null | BR_Int64 | BR_UInt64 | BR_Float64 | BR_String | BR_Crc_String | BR_Binary | BR_Crc_Binary | BR_Array | BR_Dictionary | BR_Sequence | BR_Table | BR_Uuid | BR_Rbga | BR_Font =>
               Unused: Unsigned_32;
         end case;
      end record;
   --
   for Item_Header use
      record
         Item_Type             at 0  range 0..7;
         Options               at 1  range 0..7;
         Flags                 at 2  range 0..7;
         Name_Field_Byte_Count at 3  range 0..7;
         Byte_Count            at 4  range 0..31;
         Parent_Offset         at 8  range 0..31;
         --
         Value_Bool            at 12 range 0..7;
         Value_Int8            at 12 range 0..8;
         Value_Int16           at 12 range 0..15;
         Value_Int32           at 12 range 0..31;
         Value_UInt8           at 12 range 0..7;
         Value_UInt16          at 12 range 0..15;
         Value_UInt32          at 12 range 0..31;
         Value_Float32         at 12 range 0..31;
         Unused                at 12 range 0..31;
      end record;


   -- Sets the type of an item.
   --
   procedure Item_Type (I: Item_Header; Value: BR_Item_Type);


   -- Returns the type of the item.
   -- Can raise Enum_Mapping_Failed.
   --
   function Item_Type (I: Item_Header) return BR_Item_Type;


   -- A pointer to an item layout
   --
   type Item_Header_Ptr is access Item_Header;
   --
   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_Ptr);


   -- The name array
   --
   type Ascii_Array is array (Item_Name_Index range <>) of Unsigned_8;


   -- Layout of the name field
   --
   type Name_Field is
      record
         CRC: Unsigned_16;
         Byte_Count: Unsigned_8;
      end record;
   --
   for Name_Field use
      record
         CRC        at 0 range 0..15;
         Byte_Count at 2 range 0..7;
      end record;


   --




end Item;

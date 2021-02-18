
with Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;

with BRBON_Basic_Types; use BRBON_Basic_Types;

package BRBON is


   -- All types available for storage into an Item_Manager.
   --
   type Item_Type is (
                      Br_Illegal,      -- Used for error detection, cannot be used by the user.
                      Br_Null,         -- A null has no associated value, it simply exists.
                      Br_Bool,         -- Corresponding to Standard.Boolean.
                      Br_Int8,         -- An integer with a size of 8 bits (Byte, char).
                      Br_Int16,        -- An integer with a size of 16 bits.
                      Br_Int32,        -- An integer with a size of 32 bits.
                      Br_Int64,        -- An integer with a size of 64 bits.
                      Br_UInt8,        -- An integer with a size of 8 bits and range 0 .. 2**8-1.
                      Br_UInt16,       -- An integer with a size of 16 bits and range 0 .. 2**16-1.
                      Br_UInt32,       -- An integer with a size of 32 bits and range 0 .. 2**32-1.
                      Br_UInt64,       -- An integer with a size of 64 bits and range of 0 .. 2**64-1.
                      Br_Float32,      -- An IEEE 754 32 bit float. Accurate to about 6 decimals, range approx 1.1e-38 to 3.4e38.
                      Br_Float64,      -- An IEEE 754 64 bit float. Accurate to about 15 digits, range aprox 2.2e-308 to 1.7e+308.
                      Br_String,       -- Corresponds to Standard.String.
                      Br_Crc_String,   -- A string with an associated CRC-16 code for fast searches.
                      Br_Binary,       -- A series of bytes, corresponds to array (1..Count) of Br_UInt8.
                      Br_Crc_Binary,   -- A binary with associated CRC-16 code fro fast searches.
                      Br_Array,        -- An array of Brbon.Item_Types.
                      Br_Dictionary,   -- A dictionary associates a key (string) with a value (Brbon.Item_Type).
                      Br_Sequence,     -- A sequence of Brbon.Item_Type's.
                      Br_Table,        -- A 2 dimension array of Brbon.Item_Type's addressed by column (string or index) and row (index).
                      Br_Uuid,         -- A UUID, an array of 16 Br_UInt8 values returned as array or string.
                      Br_Rbga,         -- A RGBA (Red Green Blue Alpha) for color specifications.
                      Br_Font          -- A font specification (family name and font name).
                     );
   for Item_Type'Size use 8;
   for Item_Type use (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);


   -- Option associated with stored items (currently unused)
   --
   type Item_Options is new Bits_8;
   function To_Item_Options is new Ada.Unchecked_Conversion(Unsigned_8, Item_Options);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Options, Unsigned_8);


   -- Item Name
   --
   subtype Item_Name_Index is Integer range 1..245;
   package Item_Name_Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length (Item_Name_Index'Last);
   subtype Item_Name is Item_Name_Bounded_String.Bounded_String;
   --
   No_Item_Name: constant Item_Name := Item_Name_Bounded_String.To_Bounded_String("");


   -- Possible exceptions
   -- ===================

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


end BRBON;

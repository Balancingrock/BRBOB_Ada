with Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;


package BRBON is

   -- The endianness to be used to store data.
   --
   type Endianness is (Big, Little);




   --type Integer_8_Ptr is access all Integer_8;
   --type Integer_16_Ptr is access all Integer_16;
   --type Integer_32_Ptr is access all Integer_32;
   --type Integer_64_Ptr is access all Integer_64;


   --type Unsigned_16_Ptr is access all Unsigned_16;
   --type Unsigned_32_Ptr is access all Unsigned_32;
   --type Unsigned_64_Ptr is access all Unsigned_64;

   --type IEEE_Float_32_Ptr is access all IEEE_Float_32;
   --type IEEE_Float_64_Ptr is access all IEEE_Float_64;

   --function To_Integer_8_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_ptr, Integer_8_Ptr);
   --function To_Integer_16_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Integer_16_Ptr);
   --function To_Integer_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Integer_32_Ptr);
   --function To_Integer_64_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Integer_64_Ptr);
   --
   --function To_Unsigned_16_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_16_Ptr);
   --function To_Unsigned_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_32_Ptr);
   --function To_Unsigned_64_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_64_Ptr);
   --
   --function To_IEEE_Float_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, IEEE_Float_32_Ptr);
   --function To_IEEE_Float_64_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, IEEE_Float_64_Ptr);





   -- All types available for storage into an Item_Manager.
   --
   --type Item_Type is (
   --                   Br_Illegal,      -- Used for error detection, cannot be used by the user.
   --                   Br_Null,         -- A null has no associated value, it simply exists.
   --                   Br_Bool,         -- Corresponding to Standard.Boolean.
   --                   Br_Int8,         -- An integer with a size of 8 bits (Byte, char).
   --                   Br_Int16,        -- An integer with a size of 16 bits.
   --                   Br_Int32,        -- An integer with a size of 32 bits.
   --                   Br_Int64,        -- An integer with a size of 64 bits.
   --                   Br_UInt8,        -- An integer with a size of 8 bits and range 0 .. 2**8-1.
   --                   Br_UInt16,       -- An integer with a size of 16 bits and range 0 .. 2**16-1.
   --                   Br_UInt32,       -- An integer with a size of 32 bits and range 0 .. 2**32-1.
   --                   Br_UInt64,       -- An integer with a size of 64 bits and range of 0 .. 2**64-1.
   --                   Br_Float32,      -- An IEEE 754 32 bit float. Accurate to about 6 decimals, range approx 1.1e-38 to 3.4e38.
   --                   Br_Float64,      -- An IEEE 754 64 bit float. Accurate to about 15 digits, range aprox 2.2e-308 to 1.7e+308.
   --                   Br_String,       -- Corresponds to Standard.String.
   --                   Br_Crc_String,   -- A string with an associated CRC-16 code for fast searches.
   --                   Br_Binary,       -- A series of bytes, corresponds to array (1..Count) of Br_UInt8.
   --                   Br_Crc_Binary,   -- A binary with associated CRC-16 code fro fast searches.
   --                   Br_Array,        -- An array of Brbon.Item_Types.
   --                   Br_Dictionary,   -- A dictionary associates a key (string) with a value (Brbon.Item_Type).
   --                   Br_Sequence,     -- A sequence of Brbon.Item_Type's.
   --                   Br_Table,        -- A 2 dimension array of Brbon.Item_Type's addressed by column (string or index) and row (index).
   --                   Br_Uuid,         -- A UUID, an array of 16 Br_UInt8 values returned as array or string.
   --                   Br_Rbga,         -- A RGBA (Red Green Blue Alpha) for color specifications.
   --                   Br_Font          -- A font specification (family name and font name).
   --                  );
   --for Item_Type'Size use 8;
   --for Item_Type use (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);





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

   -- This exception is raised when a bit pattern in the raw data could not be mapped to a corresponding enum.
   --
   Enum_Mapping_Failed: exception;

end BRBON;

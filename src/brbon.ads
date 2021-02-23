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
   function Round_Up_To_Nearest_Multiple_of_8 (A: Unsigned_32) return Unsigned_32 is ((A + 8) and 16#FFFFFFF8#);


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

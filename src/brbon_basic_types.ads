with Ada.Unchecked_Conversion;
with System;


package BRBON_Basic_Types is


   -- The size definitions are copied from Interfaces.ads

   type Integer_8  is range -2 **  7 .. 2 **  7 - 1;
   for Integer_8'Size use  8;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;

   type Integer_64 is new Long_Long_Integer;
   for Integer_64'Size use 64;

   type Unsigned_8  is mod 2 ** 8;
   for Unsigned_8'Size use  8;
   pragma Provide_Shift_Operators (Unsigned_8);

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;
   pragma Provide_Shift_Operators (Unsigned_16);

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   pragma Provide_Shift_Operators (Unsigned_32);

   type Unsigned_64 is mod 2 ** Long_Long_Integer'Size;
   for Unsigned_64'Size use 64;

   type Float_32 is digits 6;
   for Float_32'Size use 32;

   type Float_64 is digits 15;
   for Float_64'Size use 64;


   -- The endianness to be used to store data.
   --
   type Endianness is (Big, Little);


   type Array_Of_Unsigned_8 is array (Unsigned_32 range <>) of aliased Unsigned_8;

   type Integer_8_Ptr is access all Integer_8;
   type Integer_16_Ptr is access all Integer_16;
   type Integer_32_Ptr is access all Integer_32;
   type Integer_64_Ptr is access all Integer_64;

   type Unsigned_8_Ptr is access all Unsigned_8;
   type Unsigned_16_Ptr is access all Unsigned_16;
   type Unsigned_32_Ptr is access all Unsigned_32;
   type Unsigned_64_Ptr is access all Unsigned_64;

   type Float_32_Ptr is access all Float_32;
   type Float_64_Ptr is access all Float_64;

   function To_Integer_8_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_ptr, Integer_8_Ptr);
   function To_Integer_16_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Integer_16_Ptr);
   function To_Integer_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Integer_32_Ptr);
   function To_Integer_64_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Integer_64_Ptr);
   --
   function To_Unsigned_16_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_16_Ptr);
   function To_Unsigned_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_32_Ptr);
   function To_Unsigned_64_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_64_Ptr);
   --
   function To_Float_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Float_32_Ptr);
   function To_Float_64_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Float_64_Ptr);


   type Bits_8 is array (Integer range 0..7) of Boolean with Pack;

   function To_Bits_8 is new Ada.Unchecked_Conversion (Unsigned_8, Bits_8);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Bits_8, Unsigned_8);


   -- For memmove support
   --
   function To_Address is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, System.Address);

end BRBON_Basic_Types;

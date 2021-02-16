with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System;


package BRBON_Basic_Types is

   subtype Float_32 is IEEE_Float_32;
   subtype Float_64 is IEEE_Float_64;

   type Array_Of_Unsigned_8 is array (Unsigned_32 range <>) of aliased Unsigned_8;

   type Integer_8_Ptr is access all Integer_8;
   type Integer_16_Ptr is access all Integer_16;
   type Integer_32_Ptr is access all Integer_32;
   type Integer_64_Ptr is access all Integer_64;

   type Unsigned_8_Ptr is access all Unsigned_8;
   type Unsigned_16_Ptr is access all Unsigned_16;
   type Unsigned_32_Ptr is access all Unsigned_32;
   type Unsigned_64_Ptr is access all Unsigned_64;

   type Float_32_Ptr is access all IEEE_Float_32;
   type Float_64_Ptr is access all IEEE_Float_64;

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

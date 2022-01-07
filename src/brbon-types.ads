with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with BRBON; use BRBON;


private package BRBON.Types is

   type Array_Of_Boolean is array (Unsigned_32 range <>) of Boolean;


   Empty_Unsigned_8_Array: constant Unsigned_8_Array (1..0) := (others => 0);
   Short_Unsigned_8_Array: constant Unsigned_8_Array (1..1) := (others => 0);


   subtype Two_Bytes is Unsigned_8_Array (1 .. 2);
   subtype Four_Bytes is Unsigned_8_Array (1 .. 4);
   subtype Eight_Bytes is Unsigned_8_Array (1 .. 8);

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
   function To_Integer_16 is new Ada.Unchecked_Conversion (Unsigned_16, Integer_16);
   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Integer_16, Unsigned_16);
   function To_Integer_32 is new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);
   function To_Unsigned_32 is new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   function To_Integer_64 is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);
   function To_Unsigned_64 is new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);


   -- ==========================================================================
   -- For items

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

   Item_Overhead_Byte_Count: Array (Item_Type) of Unsigned_32 :=
     (
      0,  -- Illegal
      0,  -- Null
      0,  -- Bool
      0,  -- Int_8
      0,  -- Int_16
      0,  -- Int_32
      8,  -- Int_64
      0,  -- UInt_8
      0,  -- UInt_16
      0,  -- UInt_32
      8,  -- UInt_64
      0,  -- Float_32
      8,  -- Float_64
      4,  -- String
      8,  -- CRC_String
      4,  -- Binary
      8,  -- CRC_Binary
      32, -- Array
      8,  -- Dictionary
      8,  -- Sequence
      16, -- Table (no column descriptors included)
      16, -- UUID
      0,  -- RGBA
      6   -- Font
     );


end BRBON.Types;

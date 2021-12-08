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
   type Array_Of_Unsigned_8_Ptr is access all Array_Of_Unsigned_8;

   type Array_Of_Boolean is array (Unsigned_32 range <>) of Boolean;

   procedure Deallocate_Array_Of_Unsigned_8 is new Ada.Unchecked_Deallocation (Array_Of_Unsigned_8, Array_Of_Unsigned_8_Ptr);

   Empty_Array_Of_Unsigned_8: constant Array_Of_Unsigned_8 (1..0) := (others => 0);
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
   function To_Integer_16 is new Ada.Unchecked_Conversion (Unsigned_16, Integer_16);
   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Integer_16, Unsigned_16);
   function To_Integer_32 is new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);
   function To_Unsigned_32 is new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   function To_Integer_64 is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);
   function To_Unsigned_64 is new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);


   -- ==========================================================================
   -- For blocks

   type Block_Type is
     (
      Illegal,
      Single_Item
     );
   for Block_Type'Size use 16;
   for Block_Type use
     (
      Illegal => 0,
      Single_Item => 1
     );

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Block_Type, Unsigned_16);
   function To_Block_Type is new Ada.Unchecked_Conversion (Unsigned_16, Block_Type);


   type Block_Options is (No_Block_Options, Reacquisition_Possible);
   for Block_Options'Size use 16;
   for Block_Options use
      (
         No_Block_Options       => 0,
         Reacquisition_Possible => 16#01#
      );

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Block_Options, Unsigned_16);
   function To_Block_Options is new Ada.Unchecked_Conversion (Unsigned_16, Block_Options);


   -- ==========================================================================
   -- For items

   Minimum_Item_Byte_Count: constant Unsigned_32 := 16;


   type Item_Type is
     (
      Illegal,
      Null_Type,
      Bool_Type,
      Int_8_Type, Int_16_Type, Int_32_Type, Int_64_Type,
      UInt_8_Type, UInt_16_Type, UInt_32_Type, UInt_64_Type,
      Float_32_Type, Float_64_Type,
      String_Type, Crc_String_Type,
      Binary_Type, Crc_Binary_Type,
      Array_Type, Dictionary_Type, Sequence_Type, Table_Type,
      UUID_Type,
      RGBA_Type,
      Font_Type
     );

   for Item_Type'Size use 8;

   for Item_Type use
     (
      Illegal         => 0,
      Null_Type       => 16#01#,
      Bool_Type       => 16#02#,
      Int_8_Type      => 16#03#,
      Int_16_Type     => 16#04#,
      Int_32_Type     => 16#05#,
      Int_64_Type     => 16#06#,
      UInt_8_Type     => 16#07#,
      UInt_16_Type    => 16#08#,
      UInt_32_Type    => 16#09#,
      UInt_64_Type    => 16#0A#,
      Float_32_Type   => 16#0B#,
      Float_64_Type   => 16#0C#,
      String_Type     => 16#0D#,
      Crc_String_Type => 16#0E#,
      Binary_Type     => 16#0F#,
      Crc_Binary_Type => 16#10#,
      Array_Type      => 16#11#,
      Dictionary_Type => 16#12#,
      Sequence_Type   => 16#13#,
      Table_Type      => 16#14#,
      UUID_Type       => 16#15#,
      RGBA_Type       => 16#16#,
      Font_Type       => 16#17#
     );

   function To_Item_Type is new Ada.Unchecked_Conversion (Unsigned_8, Item_Type);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Type, Unsigned_8);

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
      8,  -- String (includes 4 bytes of the string itself)
      8,  -- Crc_String
      8,  -- Binary (includes 4 bytes of the binary itself)
      8,  -- Crc_Binary
      32, -- Array
      8,  -- Dictionary
      8,  -- Sequence
      16, -- Table (no column descriptors included)
      16, -- UUID
      0,  -- RGBA
      8   -- Font (includes 2 bytes from the font & family)
     );

   type Item_Flags is
      record
         Flag_0: Boolean;
         Flag_1: Boolean;
         Flag_2: Boolean;
         Flag_3: Boolean;
         Flag_4: Boolean;
         Flag_5: Boolean;
         Flag_6: Boolean;
         Flag_7: Boolean;
      end record;

   for Item_Flags use
      record
         Flag_0 at 0 range 0..0;
         Flag_1 at 0 range 1..1;
         Flag_2 at 0 range 2..2;
         Flag_3 at 0 range 3..3;
         Flag_4 at 0 range 4..4;
         Flag_5 at 0 range 5..5;
         Flag_6 at 0 range 6..6;
         Flag_7 at 0 range 7..7;
      end record;

   function To_Item_Flags is new Ada.Unchecked_Conversion (Unsigned_8, Item_Flags);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Flags, Unsigned_8);

   No_Item_Flags: constant Item_Flags := (false, false, false, false, false, false, false, false);


   type Item_Options is
      record
         Option_0: Boolean;
         Option_1: Boolean;
         Option_2: Boolean;
         Option_3: Boolean;
         Option_4: Boolean;
         Option_5: Boolean;
         Option_6: Boolean;
         Option_7: Boolean;
      end record;

   for Item_Options use
      record
         Option_0 at 0 range 0..0;
         Option_1 at 0 range 1..1;
         Option_2 at 0 range 2..2;
         Option_3 at 0 range 3..3;
         Option_4 at 0 range 4..4;
         Option_5 at 0 range 5..5;
         Option_6 at 0 range 6..6;
         Option_7 at 0 range 7..7;
      end record;

   function To_Item_Options is new Ada.Unchecked_Conversion (Unsigned_8, Item_Options);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Options, Unsigned_8);

   No_Item_Options: constant Item_Options := (false, false, false, false, false, false, false, false);


   -- ====================================================
   -- For Item Names

   Max_Name_Length: constant Integer := 248;


end BRBON.Types;

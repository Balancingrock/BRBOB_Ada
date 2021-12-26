-- =====================================================================================================================
--
--  File:       Storage_Area.adb
--  Project:    BRBON
--
--  Version:    0.1.0
--
--  Author:     Marinus van der Lugt
--  Company:    http://balancingrock.nl
--  Repository: https://github.com/Balancingrock/BRBON_Ada
--
--  Copyright:  (c) 2021 Marinus van der Lugt, All rights reserved.
--
--  License:    MIT, see LICENSE file
--
--  And because I too need to make a living:
--
--   - You can send payment (you choose the amount) via paypal to: sales@balancingrock.nl
--   - Or wire bitcoins to: 1GacSREBxPy1yskLMc9de2nofNv2SNdwqH
--
--  If you prefer to pay in another way, please contact me at rien@balancingrock.nl
--
--  Prices/Quotes for support, modifications or enhancements can also be obtained from: rien@balancingrock.nl
--
-- =====================================================================================================================
-- Purpose
--
-- See specification
--
-- =====================================================================================================================
-- History
--
-- 0.1.0 - Initial version
--
-- =====================================================================================================================

with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Byte_Swapping;

with BRBON.Utils;
with CRC_Package;


package body BRBON.Container is


   -- Swap functions
   --
   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);
   function Swap_Unsigned_64 is new GNAT.Byte_Swapping.Swapped8 (Unsigned_64);
   function Swap_Integer_16 is new GNAT.Byte_Swapping.Swapped2 (Integer_16);
   function Swap_Integer_32 is new GNAT.Byte_Swapping.Swapped4 (Integer_32);
   function Swap_Integer_64 is new GNAT.Byte_Swapping.Swapped8 (Integer_64);
   function Swap_Float_32 is new GNAT.Byte_Swapping.Swapped4 (IEEE_Float_32);
   function Swap_Float_64 is new GNAT.Byte_Swapping.Swapped8 (IEEE_Float_64);

-- -----------------------------------------------------------------------------

   function Factory (In_Buffer_Ptr: BRBON.Unsigned_8_Array_Ptr; For_Byte_Order: BRBON.Byte_Storage_Order) return Block is
   
      B: BRBON.Block;
   
   begin
      
      B.Data := Buffer_Ptr;
      B.Swap := For_Byte_Order /= BRBON.Machine_Byte_Storage_Order;
      
      if BRBON.Zero_Storage then
         B.Data.all := (others => 0);
      end if;
      
      return B;
      
   end Factory;

-- -----------------------------------------------------------------------------

   function Factory (In_Buffer_Ptr: BRBON.Unsigned_8_Array_Ptr; Path: String; For_Byte_Order: BRBON.Byte_Storage_Order) return Instance;

      File: Ada.Streams.Stream_IO.File_Type;
      File_Size: Unsigned_64;
      In_Stream: Stream_Access;
      B: BRBON.Block;

   begin
   
      Open (File => File,
            Mode => In_File,
            Name => Path);
   
      File_Size := Unsigned_64 (Size (File));
   
      In_Stream := Stream(File);
   
      if File_Size > Buffer_Ptr.all'Length then
         raise BRBON.Storage_Warning with "File too large for buffer";
      end if;
   
      Array_Of_Unsigned_8'Read (In_Stream, Buffer_Ptr.all); -- Filling to less than the upper limit is possible/expected
   
      B.Data := Buffer_Ptr;
      B.Swap := Using_Endianness /= Configure.Machine_Endianness;
     
      return B;
      
   end Factory;

-- -----------------------------------------------------------------------------

   procedure Write_to_File (B: BRBON.Block; Path: String) is
   
      File: Ada.Streams.Stream_IO.File_Type;
      subtype T is BRBON.Unsigned_8_Array (B.Data'First .. B.Data'Last);
      Out_Stream: Stream_Access;
      
   begin
   
      Create (File => File,
              Mode => Out_File,
              Name => Path);
   
      Out_Stream := Stream (File);
   
      T'Write (Out_Stream, B.Data.all);
   
      Close (File);
   
   end Write_to_File;

-- -----------------------------------------------------------------------------

   function Storage_Byte_Count (B: BRBON.Block) return Unsigned_32 is
   
   begin
   
      return Unsigned_32 (B.Data'Length);
   
   end Byte_Count;

-- -----------------------------------------------------------------------------

   function Storage_Byte_Order (B: BRBON.Block) return BRBON.Byte_Storage_Order is
   
   begin
   
      if BRBON.Machine_Byte_Storage_Order = BRBON.MSB_First then
         if B.Swap then
            return BRBON.LSB_First;
         else
            return BRBON.MSB_First;
         end if;
      else
         if B.Swap then
            return BRBON.MSB_First;
         else
            return BRBON.LSB_First;
         end if;
      end if;
   end Storage_Byte_Order;

-- -----------------------------------------------------------------------------

   procedure Test_Support_Get_Bytes (B: BRBON.Block; Start: Unsigned_32; Dest: out BRBON.Unsigned_8_Array) is
   
      Index: Unsigned_32 := Start;
   
   begin
   
      if Dest'Length > 0 then
   
         for I in Dest'Range loop
   
            if Index <= B.Data'Last then
               Dest (I) := B.Data (Index);
            else
               Dest (I) := 0;
            end if;
   
            Index := Index + 1;
   
         end loop;
   
      end if;
   
   end Test_Support_Get_Bytes;

-- -----------------------------------------------------------------------------

   procedure Set_Bool (B: BRBON.Block; Offset: Unsigned_32; Value: Boolean) is
   
   begin
   
      B.Data (Offset) := (if Value then 1 else 0);
   
   end Set_Bool;

-- -----------------------------------------------------------------------------

   function Get_Bool (B: BRBON.Block; Offset: Unsigned_32) return Boolean is
   
   begin
   
      return B.Data (Offset) /= 0;
   
   end Get_Bool;

-- -----------------------------------------------------------------------------

   procedure Set_Unsigned_8 (B: BRBON.Block; Offset: Unsigned_32; Value: Unsigned_8) is
   
   begin
      
      B.Data (Offset) := Value;
   
   end Set_Unsigned_8;

-- -----------------------------------------------------------------------------

   function Get_Unsigned_8 (B: BRBON.Block; Offset: Unsigned_32) return Unsigned_8 is
   
   begin
   
      return B.Data (Offset);
      
   end Get_Unsigned_8;

-- -----------------------------------------------------------------------------

   procedure Set_Unsigned_16 (B: BRBON.Block; Offset: Unsigned_32; Value: Unsigned_16) is
   
   begin
   
      if B.Swap then
      
         B.Data (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      
      else
      
         B.Data (Offset .. Offset + 1) := To_Two_Bytes (Value);
      
      end if;
      
   end Set_Unsigned_16;

-- -----------------------------------------------------------------------------

   function Get_Unsigned_16 (B: BRBON.Block; Offset: Unsigned_32) return Unsigned_16 is
   
      Value: Unsigned_16 := To_Unsigned_16 (B.Data (Offset .. Offset + 1));
   
   begin
   
      if B.Swap then
      
         return Swap_Unsigned_16 (Value);
      
      else
      
         return Value;
      
      end if;
   
   end Get_Unsigned_16;

-- -----------------------------------------------------------------------------

   procedure Set_Unsigned_32 (B: BRBON.Block; Offset: Unsigned_32; Value: Unsigned_32) is
   
   begin
   
      if B.Swap then
      
         B.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      
      else
      
         B.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      
      end if;
      
   end Set_Unsigned_32;

-- -----------------------------------------------------------------------------

   function Get_Unsigned_32 (B: BRBON.Block; Offset: Unsigned_32) return Unsigned_32 is
   
      Value: Unsigned_32 := To_Unsigned_32 (B.Data (Offset .. Offset + 3));
   
   begin
   
      if B.Swap then
      
         return Swap_Unsigned_32 (Value);
      
      else
      
         return Value;
      
      end if;
   
   end Get_Unsigned_32;

-- -----------------------------------------------------------------------------

   procedure Set_Unsigned_64 (B: BRBON.Block; Offset: Unsigned_32; Value: Unsigned_64) is
   
   begin
      
      if B.Swap then
      
         B.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      
      else
      
         B.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      
      end if;
      
   end Set_Unsigned_64;

-- -----------------------------------------------------------------------------

   function Get_Unsigned_64 (B: BRBON.Block; Offset: Unsigned_32) return Unsigned_64 is

      Value: Unsigned_64 := To_Unsigned_64 (B.Data (Offset .. Offset + 7));

   begin

      if B.Swap then

         return Swap_Unsigned_64 (Value);

      else

         return Value;

      end if;

   end Get_Unsigned_64;

-- -----------------------------------------------------------------------------

   procedure Set_Integer_8 (B: BRBON.Block; Offset: Unsigned_32; Value: Integer_8) is
   
   begin
   
      B.Data (Offset) := To_Unsigned_8 (Value);
   
   end Set_Integer_8;

-- -----------------------------------------------------------------------------

   function Get_Integer_8 (B: BRBON.Block; Offset: Unsigned_32) return Integer_8 is
   
   begin
   
        return To_Integer_8 (B.Data (Offset));
   
   end Get_Integer_8;

-- -----------------------------------------------------------------------------

   procedure Set_Integer_16 (B: BRBON.Block; Offset: Unsigned_32; Value: Integer_16) is
   
   begin
   
      if B.Swap then
   
         B.Data (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
   
      else
   
         B.Data (Offset .. Offset + 1) := To_Two_Bytes (Value);
   
      end if;
   
   end Set_Integer_16;

-- -----------------------------------------------------------------------------

   function Get_Integer_16 (B: BRBON.Block; Offset: Unsigned_32) return Integer_16 is
   
      Value: Integer_16 := To_Integer_16 (B.Data (Offset .. Offset + 1));
   
   begin
   
      if B.Swap then
   
         return Swap_Integer_16 (Value);
   
      else
   
         return Value;
   
      end if;
   
   end Get_Integer_16;

-- -----------------------------------------------------------------------------

   procedure Set_Integer_32 (B: BRBON.Block; Offset: Unsigned_32; Value: Integer_32) is

   begin

      if B.Swap then

         B.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));

      else

         B.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);

      end if;

   end Set_Integer_32;

-- -----------------------------------------------------------------------------

   function Get_Integer_32 (B: BRBON.Block; Offset: Unsigned_32) return Integer_32 is

      Value: Integer_32 := To_Integer_32 (B.Data (Offset .. Offset + 3));

   begin

      if B.Swap then

         return Swap_Integer_32 (Value);

      else

         return Value;

      end if;

   end Get_Integer_32;

-- -----------------------------------------------------------------------------

   procedure Set_Integer_64 (B: BRBON.Block; Offset: Unsigned_32; Value: Integer_64) is

   begin

      if B.Swap then

         B.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));

      else

         B.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);

      end if;

   end Set_Integer_64;

-- -----------------------------------------------------------------------------

   function Get_Integer_64 (B: BRBON.Block; Offset: Unsigned_32) return Integer_64 is

      Value: Integer_64 := To_Integer_64 (B.Data (Offset .. Offset + 7));

   begin

      if B.Swap then
      
       return Swap_Integer_64 (Value);
      
      else
      
       return Value;
      
      end if;

   end Get_Integer_64;

-- -----------------------------------------------------------------------------

   procedure Set_Float_32 (B: BRBON.Block; Offset: Unsigned_32; Value: IEEE_Float_32) is

   begin

      if B.Swap then

         B.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));

      else

         B.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);

      end if;

   end Set_Float_32;

-- -----------------------------------------------------------------------------

   function Get_Float_32 (B: BRBON.Block; Offset: Unsigned_32) return IEEE_Float_32 is

      Value: IEEE_Float_32 := To_Float_32 (B.Data (Offset .. Offset + 3));

   begin

      if B.Swap then

         return Swap_Float_32 (Value);

      else

         return Value;

      end if;

   end Get_Float_32;

-- -----------------------------------------------------------------------------

   procedure Set_Float_64 (B: BRBON.Block; Offset: Unsigned_32; Value: IEEE_Float_64) is

   begin

      if B.Swap then

         B.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));

      else

         B.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);

      end if;

   end Set_Float_64;

-- -----------------------------------------------------------------------------

   function Get_Float_64 (B: BRBON.Block; Offset: Unsigned_32) return IEEE_Float_64 is

      Value: IEEE_Float_64 := To_Float_64 (B.Data (Offset .. Offset + 7));

   begin

      if B.Swap then

         return Swap_Float_64 (Value);

      else

         return Value;

      end if;

   end Get_Float_64;

-- -----------------------------------------------------------------------------

   function Get_String (B: BRBON.Block; Offset: Unsigned_32; Length: Unsigned_32) return String is

      subtype Str_T is String (1 .. Integer (Length));
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Unsigned_32 (Length));
      function To_Str_T is new Ada.Unchecked_Conversion (Arr_T, Str_T);

   begin

      return To_Str_T (Arr_T (B.Data (Offset .. Offset + Length - 1)));

   end Get_String;

-- -----------------------------------------------------------------------------

   procedure Set_String (B: BRBON.Block; Offset: Unsigned_32; Value: String) is
   
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Value'Length);
      subtype Str_T is String (1 .. Value'Length);
      function To_Arr_T is new Ada.Unchecked_Conversion (Str_T, Arr_T);
   
   begin
   
      if Value'Length > 0 then
   
         B.Data (Offset .. Offset + Value'Length - 1) := To_Arr_T (Value);
   
      end if;
   
   end Set_String;

-- -----------------------------------------------------------------------------

   function Get_Unsigned_8_Array (B: BRBON.Block; Offset: Unsigned_32; Length: Unsigned_32) return BRBON.Unsigned_8_Array is

   begin

      return B.Data (Offset .. Offset + Length - 1);

   end Get_Unsigned_8_Array;

-- -----------------------------------------------------------------------------

   procedure Set_Unsigned_8_Array (B: BRBON.Block; Offset: Unsigned_32; Value: BRBON.Unsigned_8_Array) is
   
   begin
   
      if Value'Length > 0 then
   
         B.Data (Offset .. Offset + Value'Length - 1) := Value;
   
      end if;
   
   end Set_Unsigned_8_Array;

-- -----------------------------------------------------------------------------

   function Get_CRC_16_Over_Range (B: BRBON.Block; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_16 is

   begin

      return CRC_Package.Calculate_CRC_16 (Arr => B.Data (Start .. (Start + Count - 1)));

   end Get_CRC_16_Over_Range;

-- -----------------------------------------------------------------------------

   function Get_CRC_32_Over_Range (B: BRBON.Block; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_32 is

   begin

      return CRC_Package.Calculate_CRC_32 (Arr => B.Data (Start .. (Start + Count - 1)));

   end Get_CRC_32_Over_Range;

-- -----------------------------------------------------------------------------

   procedure Test_Support_Hex_Dump (B: BRBON.Block) is

   begin

      Utils.Put_Hex (B.Data.all);

   end Test_Support_Hex_Dump;


end BRBON.Container;

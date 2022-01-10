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
with BRBON.Types;
with CRC_Package;


package body BRBON.Container_Package is


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

   
   -----------------------------------------------------------------------------
   
   procedure Setup
     (
      C: in out Container;
      Memory_Ptr: Unsigned_8_Array_Ptr;
      Byte_Order: Byte_Storage_Order
     ) is
   
   begin
      
      C.MPtr := Memory_Ptr;
      C.Swap := Byte_Order /= Machine_Byte_Storage_Order;
      
      if Zero_Storage then
         C.MPtr.all := (others => 0);
      end if;
   
   end Setup;
   
   
   -----------------------------------------------------------------------------

   function Factory (Memory_Ptr: Unsigned_8_Array_Ptr; Byte_Order: Byte_Storage_Order) return Container is
   
      C: Container;
   
   begin
      
      Setup (C, Memory_Ptr, Byte_Order);
      
      return C;
      
   end Factory;

   
   -----------------------------------------------------------------------------

   function Factory (Memory_Ptr: in out Unsigned_8_Array_Ptr; Filepath: String; Byte_Order: Byte_Storage_Order) return Container is

      File: Ada.Streams.Stream_IO.File_Type;
      File_Size: Unsigned_64;
      In_Stream: Stream_Access;
      C: Container;

   begin
   
      Open (File => File,
            Mode => In_File,
            Name => Filepath);
   
      File_Size := Unsigned_64 (Size (File));
   
      In_Stream := Stream (File);
   
      if File_Size > Memory_Ptr.all'Length then
         raise Storage_Warning with "File too large for buffer";
      end if;
   
      Unsigned_8_Array'Read (In_Stream, Memory_Ptr.all); -- Filling to less than the upper limit is possible/expected
   
      
      -- If a BRBON block was read the byte storage order may be wrong, that must be corrected later
      --
      C.MPtr := Memory_Ptr;
      C.Swap := Byte_Order /= Machine_Byte_Storage_Order;
     
      return C;
      
   end Factory;


   -----------------------------------------------------------------------------

   procedure Write_To_File (C: Container; Filepath: String) is
   
      File: Ada.Streams.Stream_IO.File_Type;
      subtype T is Unsigned_8_Array (C.MPtr'First .. C.MPtr'Last);
      Out_Stream: Stream_Access;
      
   begin
   
      Create (File => File,
              Mode => Out_File,
              Name => Filepath);
   
      Out_Stream := Stream (File);
   
      T'Write (Out_Stream, C.MPtr.all);
   
      Close (File);
   
   end Write_To_File;


   -----------------------------------------------------------------------------

   function Byte_Count (C: Container) return Unsigned_32 is
   
   begin
   
      return Unsigned_32 (C.MPtr'Length);
   
   end Byte_Count;


   -----------------------------------------------------------------------------

   function Swap_Status (C: Container) return Boolean is
   
   begin
   
      return C.Swap;

   end Swap_Status;



   -----------------------------------------------------------------------------

   procedure Set_Bool (C: Container; Offset: Unsigned_32; Value: Boolean) is
   
   begin
   
      C.MPtr (Offset) := (if Value then 1 else 0);
   
   end Set_Bool;

   
   -----------------------------------------------------------------------------

   function Get_Bool (C: Container; Offset: Unsigned_32) return Boolean is
   
   begin
   
      return C.MPtr (Offset) /= 0;
   
   end Get_Bool;

   
   -----------------------------------------------------------------------------

   procedure Set_Unsigned_8 (C: Container; Offset: Unsigned_32; Value: Unsigned_8) is
   
   begin
      
      C.MPtr (Offset) := Value;
   
   end Set_Unsigned_8;

   
   -----------------------------------------------------------------------------

   function Get_Unsigned_8 (C: Container; Offset: Unsigned_32) return Unsigned_8 is
   
   begin
   
      return C.MPtr (Offset);
      
   end Get_Unsigned_8;

   
   -----------------------------------------------------------------------------

   procedure Set_Unsigned_16 (C: Container; Offset: Unsigned_32; Value: Unsigned_16) is
   
   begin
   
      if C.Swap then
      
         C.MPtr (Offset .. Offset + 1) := Types.To_Two_Bytes (Swap_Unsigned_16 (Value));
      
      else
      
         C.MPtr (Offset .. Offset + 1) := Types.To_Two_Bytes (Value);
      
      end if;
      
   end Set_Unsigned_16;

   
   -----------------------------------------------------------------------------

   function Get_Unsigned_16 (C: Container; Offset: Unsigned_32) return Unsigned_16 is
   
      Value: Unsigned_16 := Types.To_Unsigned_16 (C.MPtr (Offset .. Offset + 1));
   
   begin
   
      if C.Swap then
      
         return Swap_Unsigned_16 (Value);
      
      else
      
         return Value;
      
      end if;
   
   end Get_Unsigned_16;

   
   -----------------------------------------------------------------------------

   procedure Set_Unsigned_32 (C: Container; Offset: Unsigned_32; Value: Unsigned_32) is
   
   begin
   
      if C.Swap then
      
         C.MPtr (Offset .. Offset + 3) := Types.To_Four_Bytes (Swap_Unsigned_32 (Value));
      
      else
      
         C.MPtr (Offset .. Offset + 3) := Types.To_Four_Bytes (Value);
      
      end if;
      
   end Set_Unsigned_32;

   
   -----------------------------------------------------------------------------

   function Get_Unsigned_32 (C: Container; Offset: Unsigned_32) return Unsigned_32 is
   
      Value: Unsigned_32 := Types.To_Unsigned_32 (C.MPtr (Offset .. Offset + 3));
   
   begin
   
      if C.Swap then
      
         return Swap_Unsigned_32 (Value);
      
      else
      
         return Value;
      
      end if;
   
   end Get_Unsigned_32;

   
   -----------------------------------------------------------------------------

   procedure Set_Unsigned_64 (C: Container; Offset: Unsigned_32; Value: Unsigned_64) is
   
   begin
      
      if C.Swap then
      
         C.MPtr (Offset .. Offset + 7) := Types.To_Eight_Bytes (Swap_Unsigned_64 (Value));
      
      else
      
         C.MPtr (Offset .. Offset + 7) := Types.To_Eight_Bytes (Value);
      
      end if;
      
   end Set_Unsigned_64;

   
   -----------------------------------------------------------------------------

   function Get_Unsigned_64 (C: Container; Offset: Unsigned_32) return Unsigned_64 is

      Value: Unsigned_64 := Types.To_Unsigned_64 (C.MPtr (Offset .. Offset + 7));

   begin

      if C.Swap then

         return Swap_Unsigned_64 (Value);

      else

         return Value;

      end if;

   end Get_Unsigned_64;

   
   -----------------------------------------------------------------------------

   procedure Set_Integer_8 (C: Container; Offset: Unsigned_32; Value: Integer_8) is
   
   begin
   
      C.MPtr (Offset) := Types.To_Unsigned_8 (Value);
   
   end Set_Integer_8;

   
   -----------------------------------------------------------------------------

   function Get_Integer_8 (C: Container; Offset: Unsigned_32) return Integer_8 is
   
   begin
   
        return Types.To_Integer_8 (C.MPtr (Offset));
   
   end Get_Integer_8;

   
   -----------------------------------------------------------------------------

   procedure Set_Integer_16 (C: Container; Offset: Unsigned_32; Value: Integer_16) is
   
   begin
   
      if C.Swap then
   
         C.MPtr (Offset .. Offset + 1) := Types.To_Two_Bytes (Swap_Integer_16 (Value));
   
      else
   
         C.MPtr (Offset .. Offset + 1) := Types.To_Two_Bytes (Value);
   
      end if;
   
   end Set_Integer_16;

   
   -----------------------------------------------------------------------------

   function Get_Integer_16 (C: Container; Offset: Unsigned_32) return Integer_16 is
   
      Value: Integer_16 := Types.To_Integer_16 (C.MPtr (Offset .. Offset + 1));
   
   begin
   
      if C.Swap then
   
         return Swap_Integer_16 (Value);
   
      else
   
         return Value;
   
      end if;
   
   end Get_Integer_16;

   
   -----------------------------------------------------------------------------

   procedure Set_Integer_32 (C: Container; Offset: Unsigned_32; Value: Integer_32) is

   begin

      if C.Swap then

         C.MPtr (Offset .. Offset + 3) := Types.To_Four_Bytes (Swap_Integer_32 (Value));

      else

         C.MPtr (Offset .. Offset + 3) :=Types. To_Four_Bytes (Value);

      end if;

   end Set_Integer_32;

   
   -----------------------------------------------------------------------------

   function Get_Integer_32 (C: Container; Offset: Unsigned_32) return Integer_32 is

      Value: Integer_32 := Types.To_Integer_32 (C.MPtr (Offset .. Offset + 3));

   begin

      if C.Swap then

         return Swap_Integer_32 (Value);

      else

         return Value;

      end if;

   end Get_Integer_32;


   -----------------------------------------------------------------------------

   procedure Set_Integer_64 (C: Container; Offset: Unsigned_32; Value: Integer_64) is

   begin

      if C.Swap then

         C.MPtr (Offset .. Offset + 7) := Types.To_Eight_Bytes (Swap_Integer_64 (Value));

      else

         C.MPtr (Offset .. Offset + 7) := Types.To_Eight_Bytes (Value);

      end if;

   end Set_Integer_64;

   
   -----------------------------------------------------------------------------

   function Get_Integer_64 (C: Container; Offset: Unsigned_32) return Integer_64 is

      Value: Integer_64 := Types.To_Integer_64 (C.MPtr (Offset .. Offset + 7));

   begin

      if C.Swap then
      
       return Swap_Integer_64 (Value);
      
      else
      
       return Value;
      
      end if;

   end Get_Integer_64;

   
   -----------------------------------------------------------------------------

   procedure Set_Float_32 (C: Container; Offset: Unsigned_32; Value: IEEE_Float_32) is

   begin

      if C.Swap then

         C.MPtr (Offset .. Offset + 3) := Types.To_Four_Bytes (Swap_Float_32 (Value));

      else

         C.MPtr (Offset .. Offset + 3) := Types.To_Four_Bytes (Value);

      end if;

   end Set_Float_32;

   
   -----------------------------------------------------------------------------

   function Get_Float_32 (C: Container; Offset: Unsigned_32) return IEEE_Float_32 is

      Value: IEEE_Float_32 := Types.To_Float_32 (C.MPtr (Offset .. Offset + 3));

   begin

      if C.Swap then

         return Swap_Float_32 (Value);

      else

         return Value;

      end if;

   end Get_Float_32;


   -----------------------------------------------------------------------------

   procedure Set_Float_64 (C: Container; Offset: Unsigned_32; Value: IEEE_Float_64) is

   begin

      if C.Swap then

         C.MPtr (Offset .. Offset + 7) := Types.To_Eight_Bytes (Swap_Float_64 (Value));

      else

         C.MPtr (Offset .. Offset + 7) := Types.To_Eight_Bytes (Value);

      end if;

   end Set_Float_64;


   -----------------------------------------------------------------------------

   function Get_Float_64 (C: Container; Offset: Unsigned_32) return IEEE_Float_64 is

      Value: IEEE_Float_64 := Types.To_Float_64 (C.MPtr (Offset .. Offset + 7));

   begin

      if C.Swap then

         return Swap_Float_64 (Value);

      else

         return Value;

      end if;

   end Get_Float_64;

   
   -----------------------------------------------------------------------------

   function Get_String (C: Container; Offset: Unsigned_32; Length: Unsigned_32) return String is

      subtype Str_T is String (1 .. Integer (Length));
      subtype Arr_T is Unsigned_8_Array (1 .. Unsigned_32 (Length));
      function To_Str_T is new Ada.Unchecked_Conversion (Arr_T, Str_T);

   begin

      return To_Str_T (Arr_T (C.MPtr (Offset .. Offset + Length - 1)));

   end Get_String;

   
   -----------------------------------------------------------------------------

   procedure Set_String (C: Container; Offset: Unsigned_32; Value: String) is
   
      subtype Arr_T is Unsigned_8_Array (1 .. Value'Length);
      subtype Str_T is String (1 .. Value'Length);
      function To_Arr_T is new Ada.Unchecked_Conversion (Str_T, Arr_T);
   
   begin
   
      if Value'Length > 0 then
   
         C.MPtr (Offset .. Offset + Value'Length - 1) := To_Arr_T (Value);
   
      end if;
   
   end Set_String;

   
   -----------------------------------------------------------------------------

   function Get_Unsigned_8_Array (C: Container; Offset: Unsigned_32; Length: Unsigned_32) return Unsigned_8_Array is

   begin

      return C.MPtr (Offset .. Offset + Length - 1);

   end Get_Unsigned_8_Array;

   
   -----------------------------------------------------------------------------

   procedure Set_Unsigned_8_Array (C: Container; Offset: Unsigned_32; Value: Unsigned_8_Array) is
   
   begin
   
      if Value'Length > 0 then
   
         C.MPtr (Offset .. Offset + Value'Length - 1) := Value;
   
      end if;
   
   end Set_Unsigned_8_Array;

   
   -----------------------------------------------------------------------------

   function Get_CRC_16_Over_Range (C: Container; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_16 is

   begin

      return CRC_Package.Calculate_CRC_16 (Arr => C.MPtr (Start .. (Start + Count - 1)));

   end Get_CRC_16_Over_Range;

   
   -----------------------------------------------------------------------------

   function Get_CRC_32_Over_Range (C: Container; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_32 is

   begin

      return CRC_Package.Calculate_CRC_32 (Arr => C.MPtr (Start .. (Start + Count - 1)));

   end Get_CRC_32_Over_Range;

   
   -----------------------------------------------------------------------------

   procedure Test_Support_Hex_Dump (C: Container) is

   begin

      Utils.Put_Hex (C.MPtr.all);

   end Test_Support_Hex_Dump;

   
   -----------------------------------------------------------------------------

   procedure Test_Support_Get_Bytes (C: Container; Start: Unsigned_32; Dest: out Unsigned_8_Array) is
   
      Index: Unsigned_32 := Start;
   
   begin
   
      if Dest'Length > 0 then
   
         for I in Dest'Range loop
   
            if Index <= C.MPtr'Last then
               Dest (I) := C.MPtr (Index);
            else
               Dest (I) := 0;
            end if;
   
            Index := Index + 1;
   
         end loop;
   
      end if;
   
   end Test_Support_Get_Bytes;

   
   -----------------------------------------------------------------------------
   
   procedure Set_Data_Byte_Order (C: in out Container; Value: Byte_Storage_Order) is
   
   begin
   
      C.Swap := Value /= BRBON.Machine_Byte_Storage_Order;
   
   end Set_Data_Byte_Order;

   
end BRBON.Container_Package;

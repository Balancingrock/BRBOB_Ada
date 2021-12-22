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


   function Factory (Buffer_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Instance is
      Byte_Count: Unsigned_32 := BRBON.Utils.Round_Down_To_Nearest_Multiple_of_32 (Unsigned_32 (Buffer_Ptr.all'Length));
      S: Instance;
   begin
      -- if Byte_Count < (Minimum_Item_Byte_Count (BR_Bool) + Minimum_Block_Byte_Count (Single_Item_File)) then raise Buffer_Error with "Buffer too small"; end if;
      S.Data := Buffer_Ptr;
      S.Swap := Using_Endianness /= Configure.Machine_Endianness;
      if Configure.Zero_Storage then S.Data.all := (others => 0); end if;
      return S;
   end Factory;


   function Factory (Buffer_Ptr: Array_Of_Unsigned_8_Ptr; Path: String; Using_Endianness: Endianness) return Instance is
      File: Ada.Streams.Stream_IO.File_Type;
      File_Size: Unsigned_64;
      In_Stream: Stream_Access;
      S: Instance;
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
      S.Data := Buffer_Ptr;
      S.Swap := Using_Endianness /= Configure.Machine_Endianness;
      return S;
   end Factory;

   procedure Write_to_File (CPtr: Instance_Ptr; Path: String) is
      File: Ada.Streams.Stream_IO.File_Type;
      subtype T is Array_Of_Unsigned_8 (CPtr.Data'First .. CPtr.Data'Last);
      Out_Stream: Stream_Access;
   begin
      Create (File => File,
              Mode => Out_File,
              Name => Path);
      Out_Stream := Stream (File);
      T'Write (Out_Stream, CPtr.Data.all);
      Close (File);
   end Write_to_File;


   function Byte_Count (CPtr: Instance_Ptr) return Unsigned_32 is
   begin
      return Unsigned_32 (CPtr.Data'Length);
   end Byte_Count;

   function Uses_Endianness (Cptr: Instance_Ptr) return Endianness is
   begin
      if Configure.Machine_Endianness = Types.Big then
         if CPtr.Swap then
            return Types.Little;
         else
            return Types.Big;
         end if;
      else
         if CPtr.Swap then
            return Types.Big;
         else
            return Types.Little;
         end if;
      end if;
   end Uses_Endianness;


   -- Test support

   procedure Test_Support_Get_Bytes (CPtr: Instance_Ptr; Start: Unsigned_32; Dest: out Array_Of_Unsigned_8) is
      Index: Unsigned_32 := Start;
   begin
      if Dest'Length > 0 then
         for I in Dest'Range loop
            if Index <= CPtr.Data'Last then
               Dest (I) := Cptr.Data (Index);
            else
               Dest (I) := 0;
            end if;
            Index := Index + 1;
         end loop;
      end if;
   end Test_Support_Get_Bytes;


   -- Operational

   procedure Set_Data_Endianness (CPtr: Instance_Ptr; Value: Endianness) is
   begin
      CPtr.Swap := Value /= Configure.Machine_Endianness;
   end Set_Data_Endianness;


   procedure Set_Bool (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Boolean) is
   begin
      CPtr.Data (Offset) := (if Value then 1 else 0);
   end Set_Bool;

   function Get_Bool (CPtr: Instance_Ptr; Offset: Unsigned_32) return Boolean is
   begin
      return CPtr.Data (Offset) /= 0;
   end Get_Bool;


   procedure Set_Unsigned_8 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      CPtr.Data (Offset) := Value;
   end Set_Unsigned_8;

   function Get_Unsigned_8 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Unsigned_8 is
   begin
        return CPtr.Data (Offset);
   end Get_Unsigned_8;


   procedure Set_Unsigned_16 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      else
         CPtr.Data (Offset .. Offset + 1) := To_Two_Bytes (Value);
      end if;
   end Set_Unsigned_16;

   function Get_Unsigned_16 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Unsigned_16 is
      Value: Unsigned_16 := To_Unsigned_16 (CPtr.Data (Offset .. Offset + 1));
   begin
      if CPtr.Swap then
         return Swap_Unsigned_16 (Value);
      else
         return Value;
      end if;
   end Get_Unsigned_16;


   procedure Set_Unsigned_32 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      else
         CPtr.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      end if;
   end Set_Unsigned_32;

   function Get_Unsigned_32 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Unsigned_32 is
      Value: Unsigned_32 := To_Unsigned_32 (CPtr.Data (Offset .. Offset + 3));
   begin
      if CPtr.Swap then
         return Swap_Unsigned_32 (Value);
      else
         return Value;
      end if;
   end Get_Unsigned_32;


   procedure Set_Unsigned_64 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      else
         CPtr.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      end if;
   end Set_Unsigned_64;

   function Get_Unsigned_64 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Unsigned_64 is
      Value: Unsigned_64 := To_Unsigned_64 (CPtr.Data (Offset .. Offset + 7));
   begin
      if CPtr.Swap then
         return Swap_Unsigned_64 (Value);
      else
         return Value;
      end if;
   end Get_Unsigned_64;


   procedure Set_Integer_8 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Integer_8) is
   begin
      CPtr.Data (Offset) := To_Unsigned_8 (Value);
   end Set_Integer_8;

   function Get_Integer_8 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Integer_8 is
   begin
        return To_Integer_8 (CPtr.Data (Offset));
   end Get_Integer_8;


   procedure Set_Integer_16 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Integer_16) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
      else
         CPtr.Data (Offset .. Offset + 1) := To_Two_Bytes (Value);
      end if;
   end Set_Integer_16;

   function Get_Integer_16 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Integer_16 is
      Value: Integer_16 := To_Integer_16 (CPtr.Data (Offset .. Offset + 1));
   begin
      if CPtr.Swap then
         return Swap_Integer_16 (Value);
      else
         return Value;
      end if;
   end Get_Integer_16;


   procedure Set_Integer_32 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Integer_32) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));
      else
         CPtr.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      end if;
   end Set_Integer_32;

   function Get_Integer_32 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Integer_32 is
      Value: Integer_32 := To_Integer_32 (CPtr.Data (Offset .. Offset + 3));
   begin
      if CPtr.Swap then
         return Swap_Integer_32 (Value);
      else
         return Value;
      end if;
   end Get_Integer_32;


   procedure Set_Integer_64 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Integer_64) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));
      else
         CPtr.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      end if;
   end Set_Integer_64;

   function Get_Integer_64 (CPtr: Instance_Ptr; Offset: Unsigned_32) return Integer_64 is
      Value: Integer_64 := To_Integer_64 (CPtr.Data (Offset .. Offset + 7));
   begin
      if CPtr.Swap then
         return Swap_Integer_64 (Value);
      else
         return Value;
      end if;
   end Get_Integer_64;


   procedure Set_Float_32 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));
      else
         CPtr.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      end if;
   end Set_Float_32;

   function Get_Float_32 (CPtr: Instance_Ptr; Offset: Unsigned_32) return IEEE_Float_32 is
      Value: IEEE_Float_32 := To_Float_32 (CPtr.Data (Offset .. Offset + 3));
   begin
      if CPtr.Swap then
         return Swap_Float_32 (Value);
      else
         return Value;
      end if;
   end Get_Float_32;


   procedure Set_Float_64 (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      if CPtr.Swap then
         CPtr.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));
      else
         CPtr.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      end if;
   end Set_Float_64;

   function Get_Float_64 (CPtr: Instance_Ptr; Offset: Unsigned_32) return IEEE_Float_64 is
      Value: IEEE_Float_64 := To_Float_64 (CPtr.Data (Offset .. Offset + 7));
   begin
      if CPtr.Swap then
         return Swap_Float_64 (Value);
      else
         return Value;
      end if;
   end Get_Float_64;


   function Get_String (CPtr: Instance_Ptr; Offset: Unsigned_32; Length: Unsigned_32) return String is
      subtype Str_T is String (1 .. Integer (Length));
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Unsigned_32 (Length));
      function To_Str_T is new Ada.Unchecked_Conversion (Arr_T, Str_T);
   begin
      return To_Str_T (Arr_T (CPtr.Data (Offset .. Offset + Length - 1)));
   end Get_String;


   procedure Set_String (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: String) is
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Value'Length);
      subtype Str_T is String (1 .. Value'Length);
      function To_Arr_T is new Ada.Unchecked_Conversion (Str_T, Arr_T);
   begin
      if Value'Length > 0 then
         CPtr.Data (Offset .. Offset + Value'Length - 1) := To_Arr_T (Value);
      end if;
   end Set_String;


   function Get_Unsigned_8_Array (CPtr: Instance_Ptr; Offset: Unsigned_32; Length: Unsigned_32) return Array_Of_Unsigned_8 is
   begin
      return CPtr.Data (Offset .. Offset + Length - 1);
   end Get_Unsigned_8_Array;


   procedure Set_Unsigned_8_Array (CPtr: Instance_Ptr; Offset: Unsigned_32; Value: Array_Of_Unsigned_8) is
   begin
      if Value'Length > 0 then
         CPtr.Data (Offset .. Offset + Value'Length - 1) := Value;
      end if;
   end Set_Unsigned_8_Array;


   function Get_CRC_16_Over_Range (CPtr: Instance_Ptr; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_16 is
   begin
      return CRC_Package.Calculate_CRC_16 (Arr => CPtr.Data (Start .. (Start + Count - 1)));
   end Get_CRC_16_Over_Range;


   function Get_CRC_32_Over_Range (CPtr: Instance_Ptr; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_32 is
   begin
      return CRC_Package.Calculate_CRC_32 (Arr => Cptr.Data (Start .. (Start + Count - 1)));
   end Get_CRC_32_Over_Range;

   procedure Test_Support_Hex_Dump (CPtr: Instance_Ptr) is
   begin
      Utils.Put_Hex (Cptr.Data.all);
   end Test_Support_Hex_Dump;


end BRBON.Container;

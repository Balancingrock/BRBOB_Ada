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


   function Byte_Store_Factory (Buffer_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Byte_Store is
      Byte_Count: Unsigned_32 := Unsigned_32 (Buffer_Ptr.all'Length);
      S: Byte_Store;
   begin
      if Byte_Count < (Minimum_Item_Byte_Count (BR_Bool) + Minimum_Block_Byte_Count (Single_Item_File)) then raise Buffer_Error with "Buffer too small"; end if;
      S.Data := Buffer_Ptr;
      S.Swap := Using_Endianness /= Machine_Endianness;
      if Zero_Storage then S.Data.all := (others => 0); end if;
      return S;
   end Byte_Store_Factory;


   function Byte_Store_Factory (Buffer_Ptr: Array_Of_Unsigned_8_Ptr; Path: String; Using_Endianness: Endianness) return Byte_Store is
      File: Ada.Streams.Stream_IO.File_Type;
      File_Size: Unsigned_64;
      Byte_Count: Unsigned_32;
   begin
      Open (File, In_File, Path);
      File_Size := Unsigned_64 (Size (File));
      if File_Size <= Unsigned_64 (Unsigned_32'Last) then
         Byte_Count := Unsigned_32 (File_Size); -- Find out how big the storage area data component should be
         declare
            Store: Byte_Store;
            In_Stream: Stream_Access := Stream (File);
            subtype T is Array_Of_Unsigned_8 (0..Byte_Count-1);
         begin
            T'Read (In_Stream, Store.Data.all);
            Store.Swap := Using_Endianness = Machine_Endianness;
            Close (File);
            return Store;
         end;
      else
         Close(File);
         raise File_IO_Error with "File larger than Unsigned_32'Max";
      end if;
   end Byte_Store_Factory;

   procedure Write_to_File (S: in out Byte_Store'Class; Filepath: String) is
      File: Ada.Streams.Stream_IO.File_Type;
      subtype T is Array_Of_Unsigned_8 (S.Data'First .. S.Data'Last);
      Out_Stream: Stream_Access;
   begin
      Create (File => File,
              Mode => Out_File,
              Name => Filepath);
      Out_Stream := Stream (File);
      T'Write (Out_Stream, S.Data.all);
      Close (File);
   end Write_to_File;


   function Length (S: in out Byte_Store) return Unsigned_32 is
   begin
      return Unsigned_32 (S.Data'Length);
   end Length;

   function Uses_Endianness (S: in out Byte_Store) return Endianness is
   begin
      if Machine_Endianness = Big then
         if S.Swap then
            return Little;
         else
            return Big;
         end if;
      else
         if S.Swap then
            return Big;
         else
            return Little;
         end if;
      end if;
   end Uses_Endianness;


   -- Test support

   procedure Test_Support_Get_Bytes (S: in out Byte_Store; Start: Unsigned_32; Dest: out Array_Of_Unsigned_8) is
      Index: Unsigned_32 := Start;
   begin
      if Dest'Length > 0 then
         for I in Dest'Range loop
            if Index <= S.Data'Last then
               Dest (I) := S.Data (Index);
            else
               Dest (I) := 0;
            end if;
            Index := Index + 1;
         end loop;
      end if;
   end Test_Support_Get_Bytes;


   -- Operational

   procedure Set_Item_Type (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: BR_Item_Type) is
   begin
      S.Data (Offset) := To_Unsigned_8 (Value);
   end Set_Item_Type;

   function Valid_Item_Type (S: Byte_Store'Class; Offset: Unsigned_32) return Boolean is
      T: Unsigned_8 := S.Data (Offset);
   begin
      if T = 0 then return False; end if;
      return T <= To_Unsigned_8 (BR_Item_Type'Last);
   end Valid_Item_Type;

   function Get_Item_Type (S: Byte_Store'Class; Offset: Unsigned_32) return BR_Item_Type is
   begin
      if not S.Valid_Item_Type (Offset) then raise BRBON.Illegal_Item_Type; end if;
      return To_BR_Item_Type (S.Data (Offset));
   end Get_Item_Type;


   procedure Set_Item_Options (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: BR_Item_Options) is
   begin
      S.Data (Offset) := To_Unsigned_8 (Value);
   end Set_Item_Options;

   function Get_Item_Options (S: Byte_Store'Class; Offset: Unsigned_32) return BR_Item_Options is
   begin
      return To_BR_Item_Options (S.Data (Offset));
   end Get_Item_Options;


   procedure Set_Item_Flags (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: BR_Item_Flags) is
   begin
      S.Data (Offset) := To_Unsigned_8 (Value);
   end Set_Item_Flags;

   function Get_Item_Flags (S: Byte_Store'Class; Offset: Unsigned_32) return BR_Item_Flags is
   begin
      return To_BR_Item_Flags (S.Data (Offset));
   end Get_Item_Flags;


   procedure Set_Bool (S:in out Byte_Store'Class; Offset: Unsigned_32; Value: Boolean) is
   begin
      S.Data (Offset) := (if Value then 1 else 0);
   end Set_Bool;

   function Get_Bool (S: Byte_Store'Class; Offset: Unsigned_32) return Boolean is
   begin
      return S.Data (Offset) /= 0;
   end Get_Bool;


   procedure Set_Unsigned_8 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      S.Data (Offset) := Value;
   end Set_Unsigned_8;

   function Get_Unsigned_8 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_8 is
   begin
        return S.Data (Offset);
   end Get_Unsigned_8;


   procedure Set_Unsigned_16 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      else
         S.Data (Offset .. Offset + 1) := To_Two_Bytes (Value);
      end if;
   end Set_Unsigned_16;

   function Get_Unsigned_16 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_16 is
      Value: Unsigned_16 := To_Unsigned_16 (S.Data (Offset .. Offset + 1));
   begin
      if S.Swap then
         return Swap_Unsigned_16 (Value);
      else
         return Value;
      end if;
   end Get_Unsigned_16;


   procedure Set_Unsigned_32 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      else
         S.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      end if;
   end Set_Unsigned_32;

   function Get_Unsigned_32 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_32 is
      Value: Unsigned_32 := To_Unsigned_32 (S.Data (Offset .. Offset + 3));
   begin
      if S.Swap then
         return Swap_Unsigned_32 (Value);
      else
         return Value;
      end if;
   end Get_Unsigned_32;


   procedure Set_Unsigned_64 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      else
         S.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      end if;
   end Set_Unsigned_64;

   function Get_Unsigned_64 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_64 is
      Value: Unsigned_64 := To_Unsigned_64 (S.Data (Offset .. Offset + 7));
   begin
      if S.Swap then
         return Swap_Unsigned_64 (Value);
      else
         return Value;
      end if;
   end Get_Unsigned_64;


   procedure Set_Integer_8 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_8) is
   begin
      S.Data (Offset) := To_Unsigned_8 (Value);
   end Set_Integer_8;

   function Get_Integer_8 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_8 is
   begin
        return To_Integer_8 (S.Data (Offset));
   end Get_Integer_8;


   procedure Set_Integer_16 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_16) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
      else
         S.Data (Offset .. Offset + 1) := To_Two_Bytes (Value);
      end if;
   end Set_Integer_16;

   function Get_Integer_16 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_16 is
      Value: Integer_16 := To_Integer_16 (S.Data (Offset .. Offset + 1));
   begin
      if S.Swap then
         return Swap_Integer_16 (Value);
      else
         return Value;
      end if;
   end Get_Integer_16;


   procedure Set_Integer_32 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_32) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));
      else
         S.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      end if;
   end Set_Integer_32;

   function Get_Integer_32 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_32 is
      Value: Integer_32 := To_Integer_32 (S.Data (Offset .. Offset + 3));
   begin
      if S.Swap then
         return Swap_Integer_32 (Value);
      else
         return Value;
      end if;
   end Get_Integer_32;


   procedure Set_Integer_64 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_64) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));
      else
         S.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      end if;
   end Set_Integer_64;

   function Get_Integer_64 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_64 is
      Value: Integer_64 := To_Integer_64 (S.Data (Offset .. Offset + 7));
   begin
      if S.Swap then
         return Swap_Integer_64 (Value);
      else
         return Value;
      end if;
   end Get_Integer_64;


   procedure Set_Float_32 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));
      else
         S.Data (Offset .. Offset + 3) := To_Four_Bytes (Value);
      end if;
   end Set_Float_32;

   function Get_Float_32 (S: Byte_Store'Class; Offset: Unsigned_32) return IEEE_Float_32 is
      Value: IEEE_Float_32 := To_Float_32 (S.Data (Offset .. Offset + 3));
   begin
      if S.Swap then
         return Swap_Float_32 (Value);
      else
         return Value;
      end if;
   end Get_Float_32;


   procedure Set_Float_64 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      if S.Swap then
         S.Data (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));
      else
         S.Data (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      end if;
   end Set_Float_64;

   function Get_Float_64 (S: Byte_Store'Class; Offset: Unsigned_32) return IEEE_Float_64 is
      Value: IEEE_Float_64 := To_Float_64 (S.Data (Offset .. Offset + 7));
   begin
      if S.Swap then
         return Swap_Float_64 (Value);
      else
         return Value;
      end if;
   end Get_Float_64;


   function Get_String (S: Byte_Store'Class; Offset: Unsigned_32; Length: Unsigned_32) return String is
      subtype Str_T is String (1 .. Integer (Length));
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Unsigned_32 (Length));
      function To_Str_T is new Ada.Unchecked_Conversion (Arr_T, Str_T);
   begin
      return To_Str_T (Arr_T (S.Data (Offset .. Offset + Length - 1)));
   end Get_String;


   procedure Set_String (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: String) is
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Value'Length);
      subtype Str_T is String (1 .. Value'Length);
      function To_Arr_T is new Ada.Unchecked_Conversion (Str_T, Arr_T);
   begin
      S.Data (Offset .. Offset + Value'Length - 1) := To_Arr_T (Value);
   end Set_String;


   function Get_Unsigned_8_Array (S: Byte_Store'Class; Offset: Unsigned_32; Length: Unsigned_32) return Array_Of_Unsigned_8 is
   begin
      return S.Data (Offset .. Offset + Length - 1);
   end Get_Unsigned_8_Array;


   procedure Set_Unsigned_8_Array (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Array_Of_Unsigned_8) is
   begin
      S.Data (Offset .. Offset + Value'Last) := Value;
   end Set_Unsigned_8_Array;


end BRBON.Container;

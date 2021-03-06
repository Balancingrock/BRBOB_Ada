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
--  And because I need to make a living:
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


package body Storage_Area is


   --function To_Array_Of_Unsigned_8_Ptr is new Ada.Unchecked_Conversion (String_Ptr, Array_Of_Unsigned_8_Ptr);


   function Allocate_And_Create (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr is
      S: Storage_Area_Ptr := new Storage_Area;
   begin
      S.all.Data := new Array_Of_Unsigned_8 (0 .. Byte_Count - 1);
      S.all.Swap := Using_Endianness = Machine_Endianness;
      return S;
   end Allocate_And_Create;

   procedure Finalize (S: in out Storage_Area) is
   begin
      Deallocate_Array_Of_Unsigned_8 (S.Data);
   end Finalize;


   -- Operational

   procedure Set_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Type) is
   begin
      S.Data.all (Offset) := BR_Item_Type'Pos (Value);
   end Set_Item_Type;


   procedure Set_Item_Options (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Options) is
   begin
      S.Data.all (Offset) := To_Unsigned_8 (Value);
   end Set_Item_Options;


   procedure Set_Item_Flags (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Flags) is
   begin
      S.Data.all (Offset) := To_Unsigned_8 (Value);
   end Set_Item_Flags;


   procedure Set_Bool (S: Storage_Area'Class; Offset: Unsigned_32; Value: Boolean) is
   begin
      S.Data (Offset) := (if Value then 1 else 0);
   end Set_Bool;


   procedure Set_Unsigned_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      S.Data.all (Offset) := Value;
   end Set_Unsigned_8;


   procedure Set_Unsigned_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      end if;
   end Set_Unsigned_16;


   procedure Set_Unsigned_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      end if;
   end Set_Unsigned_32;


   procedure Set_Unsigned_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      end if;
   end Set_Unsigned_64;


   procedure Set_Integer_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_8) is
   begin
      S.Data.all (Offset) := To_Unsigned_8 (Value);
   end Set_Integer_8;


   procedure Set_Integer_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_16) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
      end if;
   end Set_Integer_16;


   procedure Set_Integer_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_32) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));
      end if;
   end Set_Integer_32;


   procedure Set_Integer_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_64) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));
      end if;
   end Set_Integer_64;


   procedure Set_Float_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));
      end if;
   end Set_Float_32;


   procedure Set_Float_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));
      end if;
   end Set_Float_64;


   function Get_String (S: Storage_Area'Class; Offset: Unsigned_32; Length: Unsigned_32) return String is
      subtype Str_T is String (1 .. Integer (Length));
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Unsigned_32 (Length));
      function To_Str_T is new Ada.Unchecked_Conversion (Arr_T, Str_T);
   begin
      return To_Str_T (Arr_T (S.Data (Offset .. Offset + Length - 1)));
   end Get_String;


   procedure Set_String (S: Storage_Area'Class; Offset: Unsigned_32; Value: String) is
      subtype Arr_T is Array_Of_Unsigned_8 (1 .. Value'Length);
      subtype Str_T is String (1 .. Value'Length);
      function To_Arr_T is new Ada.Unchecked_Conversion (Str_T, Arr_T);
   begin
      S.Data (Offset .. Offset + Value'Length - 1) := To_Arr_T (Value);
   end Set_String;


   function Get_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Length: Unsigned_32) return Array_Of_Unsigned_8 is
   begin
      return S.Data (Offset .. Offset + Length - 1);
   end Get_Unsigned_8_Array;


   procedure Set_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Value: Array_Of_Unsigned_8) is
   begin
      S.Data.all (Offset .. Offset + Value'Last) := Value;
   end Set_Unsigned_8_Array;


end Storage_Area;

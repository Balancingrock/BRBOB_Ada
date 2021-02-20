with GNAT.Byte_Swapping;
with Ada.Unchecked_Conversion;


package body Storage_Area is


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


   procedure Set_Unsigned_8 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      S.Data.all (Offset) := Value;
   end Set_Unsigned_8;

   function Get_Unsigned_8 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_8 is
   begin
      return S.Data.all (Offset);
   end Get_Unsigned_8;

   procedure Set_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      end if;
   end Set_Unsigned_16;

   function Get_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_16 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Unsigned_16 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Unsigned_16 (To_Unsigned_16 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Unsigned_16;

   procedure Set_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      end if;
   end Set_Unsigned_32;

   function Get_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_32 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Unsigned_32 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Unsigned_32 (To_Unsigned_32 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Unsigned_32;

   procedure Set_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      end if;
   end Set_Unsigned_64;

   function Get_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_64 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Unsigned_64 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Unsigned_64 (To_Unsigned_64 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Unsigned_64;

   procedure Set_Integer_8 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_8) is
   begin
      S.Data.all (Offset) := To_Unsigned_8 (Value);
   end Set_Integer_8;

   function Get_Integer_8 (S: Storage_Area; Offset: Unsigned_32) return Integer_8 is
   begin
      return To_Integer_8 (S.Data.all (Offset));
   end Get_Integer_8;

   procedure Set_Integer_16 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_16) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
      end if;
   end Set_Integer_16;

   function Get_Integer_16 (S: Storage_Area; Offset: Unsigned_32) return Integer_16 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Integer_16 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Integer_16 (To_Integer_16 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Integer_16;

   procedure Set_Integer_32 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));
      end if;
   end Set_Integer_32;

   function Get_Integer_32 (S: Storage_Area; Offset: Unsigned_32) return Integer_32 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Integer_32 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Integer_32 (To_Integer_32 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Integer_32;

   procedure Set_Integer_64 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));
      end if;
   end Set_Integer_64;

   function Get_Integer_64 (S: Storage_Area; Offset: Unsigned_32) return Integer_64 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Integer_64 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Integer_64 (To_Integer_64 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Integer_64;

   procedure Set_Float_32 (S: Storage_Area; Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));
      end if;
   end Set_Float_32;

   function Get_Float_32 (S: Storage_Area; Offset: Unsigned_32) return IEEE_Float_32 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Float_32 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Float_32 (To_Float_32 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Float_32;

   procedure Set_Float_64 (S: Storage_Area; Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));
      end if;
   end Set_Float_64;

   function Get_Float_64 (S: Storage_Area; Offset: Unsigned_32) return IEEE_Float_64 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Float_64 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Float_64 (To_Float_64 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Float_64;

   procedure Set_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: Array_Of_Unsigned_8) is
   begin
      S.Data.all (Offset .. Offset + Value'Last) := Value;
   end Set_Unsigned_8_Array;

   procedure Get_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: in out Array_Of_Unsigned_8) is
   begin
      Value := S.Data.all (Offset .. Offset + Value'Last);
   end Get_Unsigned_8_Array;

   --

   function New_Storage_Area (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr is
      S: Storage_Area_Ptr;
   begin
      S := new Storage_Area;
      S.Data := new Array_Of_Unsigned_8 (1 .. Byte_COunt);
      S.Uses_Endianness := Using_Endianness;
      return S;
   end New_Storage_Area;


end Storage_Area;

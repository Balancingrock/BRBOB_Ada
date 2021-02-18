with GNAT.Byte_Swapping;

with BRBON_Configure; use BRBON_Configure;
with Pointer_Math; use Pointer_Math;


package body Pointer_Basics is


   -- ========================
   -- Supporting byte swapping
   -- ========================

   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);
   function Swap_Unsigned_64 is new GNAT.Byte_Swapping.Swapped8 (Unsigned_64);
   function Swap_Integer_16 is new GNAT.Byte_Swapping.Swapped2 (Integer_16);
   function Swap_Integer_32 is new GNAT.Byte_Swapping.Swapped4 (Integer_32);
   function Swap_Integer_64 is new GNAT.Byte_Swapping.Swapped8 (Integer_64);
   function Swap_Float_32 is new GNAT.Byte_Swapping.Swapped4 (Float_32);
   function Swap_Float_64 is new GNAT.Byte_Swapping.Swapped8 (Float_64);

   --

   procedure Set_Unsigned_8_At_Ptr_With_Offset(Ptr: Unsigned_8_Ptr; Value: Unsigned_8) is
      Pointer: Unsigned_8_Ptr := Ptr + Offset;
   begin
      Pointer.all := Value;
   end Set_Unsigned_8_At_Ptr_With_Offset;

   procedure Set_Unsigned_16_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_16; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Unsigned_16_Ptr (Ptr + Offset).all := Value;
      else
         To_Unsigned_16_Ptr (Ptr + Offset).all := Swap_Unsigned_16 (Value);
      end if;
   end Set_Unsigned_16_At_Ptr_With_Offset;

   procedure Set_Unsigned_32_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_32; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Unsigned_32_Ptr (Ptr + Offset).all := Value;
      else
         To_Unsigned_32_Ptr (Ptr + Offset).all := Swap_Unsigned_32 (Value);
      end if;
   end Set_Unsigned_32_At_Ptr_With_Offset;

   procedure Set_Unsigned_64_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_64; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Unsigned_64_Ptr (Ptr + Offset).all := Value;
      else
         To_Unsigned_64_Ptr (Ptr + Offset).all := Swap_Unsigned_64 (Value);
      end if;
   end Set_Unsigned_64_At_Ptr_With_Offset;

   procedure Set_Integer_8_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_8) is
   begin
      To_Integer_8_Ptr (Ptr + Offset).all := Value;
   end Set_Integer_8_At_Ptr_With_Offset;

   procedure Set_Integer_16_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_16; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Integer_16_Ptr (Ptr + Offset).all := Value;
      else
         To_Integer_16_Ptr (Ptr + Offset).all := Swap_Integer_16 (Value);
      end if;
   end Set_Integer_16_At_Ptr_With_Offset;

   procedure Set_Integer_32_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_32; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Integer_32_Ptr (Ptr + Offset).all := Value;
      else
         To_Integer_32_Ptr (Ptr + Offset).all := Swap_Integer_32 (Value);
      end if;
   end Set_Integer_32_At_Ptr_With_Offset;

   procedure Set_Integer_64_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_64; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Integer_64_Ptr (Ptr + Offset).all := Value;
      else
         To_Integer_64_Ptr (Ptr + Offset).all := Swap_Integer_64 (Value);
      end if;
   end Set_Integer_64_At_Ptr_With_Offset;

   procedure Set_Float_32_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Float_32; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Float_32_Ptr (Ptr + Offset).all := Value;
      else
         To_Float_32_Ptr (Ptr + Offset).all := Swap_Float_32 (Value);
      end if;
   end Set_Float_32_At_Ptr_With_Offset;

   procedure Set_Float_64_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Float_64; Using_Endianness: Endianness) is
   begin
      if Machine_Endianness = Using_Endianness then
         To_Float_64_Ptr (Ptr + Offset).all := Value;
      else
         To_Float_64_Ptr (Ptr + Offset).all := Swap_Float_64 (Value);
      end if;
   end Set_Float_64_At_Ptr_With_Offset;

   function Get_Unsigned_8_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr) return Unsigned_8 is
      Pointer: Unsigned_8_Ptr := Ptr + Offset;
   begin
      return Pointer.all;
   end Get_Unsigned_8_From_Ptr_With_Offset;

   function Get_Unsigned_16_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Unsigned_16 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Unsigned_16_Ptr (Ptr + Offset).all;
      else
         return Swap_Unsigned_16 (To_Unsigned_16_Ptr (Ptr + Offset).all);
      end if;
   end Get_Unsigned_16_From_Ptr_With_Offset;

   function Get_Unsigned_32_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Unsigned_32 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Unsigned_32_Ptr (Ptr + Offset).all;
      else
         return Swap_Unsigned_32 (To_Unsigned_32_Ptr (Ptr + Offset).all);
      end if;
   end Get_Unsigned_32_From_Ptr_With_Offset;

   function Get_Unsigned_64_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Unsigned_64 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Unsigned_64_Ptr (Ptr + Offset).all;
      else
         return Swap_Unsigned_64 (To_Unsigned_64_Ptr (Ptr + Offset).all);
      end if;
   end Get_Unsigned_64_From_Ptr_With_Offset;

   function Get_Integer_8_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr) return Integer_8 is
   begin
      return To_Integer_8_Ptr (Ptr + Offset).all;
   end Get_Integer_8_From_Ptr_With_Offset;

   function Get_Integer_16_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Integer_16 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Integer_16_Ptr (Ptr + Offset).all;
      else
         return Swap_Integer_16 (To_Integer_16_Ptr (Ptr + Offset).all);
      end if;
   end Get_Integer_16_From_Ptr_With_Offset;

   function Get_Integer_32_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Integer_32 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Integer_32_Ptr (Ptr + Offset).all;
      else
         return Swap_Integer_32 (To_Integer_32_Ptr (Ptr + Offset).all);
      end if;
   end Get_Integer_32_From_Ptr_With_Offset;

   function Get_Integer_64_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Integer_64 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Integer_64_Ptr (Ptr + Offset).all;
      else
         return Swap_Integer_64 (To_Integer_64_Ptr (Ptr + Offset).all);
      end if;
   end Get_Integer_64_From_Ptr_With_Offset;

   function Get_Float_32_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Float_32 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Float_32_Ptr (Ptr + Offset).all;
      else
         return Swap_Float_32 (To_Float_32_Ptr (Ptr + Offset).all);
      end if;
   end Get_Float_32_From_Ptr_With_Offset;

   function Get_Float_64_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Float_64 is
   begin
      if Machine_Endianness = Using_Endianness then
         return To_Float_64_Ptr (Ptr + Offset).all;
      else
         return Swap_Float_64 (To_Float_64_Ptr (Ptr + Offset).all);
      end if;
   end Get_Float_64_From_Ptr_With_Offset;


end Pointer_Basics;

with Interfaces; use Interfaces;

with BRBON; use BRBON;
with BRBON_Basic_Types; use BRBON_Basic_Types;


package Pointer_Basics is

   generic
      Offset: Unsigned_32;
   procedure Set_Unsigned_8_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_8);

   generic
      Offset: Unsigned_32;
   procedure Set_Unsigned_16_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_16; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Unsigned_32_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_32; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Unsigned_64_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Unsigned_64; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Integer_8_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_8);

   generic
      Offset: Unsigned_32;
   procedure Set_Integer_16_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_16; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Integer_32_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_32; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Integer_64_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Integer_64; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Float_32_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Float_32; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   procedure Set_Float_64_At_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Value: Float_64; Using_Endianness: Endianness);

   generic
      Offset: Unsigned_32;
   function Get_Unsigned_8_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr) return Unsigned_8;

   generic
      Offset: Unsigned_32;
   function Get_Unsigned_16_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Unsigned_16;

   generic
      Offset: Unsigned_32;
   function Get_Unsigned_32_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Unsigned_32;

   generic
      Offset: Unsigned_32;
   function Get_Unsigned_64_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Unsigned_64;

   generic
      Offset: Unsigned_32;
   function Get_Integer_8_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr) return Integer_8;

   generic
      Offset: Unsigned_32;
   function Get_Integer_16_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Integer_16;

   generic
      Offset: Unsigned_32;
   function Get_Integer_32_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Integer_32;

   generic
      Offset: Unsigned_32;
   function Get_Integer_64_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Integer_64;

   generic
      Offset: Unsigned_32;
   function Get_Float_32_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Float_32;

   generic
      Offset: Unsigned_32;
   function Get_Float_64_From_Ptr_With_Offset (Ptr: Unsigned_8_Ptr; Using_Endianness: Endianness) return Float_64;

end Pointer_Basics;

with Interfaces; use Interfaces;
with BRBON; use BRBON;


-- Contains methods to convert types into a stream of bytes.
--
package Decomposable_Types is


   -- A decomposable should supply bytes and return True as long as a valid byte is supplied.
   -- When no valid byte can be supplied it should return False.
   -- Once False is returned it will not be called again.
   --
   type Decomposable is abstract tagged
      record
         Remainder: Unsigned_32;
      end record;

   function Next (Source: in out Decomposable; Byte: out Unsigned_8) return Boolean is abstract;


   -- String
   --
   type String_Ptr is access all String;
   --
   type Decomposable_String is new Decomposable with
      record
         Ptr: String_Ptr;
      end record;

   overriding
   function Next (Source: in out Decomposable_String; Byte: out Unsigned_8) return Boolean;


   -- Array of Unsigned_8
   --
   type Decomposable_Array_Of_Unsigned_8 is new Decomposable with
      record
         Ptr: Array_Of_Unsigned_8_Ptr;
      end record;

   overriding
   function Next (Source: in out Decomposable_Array_Of_Unsigned_8; Byte: out Unsigned_8) return Boolean;


   -- Boolean
   --
   type Decomposable_Boolean is new Decomposable with
      record
         Value: Boolean;
      end record;
   --
   overriding
   function Next (Source: in out Decomposable_Boolean; Byte: out Unsigned_8) return Boolean;
   --
   function New_Decomposable_Boolean (Value: Boolean) return Decomposable_Boolean is ((1, Value));


   -- Integer_8
   --
   type Decomposable_Integer_8 is new Decomposable with
      record
         Value: Integer_8;
      end record;
   --
   overriding
   function Next (Source: in out Decomposable_Integer_8; Byte: out Unsigned_8) return Boolean;
   --
   function New_Decomposable_Integer_8 (Value: Integer_8) return Decomposable_Integer_8 is ((1, Value));


   -- Unsigned_8
   --
   type Decomposable_Unsigned_8 is new Decomposable with
      record
         Value: Unsigned_8;
      end record;
   --
   overriding
   function Next (Source: in out Decomposable_Unsigned_8; Byte: out Unsigned_8) return Boolean;
   --
   function New_Decomposable_Unsigned_8 (Value: Unsigned_8) return Decomposable_Unsigned_8 is ((1, Value));


   -- Integer_16
   --
   type Decomposable_Integer_16 is new Decomposable with
      record
         Value: Two_Bytes;
      end record;
   --
   overriding
   function Next (Src: in out Decomposable_Integer_16; Byte: out Unsigned_8) return Boolean;
   --
   function New_Decomposable_Integer_16 (Value: Integer_16; Endianness: BRBON.Endianness) return Decomposable_Integer_16;


   -- Unsigned_16
   --
   type Decomposable_Unsigned_16 is new Decomposable with
      record
         Value: Two_Bytes;
      end record;
   --
   overriding
   function Next (Src: in out Decomposable_Unsigned_16; Byte: out Unsigned_8) return Boolean;
   --
   function New_Decomposable_Unsigned_16 (Value: Unsigned_16; Endianness: BRBON.Endianness) return Decomposable_Unsigned_16;


   -- Integer_32
   --
   type Decomposable_Integer_32 is new Decomposable with
      record
         Value: Four_Bytes;
      end record;
   --
   overriding
   function Next (Src: in out Decomposable_Integer_32; Byte: out Unsigned_8) return Boolean;
   --
   function New_Decomposable_Integer_32 (Value: Integer_32; Endianness: BRBON.Endianness) return Decomposable_Integer_32;


end Decomposable_Types;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with BRBON; use BRBON;
with Decomposable_Types; use Decomposable_Types;

package BRBON_Support is


   -- Returns the CRC-16 over the given string.
   --
   function Crc_16 (Ptr: String_Ptr) return Unsigned_16;


   -- Returns the CRC-32 over the given string.
   --
   function Crc_32 (Ptr: String_Ptr) return Unsigned_32;


   -- Returns the CRC-16 starting at the pointer for the given length.
   --
   function Crc_16 (Ptr: Array_Of_Unsigned_8_Ptr) return Unsigned_16;


   -- Returns the CRC-32 starting at the pointer for the given length.
   --
   function Crc_32 (Ptr: Array_Of_Unsigned_8_Ptr) return Unsigned_32;


private


   -- Calculates a CRC-16 value over a series of bytes.
   -- @value Source A source that supplies the bytes over which the CRC must be calculated.
   -- @value Initialization The initial value should be 0 (default) for a ARC CRC or 16#FFFF# for a MODBUS CRC.
   -- @value Polynomial The default polynomal is 16#A001# which is the reverse of 16#8005#.
   --
   function Crc_16 (Source: in out Decomposable'Class; Initalization: Unsigned_16 := 0; Polynomial: Unsigned_16 := 16#A001#) return Unsigned_16;


   -- Calculates a CRC-32 value over a series of bytes.
   -- @value Provider A source that supplies the bytes over which the CRC must be calculated.
   -- @value Initialization The initial value usually set to 0 (default) or 16#FFFF_FFFF#. Setting to 0 will skip leading zero's in the data provider.
   --
   function Crc_32 (Source: in out Decomposable'Class; Initalization: Unsigned_32 := 16#FFFF_FFFF#) return Unsigned_32;


end BRBON_Support;

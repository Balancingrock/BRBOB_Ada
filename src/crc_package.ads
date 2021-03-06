with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with Serializable_Types; use Serializable_Types;

package CRC_Package is


   subtype CRC_16 is Unsigned_16;

   subtype CRC_32 is Unsigned_32;


   -- Returns the CRC-16 over the given string.
   --
   function Calculate_CRC_16 (Str: String) return CRC_16;


   -- Returns the CRC-32 over the given string.
   --
   function Calculate_CRC_32 (Str: String) return CRC_32;


   -- Returns the CRC-16 starting at the pointer for the given length.
   --
   function Calculate_CRC_16 (Arr: Array_Of_Unsigned_8) return CRC_16;


   -- Returns the CRC-32 starting at the pointer for the given length.
   --
   function Calculate_CRC_32 (Arr: Array_Of_Unsigned_8) return CRC_32;


private


   -- Calculates a CRC-16 value over a series of bytes.
   -- @value Source A source that supplies the bytes over which the CRC must be calculated.
   -- @value Initialization The initial value should be 0 (default) for a ARC CRC or 16#FFFF# for a MODBUS CRC.
   -- @value Polynomial The default polynomal is 16#A001# which is the reverse of 16#8005#.
   --
   function Calculate_CRC_16 (Source: in out Serializable; Initalization: Unsigned_16 := 0; Polynomial: Unsigned_16 := 16#A001#) return CRC_16;


   -- Calculates a CRC-32 value over a series of bytes.
   -- @value Provider A source that supplies the bytes over which the CRC must be calculated.
   -- @value Initialization The initial value usually set to 0 (default) or 16#FFFF_FFFF#. Setting to 0 will skip leading zero's in the data provider.
   --
   function Calculate_CRC_32 (Source: in out Serializable; Initalization: Unsigned_32 := 16#FFFF_FFFF#) return CRC_32;


end CRC_Package;

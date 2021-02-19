with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BRBON_Basic_Types; use BRBON_Basic_Types;


package BRBON_Support is


   -- Returns the CRC-16 over the given string.
   --
   function Crc_16 (Str: String) return Unsigned_16;


   -- Returns the CRC-32 over the given string.
   --
   function Crc_32 (Str: String) return Unsigned_32;


   -- Returns the CRC-16 starting at the pointer for the given length.
   --
   function Crc_16 (Ptr: Unsigned_8_Ptr; Length: Unsigned_32) return Unsigned_16;


   -- Returns the CRC-32 starting at the pointer for the given length.
   --
   function Crc_32 (Ptr: Unsigned_8_Ptr; Length: Unsigned_32) return Unsigned_32;


private

   -- A data source should supply bytes and return True as long as a valid byte is supplied.
   -- When no valid byte can be supplied it should return False. Once False is returned it will not be called again.
   --
   type Crc_Data is abstract tagged
      record
         Remainder: Unsigned_32;
      end record;

   function Next (Source: in out Crc_Data; Byte: out Unsigned_8) return Boolean is abstract;


   -- Calculates a CRC-16 value over a series of bytes.
   -- @value Source A pointer to a function that supplies the bytes over which the CRC must be calculated.
   -- @value Initialization The initial value should be 0 (default) for a ARC CRC or 16#FFFF# for a MODBUS CRC.
   -- @value Polynomial The default polynomal is 16#A001# which is the reverse of 16#8005#.
   --
   function Crc_16 (Source: in out Crc_Data'Class; Initalization: Unsigned_16 := 0; Polynomial: Unsigned_16 := 16#A001#) return Unsigned_16;


   -- Calculates a CRC-32 value over a series of bytes.
   -- @value Provider A pointer to a function that supplies the bytes over which the CRC must be calculated.
   -- @value Initialization The initial value usually set to 0 (default) or 16#FFFF_FFFF#. Setting to 0 will skip leading zero's in the data provider.
   --
   function Crc_32 (Source: in out Crc_Data'Class; Initalization: Unsigned_32 := 16#FFFF_FFFF#) return Unsigned_32;


   --

   type Crc_Unbounded_String is new Crc_Data with
      record
         Str: Unbounded_String;
      end record;

   overriding
   function Next (Source: in out Crc_Unbounded_String; Byte: out Unsigned_8) return Boolean;

   --

   type Crc_Unsigned_8_Ptr is new Crc_Data with
      record
         Ptr: Unsigned_8_Ptr;
         Length: Unsigned_32;
      end record;

   overriding
   function Next (Source: in out Crc_Unsigned_8_Ptr; Byte: out Unsigned_8) return Boolean;


end BRBON_Support;

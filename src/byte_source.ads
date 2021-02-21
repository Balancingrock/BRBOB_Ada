with Interfaces; use Interfaces;
with BRBON; use BRBON;


package Byte_Source is


   -- A byte source should supply bytes and return True as long as a valid byte is supplied.
   -- When no valid byte can be supplied it should return False. Once False is returned it will not be called again.
   --
   type Byte_Source is abstract tagged
      record
         Remainder: Unsigned_32;
      end record;

   function Next (Source: in out Byte_Source; Byte: out Unsigned_8) return Boolean is abstract;


   -- For the string byte source
   --
   type String_Ptr is access all String;


   -- A data source for String CRC's
   --
   type String_Source is new Byte_Source with
      record
         Ptr: String_Ptr;
      end record;

   overriding
   function Next (Source: in out String_Source; Byte: out Unsigned_8) return Boolean;


   -- A data source for binary CRC's
   --
   type Array_Of_Unsigned_8_Source is new Byte_Source with
      record
         Ptr: Array_Of_Unsigned_8_Source_Ptr;
      end record;

   overriding
   function Next (Source: in out Array_Of_Unsigned_8_Source; Byte: out Unsigned_8) return Boolean;

end Byte_Source;

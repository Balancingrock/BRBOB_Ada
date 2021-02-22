with Interfaces; use Interfaces;
with BRBON; use BRBON;


-- Contains methods to convert types into a stream of bytes.
--
package Serializable_Types is


   -- A serializable returns bytes and True as long as a valid byte is supplied.
   -- When no valid byte can be supplied it returns False.
   -- Once False is returned it will not be called again.
   --
   type Serializable is private;

   function Next (Source: in out Serializable; Byte: out Unsigned_8) return Boolean;


   function New_Serializable (Ptr: String_Ptr) return Serializable;
   function New_Serializable (Ptr: Array_Of_Unsigned_8_Ptr) return Serializable;


private


   type Serializable is
      record
         Bytes: Array_Of_Unsigned_8_Ptr;
         Remaining: Unsigned_32;
      end record;


end Serializable_Types;

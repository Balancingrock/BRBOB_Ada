with BRBON.Types; use BRBON.Types;


package UUID_Package is

   -- This implementation only support version4 variant 1 UUIDs.
   -- Note: This exception can only be raised in one of the factory methods that re-creates an existing UUID.
   --
   UUID_Version_Or_Variant_Not_Supported: exception;

   -- Raised when an illegal charcter occurs in a UUID-String.
   --
   Illegal_Character: exception;

   type UUID is private;

   subtype UUID_Bytes is Array_Of_Unsigned_8 (1 .. 16);

   subtype UUID_String is String (1 .. 36);

   function Factory return UUID;

   function Factory (Bytes: UUID_Bytes) return UUID;

   function Factory (Str: UUID_String) return UUID;

   function Nil_UUID return UUID;

   function Get_Bytes (I: UUID) return UUID_Bytes;

   function Get_String (I: UUID) return UUID_String;

   function "=" (lhs, rhs: UUID_Package.UUID) return Boolean;

private

   type UUID is
      record
         Bytes: Array_Of_Unsigned_8 (1 .. 16);
      end record;

   Nil_Instance: constant UUID := (Bytes => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));

end UUID_Package;


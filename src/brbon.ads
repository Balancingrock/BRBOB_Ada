
-- Top level package to provide a starting point for the implementation.
--
package BRBON is

   -- Possible exceptions
   -- ===================

   -- Raised when an operation is attempted on the wrong type
   --
   Type_Conflict: exception;


   -- Raised when the storage area runs out of space.
   --
   Storage_Error: exception;

   -- Raised when a BRBON structure contains an illegal type pattern.
   --
   Illegal_Item_Type: exception;

   -- Raised when a string to Item_Name conversion failed.
   --
   Item_Name_Too_Long: exception;

   -- Raised when an attempt is made to execute an incompletely coded routine
   --
   Incomplete_Code: exception;

   -- This exception is raised when a bit pattern in the raw data could not be mapped to a corresponding enum.
   --
   Enum_Mapping_Failed: exception;

end BRBON;

with BRBON_Basic_Types; use BRBON_Basic_Types;

package BRBON_Configure is

   -- Change this to the endianness of the processor.
   --
   Machine_Endianness: constant Endianness := Little;

   -- When set to True, any new storage space allocated will be set to 0.
   -- Mostly usefull for testing, to ensure an expected content.
   --
   Zero_New_Storage: constant Boolean := False;

end BRBON_Configure;

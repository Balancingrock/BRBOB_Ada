with BRBON.Types; use BRBON.Types;


package BRBON.Configure is


   -- Change this to the endianness of the processor.
   --
   Machine_Endianness: constant Endianness := Little;


   -- When set to True, any new storage space allocated will be set to 0.
   -- Mostly usefull for testing, to ensure an expected content.
   --
   Zero_New_Storage: constant Boolean := True;


end BRBON.Configure;

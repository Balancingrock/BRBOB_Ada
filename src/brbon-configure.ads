with BRBON.Types; use BRBON.Types;


package BRBON.Configure is


   -- Change this to the endianness of the processor.
   --
   Machine_Endianness: constant Endianness := Little;


   -- When set to True, any storage space used for the first time will be set to 0.
   -- Mostly usefull for testing, to ensure an expected content. Set to false for
   -- best performance.
   --
   Zero_Storage: constant Boolean := True;


end BRBON.Configure;

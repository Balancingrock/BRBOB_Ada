with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;


package BRBON.Block is


   -- The types of blocks available
   --
   type Block_Type is
     (
      Illegal,
      Single_Item_File
     );
   for Block_Type'Size use 16;
   for Block_Type use
     (
      Illegal => 0,
      Single_Item_File => 1
     );

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Block_Type, Unsigned_16);
   function To_Block_Type is new Ada.Unchecked_Conversion (Unsigned_16, Block_Type);

   Minimum_Block_Byte_Count: array (Block_Type'Range) of Unsigned_32 :=
     (
      Illegal => 0,
      Single_Item_File => 8 + 8 + 8 + 8
     );



end BRBON.Block;

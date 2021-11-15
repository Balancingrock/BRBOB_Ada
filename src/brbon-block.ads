with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;

with BRBON.Types; use BRBON.Types;


package BRBON.Block is


   -- The types of blocks available
   --
   type Instance_Type is
     (
      Illegal,
      Single_Item_File
     );
   for Instance_Type'Size use 16;
   for Instance_Type use
     (
      Illegal => 0,
      Single_Item_File => 1
     );

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Instance_Type, Unsigned_16);
   function To_Block_Instance_Type is new Ada.Unchecked_Conversion (Unsigned_16, Instance_Type);


   -- The options for a Block
   --
   type Options is (No_Options, Reacquisition_Possible);
   for Options'Size use 16;
   for Options use
      (
         No_Options             => 0,
         Reacquisition_Possible => 16#01#
      );
   
end BRBON.Block;

with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;


package BRBON.Footer is

   Footer_Byte_Count: Array (Block_Type) of Unsigned_32 :=
     (
      Illegal => 0,
      Single_Item_File => 8
     );

end BRBON.Footer;

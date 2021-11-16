with Interfaces; use Interfaces;

with BRBON.Block;

package BRBON.Block.Footer is

   Footer_Byte_Count: Array (BRBON.Block.Instance_Type) of Unsigned_32 :=
     (
      Illegal => 0,
      Single_Item_File => 8
     );

end BRBON.Block.Footer;

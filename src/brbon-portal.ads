with Interfaces; use Interfaces;

with BRBON.Container;


package BRBON.Portal is

   type Instance is
      record
         Container: BRBON.Container.Instance;
         Item_Offset: Unsigned_32;
      end record;

   function Factory (For_Container: Container.Instance; Using_Item_Offset: Unsigned_32) return Instance;

end BRBON.Portal;

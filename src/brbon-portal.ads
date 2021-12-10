with Interfaces; use Interfaces;

with BRBON.Container;


package BRBON.Portal is

   type Instance is private;

   function Factory (For_Container_Ptr: Container.Instance_Ptr; Using_Item_Offset: Unsigned_32) return Instance;

private

   type Instance is
      record
         Container: Container.Instance_Ptr;
         Item_Offset: Unsigned_32;
      end record;

end BRBON.Portal;

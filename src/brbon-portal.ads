with Interfaces; use Interfaces;

with BRBON.Container;
with BRBON.Types; use BRBON.Types;


package BRBON.Portal is


   type Instance is
      record
         Container: BRBON.Container.Instance;
         Item_Offset: Unsigned_32;
      end record;

   function Factory (For_Container: Container.Instance; Using_Item_Offset: Unsigned_32) return Instance;


   -- ==========================================================================
   -- Note: Only offset functions are provided here since they are universal for
   -- all kind of blocks.
   -- ==========================================================================

   function Small_Value_Offset (P: Instance) return Unsigned_32;
   pragma Inline (Small_Value_Offset);

   function Value_Offset (P: Instance) return Unsigned_32;
   pragma Inline (Value_Offset);

end BRBON.Portal;

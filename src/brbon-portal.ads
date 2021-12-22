with Interfaces; use Interfaces;

with BRBON.Container;
with BRBON.Types; use BRBON.Types;


package BRBON.Portal is

   type Instance_Type is (Normal, Element, Field);

   type Instance is
      record
         CPtr: Container.Instance_Ptr;
         Item_Offset: Unsigned_32;
         Of_Type: Instance_Type := Normal;
         Element_Index: Unsigned_32 := 0;
         Column_Index: Unsigned_32 := 0;
      end record;

   function Factory
     (
      CPtr: Container.Instance_Ptr;
      Using_Item_Offset: Unsigned_32;
      Of_Type: Instance_Type := Normal;
      Element_Index: Unsigned_32 := 0;
      Column_Index: Unsigned_32 := 0
     ) return Instance;

   function Null_Portal return Instance;

   function Is_Null_Portal (P: Instance) return Boolean;


   -- ==========================================================================
   -- Note: Only offset functions are provided here since they are universal for
   -- all kind of blocks.
   -- ==========================================================================

   function Small_Value_Offset (P: Instance) return Unsigned_32;
   pragma Inline (Small_Value_Offset);

   function Value_Offset (P: Instance) return Unsigned_32;
   pragma Inline (Value_Offset);

end BRBON.Portal;

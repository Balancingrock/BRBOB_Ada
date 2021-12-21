with BRBON.Item;

package body BRBON.Portal is


   function Factory (For_Container: Container.Instance; Using_Item_Offset: Unsigned_32) return Instance is
   begin
      return (For_Container, Using_Item_Offset, Normal, 0, 0);
   end Factory;


   function Small_Value_Offset (P: Instance) return Unsigned_32 is
   begin
      return P.Item_Offset + Item.Small_Value_Offset;
   end Small_Value_Offset;


   function Value_Offset (P: Instance) return Unsigned_32 is
   begin
      return Item.Value_Offset (P.Container, P.Item_Offset);
   end Value_Offset;


end BRBON.Portal;

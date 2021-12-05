with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

with BRBON.Types;
with BRBON.Container;


package BRBON.Item is

   procedure Create_Item (Of_Type: Types.Item_Type; In_Container: Container.Instance; At_Location: Unsigned_32);

end BRBON.Item;

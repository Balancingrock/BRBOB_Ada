package body BRBON.Portal is

   function Factory (For_Container: Container.Instance; Using_Item_Offset: Unsigned_32) return Instance is
   begin
      return (For_Container, Using_Item_Offset);
   end Factory;

end BRBON.Portal;

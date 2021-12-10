package body BRBON.Portal is

   function Factory (For_Container_Ptr: Container.Instance_Ptr; Using_Item_Offset: Unsigned_32) return Instance is
   begin
      return Instance (For_Container_Ptr, Using_Item_Offset);
   end Factory;

end BRBON.Portal;

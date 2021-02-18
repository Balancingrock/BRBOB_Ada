package body Portal_Manager is

   procedure Update_Portal_Pointers (Mgr: Portal_Manager; At_And_Above: Unsigned_8_Ptr; Below: Unsigned_8_Ptr; To_New_Base: Unsigned_8_Ptr) is
   begin
      raise BRBON.Incomplete_Code;
   end Update_Portal_Pointers;

   function New_Portal_Manager return Portal_Manager is
   begin
      return Portal_Manager'(Portals => null);
   end New_Portal_Manager;

end Portal_Manager;

package body Portal_Package is

   function Get_Item_Type (Ptr: Portal_Ptr) return BR_Item_Type is
   begin
      raise BRBON.Incomplete_Code;
      return BRBON.Types.BR_Null;
   end Get_Item_Type;


end Portal_Package;

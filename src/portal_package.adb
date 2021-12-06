with Ada.Exceptions; use Ada.Exceptions;


package body Portal_Package is

   function Get_Item_Type (Ptr: Portal_Ptr) return BRBON.Types.Item_Type is
   begin
      Raise_Exception (BRBON.Implementation'Identity, "Portal_Package.Get_Item_Type not yet implemented");
      return BRBON.Types.Null_Type;
   end Get_Item_Type;


end Portal_Package;

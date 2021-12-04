with Ada.Exceptions; use Ada.Exceptions;

package body Portal_Package is

   function Get_Item_Type (Ptr: Portal_Ptr) return BR_Item_Type is
   begin
      Raise_Exception (BRBON.Implementation'Identity, "Portal_Package.Get_Item_Type not yet implemented");
      return BRBON.Types.BR_Null;
   end Get_Item_Type;


end Portal_Package;

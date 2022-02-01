with Ada.Exceptions;


package body Portal_Package is


   Static_Null_Portal: Portal := (Null_Portal, false, null, 0, 0);


   -----------------------------------------------------------------------------

   function Is_Valid_Portal (P: Portal) return Boolean is
   begin
      return P.Is_Valid;
   end Is_Valid_Portal;


   -----------------------------------------------------------------------------

   function Is_Null_Portal (P: Portal) return Boolean is
   begin
      return P.Is_Type = Null_Portal;
   end Is_Null_Portal;


end Portal_Package;

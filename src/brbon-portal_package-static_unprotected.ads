with BRBON.Portal_Package; use BRBON.Portal_Package;


package BRBON.Portal_Package.Static_Unprotected is


   type Static_Unprotected_Portal is new Portal with null record;


   function Get_Bool (P: Static_Unprotected_Portal) return Boolean;


end BRBON.Portal_Package.Static_Unprotected;

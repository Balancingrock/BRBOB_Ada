with BRBON.Item_Package;


package body BRBON.Portal_Package is


   The_Null_Portal: Portal := (Null_Portal, false, nil, 0, 0);


   function Is_Valid_Portal (P: Portal) return Boolean is
   begin
      return P.Is_Valid;
   end Is_Valid_Portal;


   function Is_Null_Portal (P: Portal) return Boolean is
   begin
      return P.Is_Valid;
   end Is_Null_Portal;


end BRBON.Portal_Package;

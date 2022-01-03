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


   -----------------------------------------------------------------------------

   function Portal_Factory
     (
      Item_Ptr: Item_Header_Ptr;
      Element_Index: Unsigned_32 := Unsigned_32'Max;
      Column_Index: Unsigned_32 := Unsigned_32'Max;
     )
      return Portal is

      New_Portal: Portal := (Normal, true, Item_Ptr, 0, 0);

   begin

      if Element_Index < Unsigned_32'Max then

         New_Portal.Is_Type := Element;
         New_Portal.Element_Index := Element_Index;

         if Column_Index < Unsigned_32'Max then

            New_Portal.Is_Type := Field;
            New_Portal.Column_Index := Column_Index;

         else

            Ada.Exceptions.Raise_Exception (Invalid_Portal_Error'Identity, "Cannot create a field portal from only a column index");

         end if;

      else

         New_Portal.Element_Index := Element_Index;
         New_Portal.Column_Index := Column_Index;

      end if;

      return New_Portal;

   end Factory;
end BRBON.Portal_Package;

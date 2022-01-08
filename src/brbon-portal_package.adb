with Ada.Exceptions;

with BRBON.Item_Access;


package body BRBON.Portal_Package is


   The_Null_Portal: Portal := (null, Null_Portal, false, null, 0, 0);


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
      Store_Ptr: Store_Pointer;
      Item_Ptr: Item_Header_Ptr;
      Element_Index: Unsigned_32 := Unsigned_32'Last;
      Column_Index: Unsigned_32 := Unsigned_32'Last
     )
      return Portal is

      New_Portal: Portal := (Store_Ptr, Normal, true, Item_Ptr, 0, 0);

   begin

      if Element_Index < Unsigned_32'Last then

         New_Portal.Is_Type := Element;
         New_Portal.Element_Index := Element_Index;

         if Column_Index < Unsigned_32'Last then

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

   end Portal_Factory;

end BRBON.Portal_Package;

package body BRBON is


   function Factory
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


end BRBON;

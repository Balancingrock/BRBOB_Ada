with BRBON;


package body Item is

   procedure Item_Type (I: Item_Header; Value: BR_Item_Type) is
   begin
      raise BRBON.Incomplete_Code;
   end Item_Type;


   function Item_Type (I: Item_Header) return BR_Item_Type is
   begin
      raise BRBON.Incomplete_Code;
      return BR_Null;
   end Item_Type;

end Item;

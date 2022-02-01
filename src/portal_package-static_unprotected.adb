package body Portal_Package.Static_Unprotected is


   -----------------------------------------------------------------------------
   -- Local support operations
   -----------------------------------------------------------------------------

   function Get_Value_Offset (P: Portal) return Unsigned_32 is
   begin
      return Item_Header_Byte_Count + Unsigned_32 (P.Item_Ptr.Name_Field_Byte_Count_Field);
   end Get_Value_Offset;
   pragma Inline (Get_Value_Offset);


   -----------------------------------------------------------------------------
   -- API implementations
   -----------------------------------------------------------------------------


   function Get_Item_Type (P: Portal) return Item_Type is (P.Item_Ptr.Type_Field);


   function Get_Item_Options (P: Portal) return Item_Options is (P.Item_Ptr.Options_Field);


   function Get_Item_Byte_Count (P: Portal) return Unsigned_32 is (P.Item_Ptr.Byte_Count_Field);

   function Is_Element (P: Portal) return Boolean is (P.Is_Type = Element);


   function Is_Field (P: Portal) return Boolean is (P.Is_Type = Field);


   function Get_Item_Name (P: Portal) return Item_Name is
   begin
      return Item_Name_Bounded_String_Package.To_Bounded_String ("tre");
   end Get_Item_Name;


   function Get_Bool (P: Static_Unprotected_Portal) return Boolean is
   begin
      raise Implementation;
      return false;
   end Get_Bool;


end Portal_Package.Static_Unprotected;

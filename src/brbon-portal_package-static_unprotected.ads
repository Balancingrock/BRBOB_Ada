with BRBON.Portal_Package; use BRBON.Portal_Package;
with BRBON.Container_Package; use BRBON.Container_Package;


package BRBON.Portal_Package.Static_Unprotected is


   type Static_Unprotected_Portal is new Portal with null record;


   -- ==========================================================================
   -- Properties access
   -- ==========================================================================

   -- Return the type of item this portal refers to
   --
   function Get_Item_Type (P: Portal) return Item_Type;
   pragma Inline (Get_Item_Type);


   -- Returns true if the item is an element in an array.
   --
   function Is_Element (P: Portal) return Boolean;
   pragma Inline (Is_Element);


   -- Returns true if the item is a field in a table.
   --
   function Is_Field (P: Portal) return Boolean;
   pragma Inline (Is_Field);


   -- Return the options of the referenced item
   --
   -- Note: Options are (to be) defined by the BRBON standard.
   --
   function Get_Item_Options (P: Portal) return Item_Options;
   pragma Inline (Get_Item_Options);


   -- Return the byte count for the referenced item
   --
   function Get_Item_Byte_Count (P: Portal) return Unsigned_32;
   pragma Inline (Get_Item_Options);


   -- Return the name of the referenced item
   --
   -- Items without a name return an empty string.
   -- Array elements return the name of the array (if any).
   -- Table fields return the name of the table (if any).
   --
   function Get_Item_Name (P: Portal) return Item_Name;


   -- ==========================================================================
   -- Value access
   -- ==========================================================================

   function Get_Bool (P: Static_Unprotected_Portal) return Boolean;


end BRBON.Portal_Package.Static_Unprotected;

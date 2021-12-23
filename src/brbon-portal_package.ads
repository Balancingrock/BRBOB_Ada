with Interfaces; use Interfaces;

with BRBON.Container;
with BRBON.Types; use BRBON.Types;
with BRBON.Item;


package BRBON.Portal_Package is


   -- ==========================================================================
   -- Public interface
   -- ==========================================================================


   -- This excpetion is raised when an invalid portal is used.
   --
   Invalid_Portal_Error: exception;


   -- A Portal offers access to the values stored in items referenced by the portal
   --
   type Portal is private;


   -- Returns true if the portal is valid. Once invalid, a portal will never become valid again.
   -- Once a portal is invalid, any attempt to access values in the item will raise the excpetion Invalid_Portal_Error.
   -- Note: Only Dynamic functions can invalidate a portal.
   --
   function Is_Valid (P: Portal) return Boolean;


   -- The null portal may be returned if a (look-up) operation does not produce a result.
   -- A null-portal is always invalid and cannot be used to access an item (it will raise the Invalid_Portal_Error if tried).
   -- However any portal (including invalid portals) can be tested for beiing a null portal.
   --
   function Is_Null_Portal (P: Portal) return Boolean;


private

   type Portal_Type is (Null_Portal, Normal, Element, Field);

   type Portal is
      record
         Is_Type: Portal_Type;
         Is_Valid: Boolean := True;
         --
         Item_Ptr: Item.Item_Layout_Ptr;
         Element_Index: Unsigned_32 := 0;
         Column_Index: Unsigned_32 := 0;
      end record;

   function Factory
     (
      Item_Ptr: Item.Item_Layout_Ptr;
      Element_Index: Unsigned_32 := Unsigned_32'Max;
      Column_Index: Unsigned_32 := Unsigned_32'Max;
     ) return Portal;


end BRBON.Portal_Package;

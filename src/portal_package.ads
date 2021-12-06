with BRBON.Types;

package Portal_Package is

   -- A portal functions as an intermediate access mechanism to the actual value in storage.
   -- Since all access to the data in the storage area is pointer based, the pointers must be adjusted in response to
   -- insert/delete/modify operations. This is done by the portal manager.
   --
   type Portal is
      record
         Is_Valid: Boolean := False;
         --Item_Ptr: Unsigned_8_Ptr;
         Index: Integer := -1;
         Column: Integer := -1;
         Reference_Count: Integer := 0;
      end record;


   -- Most portals are allocated on the heap and will only be referenced through their pointer.
   --
   type Portal_Ptr is access Portal;


   -- Read the type of the item associated with this portal.
   --
   function Get_Item_Type (Ptr: Portal_Ptr) return BRBON.Types.Item_Type;

end Portal_Package;

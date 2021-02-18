with BRBON_Basic_Types; use BRBON_Basic_Types;


package Portal is


   -- A portal functions as an intermediate access mechanism to the actual value in storage.
   -- Since all access to the data in the storage area is pointer based, the pointers must be adjusted in response to
   -- insert/delete/modify operations. This is done by the portal manager.
   --
   type Portal is
      record
         Is_Valid: Boolean := False;
         Item_Offset: Unsigned_32;
         Index: Integer := -1;
         Column: Integer := -1;
         Reference_Count: Integer := 0;
      end record;


   -- Most portals are allocated on the heap and will only be referenced through their pointer.
   --
   type Portal_Ptr is access Portal;


end Portal;

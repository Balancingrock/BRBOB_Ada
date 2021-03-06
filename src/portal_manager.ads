with Ada.Containers.Doubly_Linked_Lists;

with BRBON.Types; use BRBON.Types;
with Portal_Package; use Portal_Package;


package Portal_Manager is

   -- For the list of portals managed by the portal manager.
   --
   package List_Of_Portals is new Ada.Containers.Doubly_Linked_Lists(Element_Type => Portal_Ptr);

   -- The list of portals maintained by a portal manager
   --
   type Portal_List is access List_Of_Portals.List;

   -- The portal manager is used to ensure portal integrity for all user created portals.
   -- Only the root portal in the Item Manager is not maneaged by the portal manager.
   --
   type Portal_Manager is record
      Portals: Portal_List;
   end record;

   -- Updates the pointers in all of the portals under control of the manager.
   --
   --procedure Update_Portal_Pointers (Mgr: Portal_Manager; At_And_Above: Unsigned_8_Ptr; Below: Unsigned_8_Ptr; To_New_Base: Unsigned_8_Ptr);


   -- Create a new portal
   --
   function New_Portal_Manager return Portal_Manager;

end Portal_Manager;

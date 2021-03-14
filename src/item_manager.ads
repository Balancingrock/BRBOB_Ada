with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with Item_Static_Unprotected; use Item_Static_Unprotected;
with BRBON.Configure; use BRBON.Configure;

with BRBON.Container; use BRBON.Container;
with Portal_Manager;


package Item_Manager is


   type Item_Manager;


   -- All Item_Managers are dynamically allocated.
   --
   type Item_Manager_Ptr is access Item_Manager;


   -- The item manager controls access to the storage area. All interactions between the client and Brbon are controlled
   -- by the item manager except for the Portals which can be used as a shortcut.
   --
   type Item_Manager (Byte_Count: Unsigned_32) is new Limited_Controlled with
      record
         Storage: aliased Byte_Store;
         Increments: Unsigned_32;
         --Portals: Portal_Manager.Portal_Manager;
      end record;


   -- Override the finalization
   --
   overriding
   procedure Finalize (Mgr: in out Item_Manager);


   -- Increases the storage area byte count. Note that this will cause the entire content of
   -- the area to be copied and hence there should be enough free heap to accomodate both the old
   -- and the new storage area.
   -- @value Mgr The Item Manager of which the storage area must be increased.
   -- @value Value The new byte count for the storage area.
   --
   procedure Increase_Storage_Byte_Count (Mgr: in out Item_Manager; Value: Unsigned_32);


   -- Returns the size of the unused area in the storage area of the item manager
   --
   function Unused_Storage (Mgr: in Item_Manager) return Unsigned_32;


   -- Creates a new item manager.
   -- The callee is responsible for deallocation.
   -- @value Use_Endianness The endianness of the data as it was created.
   -- @value Byte_Count The initial size of the storage area in bytes. Default = 10KByte.
   -- @value Root_Type The storage type at the top of the hierarchy.
   -- @value Element_Type The type of element when an array is created as the root item. Not used for other item types.
   --
   function Allocate_And_Create(
                                Use_Endianness: in Endianness := Machine_Endianness;
                                Byte_Count: in Unsigned_32 := 10 * 2**10;
                                Increment: in Unsigned_32 := 10 * 2**10;
                                Root_Type: in BR_Item_Type := Br_Dictionary;
                                Element_Type: in BR_Item_Type := BR_Bool
                               ) return Item_Manager_Ptr;

   procedure Deallocate is new Ada.Unchecked_Deallocation (Item_Manager, Item_Manager_Ptr);


end Item_Manager;

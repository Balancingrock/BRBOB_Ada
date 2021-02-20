with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with BRBON; use BRBON;
with BRBON_Configure; use BRBON_Configure;

with Storage_Area; use Storage_Area;
with Portal_Manager;


package Item_Manager is


   type Item_Manager;


   -- All Item_Managers are dynamically allocated.
   --
   type Item_Manager_Ptr is access Item_Manager;


   -- The item manager controls access to the storage area. All interactions between the client and Brbon are controlled
   -- by the item manager except for the Portals which can be used as a shortcut.
   --
   type Item_Manager is new Limited_Controlled with
      record
         Storage: Storage_Area.Storage_Area;
         Increments: Unsigned_32;
         Portals: Portal_Manager.Portal_Manager;
      end record;


   -- Override the initialization
   --
   procedure Initialize (Mgr: in out Item_Manager);


   -- Override the finalization
   --
   procedure Finalization (Mgr: in out Item_Manager);


   -- Increases the storage area byte count. Note that this will cause the entire content of
   -- the area to be copied and hence there should be enough free heap to accomodate both the old
   -- and the new storage area.
   -- @value Mgr The Item Manager of which the storage area must be increased.
   -- @value Value The new byte count for the storage area.
   --
   procedure Increase_Storage_Byte_Count (Mgr: in out Item_Manager; Value: Unsigned_32);


   -- Returns the size of the unused area in the storage area of the item manager
   --
   function Unused_Storage (Mgr: in  Item_Manager) return Unsigned_32;


   -- Creates a new item manager
   -- @value Use_Endianness The endianness of the data as it was created.
   -- @value Byte_Count The initial size of the storage area in bytes. Default = 10KByte.
   -- @value Root_Type The storage type at the top of the hierarchy.
   --
   function Item_Manager_Factory(
                                 Use_Endianness: in Endianness := Machine_Endianness;
                                 Byte_Count: in Unsigned_32 := 10 * 2**10;
                                 Increment: in Unsigned_32 := 10 * 2**10;
                                 Root_Type: in Item_Type := Br_Dictionary
                                ) return Item_Manager_Ptr;

end Item_Manager;

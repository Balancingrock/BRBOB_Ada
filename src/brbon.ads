with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Bounded_Vectors;
with Unchecked_Conversion;

with BRBON_Basic_Types; use BRBON_Basic_Types;

package BRBON is


   -- All types available for storage into an Item_Manager.
   --
   type Item_Type is (
                      Br_Illegal,      -- Used for error detection, cannot be used by the user.
                      Br_Null,         -- A null has no associated value, it simply exists.
                      Br_Bool,         -- Corresponding to Standard.Boolean.
                      Br_Int8,         -- An integer with a size of 8 bits (Byte, char).
                      Br_Int16,        -- An integer with a size of 16 bits.
                      Br_Int32,        -- An integer with a size of 32 bits.
                      Br_Int64,        -- An integer with a size of 64 bits.
                      Br_UInt8,        -- An integer with a size of 8 bits and range 0 .. 2**8-1.
                      Br_UInt16,       -- An integer with a size of 16 bits and range 0 .. 2**16-1.
                      Br_UInt32,       -- An integer with a size of 32 bits and range 0 .. 2**32-1.
                      Br_UInt64,       -- An integer with a size of 64 bits and range of 0 .. 2**64-1.
                      Br_Float32,      -- An IEEE 754 32 bit float. Accurate to about 6 decimals, range approx 1.1e-38 to 3.4e38.
                      Br_Float64,      -- An IEEE 754 64 bit float. Accurate to about 15 digits, range aprox 2.2e-308 to 1.7e+308.
                      Br_String,       -- Corresponds to Standard.String.
                      Br_Crc_String,   -- A string with an associated CRC-16 code for fast searches.
                      Br_Binary,       -- A series of bytes, corresponds to array (1..Count) of Br_UInt8.
                      Br_Crc_Binary,   -- A binary with associated CRC-16 code fro fast searches.
                      Br_Array,        -- An array of Brbon.Item_Types.
                      Br_Dictionary,   -- A dictionary associates a key (string) with a value (Brbon.Item_Type).
                      Br_Sequence,     -- A sequence of Brbon.Item_Type's.
                      Br_Table,        -- A 2 dimension array of Brbon.Item_Type's addressed by column (string or index) and row (index).
                      Br_Uuid,         -- A UUID, an array of 16 Br_UInt8 values returned as array or string.
                      Br_Rbga,         -- A RGBA (Red Green Blue Alpha) for color specifications.
                      Br_Font          -- A font specification (family name and font name).
                     );
   for Item_Type'Size use 8;
   for Item_Type use (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);


   -- Option associated with stored items (currently unused)
   --
   type Item_Options is new Bits_8;
   function To_Item_Options is new Unchecked_Conversion(Unsigned_8, Item_Options);
   function To_Unsigned_8 is new Unchecked_Conversion (Item_Options, Unsigned_8);


   -- ===================
   -- Item Name
   -- ===================

   subtype Item_Name_Index is Integer range 1..245;
   package Item_Name_Vector_245 is new Ada.Containers.Bounded_Vectors (Item_Name_Index, Unsigned_8);
   subtype Item_Name is Item_Name_Vector_245.Vector;

   No_Item_Name: constant Item_Name := Item_Name_Vector_245.To_Vector(0);


   -- All BRBON data is stored in and controlled by an Item Manager.
   --
   type Item_Manager is limited private;
   type Item_Manager_Ptr is access Item_Manager;


   -- Returns the size of the unused area in the storage area of the item manager
   --
   function Unused_Storage(Manager_Ptr: in Item_Manager_Ptr) return Unsigned_32;


   -- The endianness to be used to store data.
   --
   type Endianness is (Big, Little);


   -- Returns the endianness used by the current machine
   --
   Machine_Endianness: Endianness := Little;


   -- Creates a new item manager
   -- @value Endianness The endianness of the data as it was created.
   -- @value Byte_Count The initial size of the storage area in bytes.
   -- @value Root_Type The storage type at the top of the hierarchy.
   --
   function Item_Manager_Factory(
                                 Use_Endianness: in Brbon.Endianness := Machine_Endianness;
                                 Byte_Count: in Unsigned_32 := 10 * 2**10;
                                 Root_Type: in Item_Type := Br_Dictionary
                                ) return Item_Manager_Ptr;


   -- Portals can be used to shortcut access to the value of items in the storage area.
   -- Use Portals when speed is essential, but beware that using portals will slow down making item size changes in the storage area.
   -- (Because of the need to update the value pointers in all the portals)
   --
   type Portal is limited private;


   -- All portals except 1 are allocated on the heap and will only be referenced through their pointer. The only exception is the root portal into the storage area of an item manager.
   --
   type Portal_Ptr is access Portal;


   -- Stores or updates the given value at the specified path.
   --
   procedure Store(Value: in Integer_8; At_Path: String);
   procedure Store(Value: in Integer_16; At_Path: String);


   -- Possible exceptions
   -- ===================

   -- Raised when the storage area runs out of space.
   --
   Brbon_Storage_Error: exception;
   Illegal_Item_Type: exception;

private




   -- ====================
   -- Portal Manager
   -- ====================

   -- For the list of portals managed by the portal manager.
   --
   package List_Of_Portals is new Ada.Containers.Doubly_Linked_Lists(Element_Type => Portal_Ptr);
   type Portal_List_Ptr is access List_Of_Portals.List;



   -- The portal manager is used to ensure portal integrity for all user created portals.
   -- Only the root portal in the Item Manager is not maneaged by the portal manager.
   --
   type Portal_Manager is record
      Portal_List: Portal_List_Ptr;
   end record;


   -- ===================
   -- Item Manager
   -- ===================

   type Storage_Area is array (Unsigned_32 range <>) of aliased Unsigned_8;
   for Storage_Area'Alignment use 32;

   type Storage_Area_Ptr is access Storage_Area;


   function "=" (Left: Item_Manager; Right: Item_Manager) return Boolean is (False);


   -- The item manager controls access to the storage area. All interactions between the client and Brbon are controlled
   -- by the item manager except for the Portals which can be used as a shortcut.
   --
   type Item_Manager is record
      Endianness: Brbon.Endianness;
      Storage_Increments: Unsigned_32;
      Storage_Ptr: Storage_Area_Ptr;
      Root_Ptr: Portal_Ptr;
      Portal_Mgr: Portal_Manager;
   end record;


   -- ===================
   -- Portals
   -- ===================

   -- A portal functions as an intermediate access mechanism to the actual value in storage.
   -- Since all access to the data in the storage area is pointer based, the pointers must be adjusted in response to
   -- insert/delete/modify operations. This is done by the portal manager.
   --
   type Portal is record
      Manager_Ptr: Item_Manager_Ptr;
      Is_Valid: Boolean := False;
      Item_Offset: Unsigned_32;
      Index: Integer := -1;
      Column: Integer := -1;
      Reference_Count: Integer := 0;
   end record;

   Zero_Storage: constant boolean := True; -- Used during testing, should be False for deployment.

end BRBON;

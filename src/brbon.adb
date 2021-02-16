package body BRBON is


   -- =======================

   function Item_Manager_Factory(
                                 Use_Endianness: in Brbon.Endianness := Machine_Endianness;
                                 Byte_Count: in Unsigned_32 := 10 * 2**10;
                                 Root_Type: in Item_Type := Br_Dictionary
                                ) return Item_Manager_Ptr is

      subtype Storage_Index is Unsigned_32 range 0 .. Byte_Count;
      Storage : Storage_Area_Ptr;
      Im: Item_Manager_Ptr;


   begin

      -- Allocate storage

      Storage := new Storage_Area(Storage_Index);


      -- Clear the storage area if required.

      if Brbon.Zero_Storage then Storage.all := (others => 0); end if;


      -- Create

      -- Create the item manager record.

      Im := new Item_Manager'(
             Use_Endianness,
             Byte_Count,
             Storage,
             new Portal'(
               Manager_Ptr     => null,
               Is_Valid        => False,
               Item_Offset     => 0,
               Index           => -1,
               Column          => -1,
               Reference_Count => 0),
             (Portal_List => new List_Of_Portals.List)
             );


      return Im;

   end Item_Manager_Factory;

   function Unused_Storage(Manager_Ptr: in Item_Manager_Ptr) return Unsigned_32 is
   begin
      return 0;
   end Unused_Storage;

   procedure Store (Value: in Integer_8; At_Path: String) is
   begin
      null;
   end Store;


   procedure Store (Value: in Integer_16; At_Path: String) is
   begin
      null;
   end Store;

end BRBON;

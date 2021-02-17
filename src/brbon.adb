with Ada.Iterator_Interfaces;
with Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;use System;

with memmove;
with BRBON.Portal_Manager_Operations; use BRBON.Portal_Manager_Operations;


package body BRBON is

   procedure Deallocate_Storage_Area is new Ada.Unchecked_Deallocation (Storage_Area, Storage_Area_Ptr);

   function To_Address is new Unchecked_Conversion (Storage_Area_Ptr, Address);
   function To_Unsigned_8_Ptr is new Unchecked_Conversion (Storage_Area_Ptr, Unsigned_8_Ptr);


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
                              (Portal_List => new List_Of_Portals.List),
                                Zero_Content => False
             );


      return Im;

   end Item_Manager_Factory;

   function Unused_Storage(Manager: in Item_Manager_Ptr) return Unsigned_32 is
   begin
      return 0;
   end Unused_Storage;

   function To_Item_Name (Manager: in Item_Manager_Ptr; Value: UTF_8_String) return Item_Name is
      New_Item_Name: Item_Name := Item_Name_Vector_245.To_Vector (0);
   begin
      for I in Value'Range loop
         if Integer (Item_Name_Vector_245.Last_Index (New_Item_Name)) < Integer (Item_Name_Vector_245.Capacity (New_Item_Name)) then raise Item_Name_Too_Long; end if;
         Item_Name_Vector_245.Append (New_Item_Name, (Unsigned_8 (Character'Pos (Value (I)))));
      end loop;
      return New_Item_Name;
   end To_Item_Name;

   function To_UTF_8_String (Manager: in Item_Manager_Ptr; Value: Item_Name) return UTF_8_String is
      Count: Integer := Integer (Item_Name_Vector_245.Length (Value));
      Utf8: UTF_8_String (1 .. Count) := (others => ' ');
      Index: Integer := 1;
   begin
      for C in Value.Iterate loop
         Utf8 (Index) := Character'Val (Item_Name_Vector_245.Element (C));
         Index := Index + 1;
      end loop;
      return Utf8;
   end To_UTF_8_String;

   procedure Store (Value: in Integer_8; At_Path: String) is
   begin
      null;
   end Store;


   procedure Store (Value: in Integer_16; At_Path: String) is
   begin
      null;
   end Store;


   procedure Increase_Storage_Byte_Count (Mgr: Item_Manager_Ptr; Value: Unsigned_32) is

      -- Round the new byte count up to the nearest 32 bit boundary
      Bytes: Unsigned_32 := (Value or 2#0100#) and 16#FFFFFFFC#;

      -- The new storage area to be created (if necessary)
      New_Area_Ptr: Storage_Area_Ptr;

      -- A temporary pointer for the area to be deallocated
      Deallocation_Ptr: Storage_Area_Ptr := Mgr.Storage_Ptr;

      -- Useless return value from memmove
      Dummy_Address: Address;

   begin

      -- Exit if the requested size is smaller or equal than the current size
      --
      if Bytes <= Mgr.Storage_Ptr.all'Length then return; end if;

      -- Allocate a new storage are
      --
      New_Area_Ptr := new Storage_Area (1 .. Bytes);

      -- Check if the buffer must be zero'd
      --
      if Mgr.Zero_Content then New_Area_Ptr.all (1 .. Bytes) := (others => 0); end if;

      -- Move the old content to the new area
      --
      Dummy_Address := memmove(
                               dest => To_Address (New_Area_Ptr),
                               src  => To_Address (Mgr.Storage_Ptr),
                               num  => Mgr.Storage_Ptr.all'Length);

      -- Update the portal pointers
      --
      Update_Portal_Pointers (Mgr          => Mgr.Portal_Mgr,
                              At_And_Above => To_Unsigned_8_Ptr (Mgr.Storage_Ptr),
                              Below        => To_Unsigned_8_Ptr (Mgr.Storage_Ptr + Mgr.Storage_Ptr.all'Length),
                              To_New_Base  => To_Unsigned_8_Ptr (New_Area_Ptr));

      -- Assign new area to manager
      --
      Mgr.Storage_Ptr := New_Area_Ptr;

      -- Deallocate the old storage area
      --
      Deallocate_Storage_Area (Deallocation_Ptr);

   end Increase_Storage_Byte_Count;


end BRBON;

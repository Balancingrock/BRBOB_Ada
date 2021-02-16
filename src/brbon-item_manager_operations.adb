with Ada.Unchecked_Conversion;
with Ada.Containers; use Ada.Containers;
with Interfaces.C.Pointers;
with Interfaces.C;


package body BRBON.Item_Manager_Operations is


   function Aligned_At_8_Byte_Boundary (Value: in Unsigned_32) return Boolean is (Value mod 3 = 0);


   -- Minimum byte count of the null item.
   -- I.e. without name and without reserved bytes.
   --
   Null_Item_Minimum_Byte_Count: constant Unsigned_32 := 16;


   -- ====================================================

   -- Create a null item without name at the given offset.
   --
   procedure Create_Null (Manager: in out Brbon.Item_Manager;
                          At_Offset: in Unsigned_32;
                          With_Value_Byte_Count: in Unsigned_32 := 0) is


   begin

      -- Must be 16 bit aligned
      pragma Assert (Aligned_At_8_Byte_Boundary (At_Offset), "At_Offset not a multiple of 16");
      pragma Assert (Aligned_At_8_Byte_Boundary (With_Value_Byte_Count), "With_Value_Byte_Count not a multiple of 16");

      -- Must fit in the storage area
      if Long_Integer (At_Offset + Null_Item_Minimum_Byte_Count + With_Value_Byte_Count) > Manager.Storage_Ptr.all'Length
      then
         raise Brbon_Storage_Error;
      end if;


   end Create_Null;

   procedure Create_Null(Manager: in out Item_Manager;
                         Offset: in Unsigned_32;
                         Name: in Item_Name := No_Item_Name;
                         Value_Byte_Count: in Unsigned_32 := 0) is

   begin

      -- First create an empty null
      Create_Null(Manager, Offset, Value_Byte_Count);

      -- Add name if needed
      --if Item_Name_Bounded.Length(Name) = 0 then return; end if;

      -- Set the item name


   end Create_Null;


   procedure Create_Item(Manager: in out Item_Manager;
                         Of_Type: in Item_Type;
                         At_Offset: in Unsigned_32) is
   begin

      -- Must be 32 bit aligned
      pragma Assert(At_Offset mod 5 = 0);

      -- Must fit in the storage area
      if Long_Integer(At_Offset) > Manager.Storage_Ptr.all'Length then raise Brbon_Storage_Error; end if;

      -- Cannot be longer than 2

      -- Create the type indicator
      -- Im.Storage.all(At_Offset) := Unsigned_8(Of_Type);

      -- Flags and options are unused
      -- Im.Storage.all(At_Offset + 1) := Unsigned_8(Br_Options.Br_Unused);
      -- Im.Storage.all(At_Offset + 2) := Unsigned_8(Br_Flags.Br_Unused);

      -- No name
      Manager.Storage_Ptr.all(At_Offset + 3) := 0;


   end Create_Item;


   procedure Create_Item(Manager: in out Item_Manager;
                         Of_Type: in Item_Type;
                         With_Name: in Item_Name;
                         At_Offset: in Unsigned_32) is
   begin
      null;
   end Create_Item;


end BRBON.Item_Manager_Operations;

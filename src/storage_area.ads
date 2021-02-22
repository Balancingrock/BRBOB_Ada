with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with BRBON; use BRBON;
with BRBON_Configure; use BRBON_Configure;


package Storage_Area is


   -- The area in which items are stored.
   -- Since this is a top level definition, all allocations should be deallocated when no longer needed.
   --
   --
   type Storage_Area is new Limited_Controlled with
      record
         Data: Array_Of_Unsigned_8_Ptr;
         Uses_Endianness: Endianness;
      end record;


   -- Cleanup on destruction.
   --
   procedure Finalization (S: in out Storage_Area);


   -- Unsigned_8 access
   --
   procedure Set_Unsigned_8 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_8);
   function Get_Unsigned_8 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_8;

   -- Unsigned_16 access
   --
   procedure Set_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_16);
   function Get_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_16;

   -- Unsigned_32 access
   --
   procedure Set_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_32);
   function Get_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_32;

   -- Unsigned_64 access
   --
   procedure Set_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_64);
   function Get_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_64;

   -- Integer_8 access
   --
   procedure Set_Integer_8 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_8);
   function Get_Integer_8 (S: Storage_Area; Offset: Unsigned_32) return Integer_8;

   -- Integer_16 access
   --
   procedure Set_Integer_16 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_16);
   function Get_Integer_16 (S: Storage_Area; Offset: Unsigned_32) return Integer_16;

   -- Integer_32 access
   --
   procedure Set_Integer_32 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_32);
   function Get_Integer_32 (S: Storage_Area; Offset: Unsigned_32) return Integer_32;

   -- Integer_64 access
   --
   procedure Set_Integer_64 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_64);
   function Get_Integer_64 (S: Storage_Area; Offset: Unsigned_32) return Integer_64;

   -- Float_32 access
   --
   procedure Set_Float_32 (S: Storage_Area; Offset: Unsigned_32; Value: IEEE_Float_32);
   function Get_Float_32 (S: Storage_Area; Offset: Unsigned_32) return IEEE_Float_32;

   -- Float_64 access
   --
   procedure Set_Float_64 (S: Storage_Area; Offset: Unsigned_32; Value: IEEE_Float_64);
   function Get_Float_64 (S: Storage_Area; Offset: Unsigned_32) return IEEE_Float_64;

   -- Unsigned_8_Array access
   -- Note: It is assumed that enough storage area is available.
   --
   procedure Set_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: Array_Of_Unsigned_8);
   procedure Get_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: in out Array_Of_Unsigned_8);


   -- ===============
   -- Creating Types
   -- ===============

   procedure Create_Item_Null        (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0);
   procedure Create_Item_Bool        (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Boolean := False);
   procedure Create_Item_Integer_8   (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0);
   procedure Create_Item_Integer_16  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0);
   procedure Create_Item_Integer_32  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0);
   procedure Create_Item_Integer_64  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0);
   procedure Create_Item_Unsigned_8  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0);
   procedure Create_Item_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0);
   procedure Create_Item_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0);
   procedure Create_Item_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0);
   procedure Create_Item_Float_32    (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0);
   procedure Create_Item_Float_64    (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0);
   procedure Create_Item_String      (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: String := "");
   procedure Create_Item_CRC_String  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: String := "");
   procedure Create_Item_Binary      (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := (0));
   procedure Create_Item_CRC_Binary  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := (0));
   procedure Create_Item_Array       (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool);
   procedure Create_Item_Dictionary  (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0);
   procedure Create_Item_Sequence    (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0);
   procedure Create_Item_Table       (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0);
   procedure Create_Item_UUID        (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID);
   procedure Create_Item_RGBA        (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: RGBA := RGBA_Black);
   procedure Create_Item_Font        (S: Storage_Area; Offset: Unsigned_32; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0, Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font);


   -- A pointer to the storage area
   --
   type Storage_Area_Ptr is access Storage_Area;


   -- Create a new storage object.
   -- Note: Deallocation of the new storage area is under responsibility of the callee.
   --
   function Allocate_And_Create (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr;


   -- For deallocation when the storage area is no longer needed.
   --
   procedure Deallocate is new Ada.Unchecked_Deallocation (Storage_Area, Storage_Area_Ptr);


end Storage_Area;

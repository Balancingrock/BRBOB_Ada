with Ada.Finalization; use Ada.Finalization;
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

   type Storage_Area_Ptr is access Storage_Area;


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


   -- Create a new storage object.
   --
   function New_Storage_Area (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr;


private

end Storage_Area;

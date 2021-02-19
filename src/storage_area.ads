with Ada.Finalization; use Ada.Finalization;

with BRBON_Configure; use BRBON_Configure;
with BRBON_Basic_Types; use BRBON_Basic_Types;


package Storage_Area is


   -- The area in which items are stored.
   -- Since this is a top level definition, all allocations should be deallocated when no longer needed.
   --
   type Unsigned_8_Array is array (Unsigned_32 range <>) of aliased Unsigned_8 with Pack;
   for Unsigned_8_Array'Alignment use 32;
   --
   type Unsigned_8_Array_Ptr is access Unsigned_8_Array;
   --
   type Storage_Area is new Limited_Controlled with
      record
         Data: Unsigned_8_Array_Ptr;
         Uses_Endianness: Endianness;
      end record;


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
   procedure Set_Float_32 (S: Storage_Area; Offset: Unsigned_32; Value: Float_32);
   function Get_Float_32 (S: Storage_Area; Offset: Unsigned_32) return Float_32;

   -- Float_64 access
   --
   procedure Set_Float_64 (S: Storage_Area; Offset: Unsigned_32; Value: Float_64);
   function Get_Float_64 (S: Storage_Area; Offset: Unsigned_32) return Float_64;

   -- Unsigned_8_Array access
   -- Note: It is assumed that enough storage area is available.
   --
   procedure Set_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_8_Array);
   procedure Get_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: in out Unsigned_8_Array);


   -- Create a new storage object.
   --
   function New_Storage_With_Size (Byte_Count: Unsigned_32) return Storage_Area;


private

end Storage_Area;

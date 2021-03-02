with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with BRBON; use BRBON;
with BRBON_Configure; use BRBON_Configure;
with Pointer_Math; use Pointer_Math;


package Storage_Area is


   -- The area in which items are stored.
   -- Since this is a top level definition, all allocations should be deallocated when no longer needed.
   --
   type Storage_Area is new Limited_Controlled with
      record
         Data: Array_Of_Unsigned_8_Ptr;
         Uses_Endianness: Endianness;
         Swap: Boolean;
      end record;


   -- Returns a pointer to an element in the storage array;
   --
   function Get_Unsigned_8_Ptr (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_8_Ptr is (S.Data.all(0)'Access + Offset);
   pragma Inline (Get_Unsigned_8_Ptr);


   -- Cleanup on destruction.
   --
   procedure Finalization (S: in out Storage_Area);


   -- BR Item type access
   --
   procedure Set_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Type); pragma Inline (Set_Item_Type);
   function Valid_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32) return Boolean; pragma Inline (Valid_Item_Type);
   function Get_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32) return BR_Item_Type; pragma Inline (Get_Item_Type);

   -- BR Item Options access
   --
   procedure Set_Item_Options (S: Storage_Area'Class; Offset: Unsigned_32; Value: Item_Options); pragma Inline (Set_Item_Options);
   function Get_Item_Options (S: Storage_Area'Class; Offset: Unsigned_32) return Item_Options is (To_Item_Options (S.Data (Offset)));

   -- BR Item Flags access
   --
   procedure Set_Item_Flags (S: Storage_Area'Class; Offset: Unsigned_32; Value: Item_Flags); pragma Inline (Set_Item_Flags);
   function Get_Item_Flags (S: Storage_Area'Class; Offset: Unsigned_32) return Item_Flags is (To_Item_Flags (S.Data (Offset))); pragma Inline (Get_Item_Flags);

   -- Bool access
   --
   procedure Set_Bool (S: Storage_Area'Class; Offset: Unsigned_32; Value: Boolean); pragma Inline (Set_Bool);
   function Get_Bool (S: Storage_Area'Class; Offset: Unsigned_32) return Boolean is (S.Data (Offset) /= 0); pragma Inline (Get_Bool);

   -- Unsigned_8 access
   --
   procedure Set_Unsigned_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_8);
   function Get_Unsigned_8 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_8 is (S.Data (Offset));
   pragma Inline (Get_Unsigned_8);

   -- Unsigned_16 access
   --
   procedure Set_Unsigned_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_16);
   function Get_Unsigned_16 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_16 is (if S.Swap then Swap_Unsigned_16 (To_Unsigned_16 (S.Data (Offset .. Offset + 1))) else To_Unsigned_16 (S.Data (Offset .. Offset + 1)));
   pragma Inline (Get_Unsigned_16);

   -- Unsigned_32 access
   --
   procedure Set_Unsigned_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_32);
   function Get_Unsigned_32 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_32 is (if S.Swap then Swap_Unsigned_32 (To_Unsigned_32 (S.Data (Offset .. Offset + 3))) else To_Unsigned_32 (S.Data (Offset .. Offset + 3)));
   pragma Inline (Get_Unsigned_32);

   -- Unsigned_64 access
   --
   procedure Set_Unsigned_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_64);
   function Get_Unsigned_64 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_64 is (if S.Swap then Swap_Unsigned_64 (To_Unsigned_64 (S.Data (Offset .. Offset + 7))) else To_Unsigned_64 (S.Data (Offset .. Offset + 7)));
   pragma Inline (Get_Unsigned_64);

   -- Integer_8 access
   --
   procedure Set_Integer_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_8);
   function Get_Integer_8 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_8 is (To_Integer_8 (S.Data (Offset)));
   pragma Inline (Get_Integer_8);

   -- Integer_16 access
   --
   procedure Set_Integer_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_16);
   function Get_Integer_16 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_16 is (if S.Swap then Swap_Integer_16 (To_Integer_16 (S.Data (Offset .. Offset + 1))) else To_Integer_16 (S.Data (Offset .. Offset + 1)));
   pragma Inline (Get_Integer_16);

   -- Integer_32 access
   --
   procedure Set_Integer_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_32);
   function Get_Integer_32 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_32 is (if S.Swap then Swap_Integer_32 (To_Integer_32 (S.Data (Offset .. Offset + 3))) else To_Integer_32 (S.Data (Offset .. Offset + 3)));
   pragma Inline (Get_Integer_32);

   -- Integer_64 access
   --
   procedure Set_Integer_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_64);
   function Get_Integer_64 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_64  is (if S.Swap then Swap_Integer_64 (To_Integer_64 (S.Data (Offset .. Offset + 7))) else To_Integer_64 (S.Data (Offset .. Offset + 7)));
   pragma Inline (Get_Integer_64);

   -- Float_32 access
   --
   procedure Set_Float_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_32);
   function Get_Float_32 (S: Storage_Area'Class; Offset: Unsigned_32) return IEEE_Float_32 is (if S.Swap then Swap_Float_32 (To_Float_32 (S.Data (Offset .. Offset + 3))) else To_Float_32 (S.Data (Offset .. Offset + 3)));
   pragma Inline (Get_Float_32);


   -- Float_64 access
   --
   procedure Set_Float_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_64);
   function Get_Float_64 (S: Storage_Area'Class; Offset: Unsigned_32) return IEEE_Float_64 is (if S.Swap then Swap_Float_64 (To_Float_64 (S.Data (Offset .. Offset + 7))) else To_Float_64 (S.Data (Offset .. Offset + 7)));
   pragma Inline (Get_Float_64);


   -- Unsigned_8_Array access
   -- Note: It is assumed that enough storage area is available.
   --
   procedure Set_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Value: Array_Of_Unsigned_8);
   procedure Get_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Value: out Array_Of_Unsigned_8);


   -- String access
   --
   procedure Set_String (S: Storage_Area'Class; Offset: Unsigned_32; Value: String);
   procedure Get_String (S: Storage_Area'Class; Offset: Unsigned_32; Value: out String);


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

-- =====================================================================================================================
--
--  File:       Storage_Area.ads
--  Project:    BRBON
--
--  Version:    0.1.0
--
--  Author:     Marinus van der Lugt
--  Company:    http://balancingrock.nl
--  Repository: https://github.com/Balancingrock/BRBON_Ada
--
--  Copyright:  (c) 2021 Marinus van der Lugt, All rights reserved.
--
--  License:    MIT, see LICENSE file
--
--  And because I need to make a living:
--
--   - You can send payment (you choose the amount) via paypal to: sales@balancingrock.nl
--   - Or wire bitcoins to: 1GacSREBxPy1yskLMc9de2nofNv2SNdwqH
--
--  If you prefer to pay in another way, please contact me at rien@balancingrock.nl
--
--  Prices/Quotes for support, modifications or enhancements can also be obtained from: rien@balancingrock.nl
--
-- =====================================================================================================================
-- Purpose
--
-- This package associates a memory area with methods that provide read and write access for primitive data types.
-- All access will respect the endianess (byte order) that is associated with the memory area upon creation.
--
-- All access methods will access the memory area as told, no checks will be made. Exceptions will be raised where
-- offsets or data contents are not as assumed/expected.
--
-- =====================================================================================================================
-- History
--
-- 0.1.0 - Initial version
--
-- =====================================================================================================================

with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with BRBON; use BRBON;
with BRBON_Configure; use BRBON_Configure;
with Pointer_Math; use Pointer_Math;


-- Provides a byte array as a storage area with the necessary access operations for basic types.
-- The storage container has an associated endianness that will be used to read/write the multi-byte data.
--
package Storage_Area is


   -- ====================
   -- Definitions
   -- ====================

   -- The container in which items are stored.
   -- Since this is a top level definition, all allocations should be deallocated when no longer needed.
   --
   type Storage_Area is new Limited_Controlled with
      record
         Data: Array_Of_Unsigned_8_Ptr;
         Swap: Boolean; -- Set to True or False on creation depending on the necessity for swapping the byte order
      end record;

   -- A pointer to a storage area
   --
   type Storage_Area_Ptr is access Storage_Area;


   -- =====================
   -- Management
   -- =====================

   -- Create a new storage object and returns a pointer to it.
   -- To reclaim the memory occupied by the storage container the callee must call 'Deallocate (The_Pointer)'
   --
   function Allocate_And_Create (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr;

   -- Cleanup on destruction is necessary.
   --
   overriding
   procedure Finalize (S: in out Storage_Area);

   -- For deallocation when the storage area is no longer needed.
   --
   procedure Deallocate is new Ada.Unchecked_Deallocation (Storage_Area, Storage_Area_Ptr);


   -- =======================
   -- Operational
   -- =======================

   -- Write a BR_Item_Type to the byte at the given offset.
   --
   procedure Set_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Type);
   pragma Inline (Set_Item_Type);

   -- Return true if the byte at the given offset contains a valid BR-Item_Type coding.
   --
   function Valid_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32) return Boolean is (S.Data (Offset) > Unsigned_8 (BR_Item_Type'Pos (BR_Item_Type'Last)));
   pragma Inline (Valid_Item_Type);

   -- Return the BR_Item_Type from the given offset. Will raise BRBON.Illegal_Item_Type when the bit pattern does not represent a valid type.
   --
   function Get_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32) return BR_Item_Type is (BR_Item_Type'Val (S.Data (Offset)));
   pragma Inline (Get_Item_Type);

   -- Write the item options to the byte at the given offset.
   --
   procedure Set_Item_Options (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Options);
   pragma Inline (Set_Item_Options);

   -- Return the item options from the given offset.

   function Get_Item_Options (S: Storage_Area'Class; Offset: Unsigned_32) return BR_Item_Options is (To_BR_Item_Options (S.Data (Offset)));
   pragma Inline (Get_Item_Options);

   -- Write the Item Flags to the byte at the given offset.
   --
   procedure Set_Item_Flags (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Flags);
   pragma Inline (Set_Item_Flags);

   -- Return the item flags from the byte at the given offset.
   --
   function Get_Item_Flags (S: Storage_Area'Class; Offset: Unsigned_32) return BR_Item_Flags is (To_BR_Item_Flags (S.Data (Offset)));
   pragma Inline (Get_Item_Flags);

   -- Write a boolean value to the byte at the given offset.
   -- 0x00 for False, 0x01 for True.
   --
   procedure Set_Bool (S: Storage_Area'Class; Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Set_Bool);

   -- Returns a boolean as read from the byte at the given offset.
   -- 0x00 = False, anything else = True
   --
   function Get_Bool (S: Storage_Area'Class; Offset: Unsigned_32) return Boolean is (S.Data (Offset) /= 0);
   pragma Inline (Get_Bool);

   -- Write an Unsigned_8 to the byte at the given offset.
   --
   procedure Set_Unsigned_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Unsigned_8);

   -- Returns the byte at the given offset as a Unsigned_8.
   --
   function Get_Unsigned_8 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_8 is (S.Data (Offset));
   pragma Inline (Get_Unsigned_8);

   -- Write Unsigned_16 to the byte at the given offset and the next byte at the higher address.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Unsigned_16);

   -- Read the byte at the offset and the next byte at the higher address as an Unsigned_16.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_16 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_16 is
     (if S.Swap then Swap_Unsigned_16 (To_Unsigned_16 (S.Data (Offset .. Offset + 1))) else To_Unsigned_16 (S.Data (Offset .. Offset + 1)));
   pragma Inline (Get_Unsigned_16);

   -- Write Unsigned_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Unsigned_32);

   -- Read the byte at the offset and the next 3 bytes at the higher addresses as an Unsigned_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_32 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_32 is
     (if S.Swap then Swap_Unsigned_32 (To_Unsigned_32 (S.Data (Offset .. Offset + 3))) else To_Unsigned_32 (S.Data (Offset .. Offset + 3)));
   pragma Inline (Get_Unsigned_32);

   -- Write Unsigned_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_64);
   pragma Inline (Set_Unsigned_64);

   -- Read the byte at the offset and the next 7 bytes at the higher addresses as an Unsigned_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_64 (S: Storage_Area'Class; Offset: Unsigned_32) return Unsigned_64 is
     (if S.Swap then Swap_Unsigned_64 (To_Unsigned_64 (S.Data (Offset .. Offset + 7))) else To_Unsigned_64 (S.Data (Offset .. Offset + 7)));
   pragma Inline (Get_Unsigned_64);

   -- Write an Integer_8 to the byte at the given offset.
   --
   procedure Set_Integer_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_8);
   pragma Inline (Set_Integer_8);

   -- Returns the byte at the given offset as an Integer_8.
   --
   function Get_Integer_8 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_8 is (To_Integer_8 (S.Data (Offset)));
   pragma Inline (Get_Integer_8);

   -- Write Integer_16 to the byte at the given offset and the next byte at the higher address.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_16);
   pragma Inline (Set_Integer_16);

   -- Read the byte at the offset and the next byte at the higher address as an Unsigned_16.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_16 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_16 is
     (if S.Swap then Swap_Integer_16 (To_Integer_16 (S.Data (Offset .. Offset + 1))) else To_Integer_16 (S.Data (Offset .. Offset + 1)));
   pragma Inline (Get_Integer_16);

   -- Write Integer_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_32);
   pragma Inline (Set_Integer_32);

   -- Read the byte at the offset and the next 3 bytes at the higher addresses as an Integer_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_32 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_32 is
     (if S.Swap then Swap_Integer_32 (To_Integer_32 (S.Data (Offset .. Offset + 3))) else To_Integer_32 (S.Data (Offset .. Offset + 3)));
   pragma Inline (Get_Integer_32);

   -- Write Integer_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_64);
   pragma Inline (Set_Integer_64);

   -- Read the byte at the offset and the next 7 bytes at the higher addresses as an Integer_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_64 (S: Storage_Area'Class; Offset: Unsigned_32) return Integer_64  is
     (if S.Swap then Swap_Integer_64 (To_Integer_64 (S.Data (Offset .. Offset + 7))) else To_Integer_64 (S.Data (Offset .. Offset + 7)));
   pragma Inline (Get_Integer_64);

   -- Write Float_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Float_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_32);

   -- Read the byte at the offset and the next 3 bytes at the higher addresses as a Float_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Float_32 (S: Storage_Area'Class; Offset: Unsigned_32) return IEEE_Float_32 is
     (if S.Swap then Swap_Float_32 (To_Float_32 (S.Data (Offset .. Offset + 3))) else To_Float_32 (S.Data (Offset .. Offset + 3)));
   pragma Inline (Get_Float_32);

   -- Write Float_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Float_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_64);
   pragma Inline (Set_Float_64);

   -- Read the byte at the offset and the next 7 bytes at the higher addresses as a Float_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Float_64 (S: Storage_Area'Class; Offset: Unsigned_32) return IEEE_Float_64 is
     (if S.Swap then Swap_Float_64 (To_Float_64 (S.Data (Offset .. Offset + 7))) else To_Float_64 (S.Data (Offset .. Offset + 7)));
   pragma Inline (Get_Float_64);

   -- Write all the bytes that make up the string to successive bytes starting at the given offset.
   --
   procedure Set_String (S: Storage_Area'Class; Offset: Unsigned_32; Value: String);
   pragma Inline (Set_String);

   -- Return successive bytes starting at the given offset for the given length as a string.
   --
   function Get_String (S: Storage_Area'Class; Offset: Unsigned_32; Length: Unsigned_32) return String;
   pragma Inline (Get_String);

   -- Write all the bytes in the array to successive bytes starting at the given offset.
   --
   procedure Set_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Value: Array_Of_Unsigned_8);
   pragma Inline (Set_Unsigned_8_Array);

   -- Return successive bytes starting at the given offset for the given length as a unsigned_8 array.
   --
   function Get_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Length: Unsigned_32) return Array_Of_Unsigned_8;
   pragma Inline (Get_Unsigned_8_Array);


end Storage_Area;

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
--  And because I too need to make a living:
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

with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Configure; use BRBON.Configure;


-- @summary
-- An endianness aware byte container.
--
-- @description
-- Access to byte and multi-byte data in a memory image. While the access is endianness aware, at this level the API user
-- has to specify the endianness of the data. At the higher levels APIs this will be set automatically where possible.
--
package BRBON.Container is


   -- ====================
   -- Definitions
   -- ====================

   --  The container to be used for storage/retrieval of byte based data.
   --
   type Byte_Store is tagged private;

   -- A pointer to a binary store
   --
   type Byte_Store_Ptr is access all Byte_Store;


   -- =====================
   -- Management
   -- =====================

   -- Create a new byte store in the provided buffer. The buffer will be nilled if the BRBON.Configure Zero_Storage is set to True.
   -- @param Buffer_Ptr The memory area to be used for storage.
   -- @param Using_Endianness The endianness (Big or Little) to be used in the storage area.
   -- @return The new byte store.
   --
   function Byte_Store_Factory (Buffer_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Byte_Store;

   -- Read the contents of the file into the given buffer, then use this as the new Byte_Store.
   -- @param Path The path to a file on the local filesystem that will be read.
   --
   function Byte_Store_Factory (Buffer_Ptr: Array_Of_Unsigned_8_Ptr; Path: String; Using_Endianness: Endianness) return Boolean; -- Byte_Store;

   -- Save the content of the byte store to file.
   -- @param Filepath The location in the filesystem to store the data.
   -- @exception File_Too_Large Raised when a file has a byte count > Unsigned_32'Last.
   -- @exception Placeholder Most system defined file associated exceptions
   --
   procedure Write_to_File (S: in out Byte_Store'Class; Filepath: String);

   -- Returns the byte count of the store
   --
   function Length (S: in out Byte_Store) return Unsigned_32;

   -- Returns the endianness of the data in the store
   --
   function Uses_Endianness (S: in out Byte_Store) return Endianness;


   -- =======================
   -- For testing purposes
   -- =======================

   -- Return a part of the store
   --
   procedure Test_Support_Get_Bytes (S: in out Byte_Store; Start: Unsigned_32; Dest: out Array_Of_Unsigned_8);


   -- =======================
   -- Operational
   -- =======================

   -- Write a BR_Item_Type to the byte at the given offset.
   --
   procedure Set_Item_Type (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: BR_Item_Type);
   pragma Inline (Set_Item_Type);

   -- Return true if the byte at the given offset contains a valid BR-Item_Type coding.
   --
   function Valid_Item_Type (S: Byte_Store'Class; Offset: Unsigned_32) return Boolean;
   pragma Inline (Valid_Item_Type);

   -- Return the BR_Item_Type from the given offset. Will raise BRBON.Illegal_Item_Type when the bit pattern does not represent a valid type.
   --
   function Get_Item_Type (S: Byte_Store'Class; Offset: Unsigned_32) return BR_Item_Type;
   pragma Inline (Get_Item_Type);

   -- Write the item options to the byte at the given offset.
   --
   procedure Set_Item_Options (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: BR_Item_Options);
   pragma Inline (Set_Item_Options);

   -- Return the item options from the given offset.

   function Get_Item_Options (S: Byte_Store'Class; Offset: Unsigned_32) return BR_Item_Options;
   pragma Inline (Get_Item_Options);

   -- Write the Item Flags to the byte at the given offset.
   --
   procedure Set_Item_Flags (S:in out  Byte_Store'Class; Offset: Unsigned_32; Value: BR_Item_Flags);
   pragma Inline (Set_Item_Flags);

   -- Return the item flags from the byte at the given offset.
   --
   function Get_Item_Flags (S: Byte_Store'Class; Offset: Unsigned_32) return BR_Item_Flags;
   pragma Inline (Get_Item_Flags);

   -- Write a boolean value to the byte at the given offset.
   -- 0x00 for False, 0x01 for True.
   --
   procedure Set_Bool (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Set_Bool);

   -- Returns a boolean as read from the byte at the given offset.
   -- 0x00 = False, anything else = True
   --
   function Get_Bool (S: Byte_Store'Class; Offset: Unsigned_32) return Boolean;
   pragma Inline (Get_Bool);

   -- Write an Unsigned_8 to the byte at the given offset.
   --
   procedure Set_Unsigned_8 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Unsigned_8);

   -- Returns the byte at the given offset as a Unsigned_8.
   --
   function Get_Unsigned_8 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Unsigned_8);

   -- Write Unsigned_16 to the byte at the given offset and the next byte at the higher address.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_16 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Unsigned_16);

   -- Read the byte at the offset and the next byte at the higher address as an Unsigned_16.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_16 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_Unsigned_16);

   -- Write Unsigned_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_32 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Unsigned_32);

   -- Read the byte at the offset and the next 3 bytes at the higher addresses as an Unsigned_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_32 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Unsigned_32);

   -- Write Unsigned_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_64 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Unsigned_64);
   pragma Inline (Set_Unsigned_64);

   -- Read the byte at the offset and the next 7 bytes at the higher addresses as an Unsigned_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_64 (S: Byte_Store'Class; Offset: Unsigned_32) return Unsigned_64;
   pragma Inline (Get_Unsigned_64);

   -- Write an Integer_8 to the byte at the given offset.
   --
   procedure Set_Integer_8 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_8);
   pragma Inline (Set_Integer_8);

   -- Returns the byte at the given offset as an Integer_8.
   --
   function Get_Integer_8 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_8;
   pragma Inline (Get_Integer_8);

   -- Write Integer_16 to the byte at the given offset and the next byte at the higher address.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_16 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_16);
   pragma Inline (Set_Integer_16);

   -- Read the byte at the offset and the next byte at the higher address as an Unsigned_16.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_16 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_16;
   pragma Inline (Get_Integer_16);

   -- Write Integer_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_32 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_32);
   pragma Inline (Set_Integer_32);

   -- Read the byte at the offset and the next 3 bytes at the higher addresses as an Integer_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_32 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_32;
   pragma Inline (Get_Integer_32);

   -- Write Integer_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_64 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Integer_64);
   pragma Inline (Set_Integer_64);

   -- Read the byte at the offset and the next 7 bytes at the higher addresses as an Integer_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_64 (S: Byte_Store'Class; Offset: Unsigned_32) return Integer_64;
   pragma Inline (Get_Integer_64);

   -- Write Float_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Float_32 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: IEEE_Float_32);

   -- Read the byte at the offset and the next 3 bytes at the higher addresses as a Float_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Float_32 (S: Byte_Store'Class; Offset: Unsigned_32) return IEEE_Float_32;
   pragma Inline (Get_Float_32);

   -- Write Float_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Float_64 (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: IEEE_Float_64);
   pragma Inline (Set_Float_64);

   -- Read the byte at the offset and the next 7 bytes at the higher addresses as a Float_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Float_64 (S: Byte_Store'Class; Offset: Unsigned_32) return IEEE_Float_64;
   pragma Inline (Get_Float_64);

   -- Write all the bytes that make up the string to successive bytes starting at the given offset.
   --
   procedure Set_String (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: String);
   pragma Inline (Set_String);

   -- Return successive bytes starting at the given offset for the given length as a string.
   --
   function Get_String (S: Byte_Store'Class; Offset: Unsigned_32; Length: Unsigned_32) return String;
   pragma Inline (Get_String);

   -- Write all the bytes in the array to successive bytes starting at the given offset.
   --
   procedure Set_Unsigned_8_Array (S: in out Byte_Store'Class; Offset: Unsigned_32; Value: Array_Of_Unsigned_8);
   pragma Inline (Set_Unsigned_8_Array);

   -- Return successive bytes starting at the given offset for the given length as a unsigned_8 array.
   --
   function Get_Unsigned_8_Array (S: Byte_Store'Class; Offset: Unsigned_32; Length: Unsigned_32) return Array_Of_Unsigned_8;
   pragma Inline (Get_Unsigned_8_Array);

private

      type Byte_Store is tagged
      record
         Data: Array_Of_Unsigned_8_Ptr;
         Swap: Boolean; -- Set to True or False on creation depending on the necessity for swapping the byte order
      end record;

end BRBON.Container;

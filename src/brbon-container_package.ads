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
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with BRBON; use BRBON;


-- @summary
-- An endianness aware byte container.
--
-- @description
-- Access to byte and multi-byte data in a memory image. While the access is endianness aware, at this level the API user
-- has to specify the endianness of the data. At the higher levels APIs this will be set automatically where possible.
--
package BRBON.Container_Package is


   -- The root type for all type of blocks.
   --
   type Container is new Ada.Finalization.Controlled with private;

   type Container_Ptr is access all Container;


   -- ==========================================================================
   -- Management
   -- ==========================================================================


   -- Use this operation to initialize a container which is not created through one of the factory methods.
   --
   procedure Setup
     (
      C: in out Container;
      Memory_Ptr: Unsigned_8_Array_Ptr;
      Byte_Order: Byte_Storage_Order;
      Deallocate_On_Finalization: Boolean
     );


   -- Create a container using the provided memory area. The memory area will be zeroed if the BRBON.Zero_Storage is set to True.
   -- @param Memory_Ptr: The memory area to be used for block.
   -- @param Byte_Order: The byte order to be used for the multi-byte data in the block.
   -- @return The new container.
   --
   function Factory (Memory_Ptr: Unsigned_8_Array_Ptr; Byte_Order: Byte_Storage_Order) return Container;


   -- Create a container with the specified byte count.
   --
   function Factory (Byte_Count: Unsigned_32; Byte_Order: Byte_Storage_Order) return Container;


   -- Read the contents of the file into the given buffer, then use this as the new Block.
   -- @param Filepath: The path to a file on the local filesystem that will be read.
   --
   function Factory (Filepath: String) return Container;


   -- Save the content of the block to file.
   -- @param Filepath: The location in the filesystem to store the data.
   --
   procedure Write_To_File (C: Container; Filepath: String);


   -- Returns the number of bytes that can be stored
   --
   function Byte_Count (C: Container) return Unsigned_32;


   -- Returns true if the multi-byte data in the container will be swapped on read/write.
   --
   function Swap_Status (C: Container) return Boolean;


   -- ==========================================================================
   -- Bascic operations
   -- ==========================================================================

   -- Write a boolean value to the byte at the given offset.
   -- 0x00 for False, 0x01 for True.
   --
   procedure Set_Bool (C: Container; Offset: Unsigned_32; Value: Boolean);
   pragma Inline (Set_Bool);


   -- Returns a boolean as read from the byte at the given offset.
   -- 0x00 = False, anything else = True
   --
   function Get_Bool (C: Container; Offset: Unsigned_32) return Boolean;
   pragma Inline (Get_Bool);


   -- Write an Unsigned_8 to the byte at the given offset.
   --
   procedure Set_Unsigned_8 (C: Container; Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Unsigned_8);


   -- Returns the byte at the given offset as a Unsigned_8.
   --
   function Get_Unsigned_8 (C: Container; Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Unsigned_8);


   -- Write Unsigned_16 to the byte at the given offset and the next byte at the higher address.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_16 (C: Container; Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Unsigned_16);


   -- Read the byte at the offset and the next byte at the higher address as an Unsigned_16.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_16 (C: Container; Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_Unsigned_16);


   -- Write Unsigned_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_32 (C: Container; Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Unsigned_32);


   -- Read the byte at the offset and the next 3 bytes at the higher addresses as an Unsigned_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_32 (C: Container; Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Unsigned_32);


   -- Write Unsigned_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Unsigned_64 (C: Container; Offset: Unsigned_32; Value: Unsigned_64);
   pragma Inline (Set_Unsigned_64);


   -- Read the byte at the offset and the next 7 bytes at the higher addresses as an Unsigned_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Unsigned_64 (C: Container; Offset: Unsigned_32) return Unsigned_64;
   pragma Inline (Get_Unsigned_64);


   -- Write an Integer_8 to the byte at the given offset.
   --
   procedure Set_Integer_8 (C: Container; Offset: Unsigned_32; Value: Integer_8);
   pragma Inline (Set_Integer_8);


   -- Returns the byte at the given offset as an Integer_8.
   --
   function Get_Integer_8 (C: Container; Offset: Unsigned_32) return Integer_8;
   pragma Inline (Get_Integer_8);


   -- Write Integer_16 to the byte at the given offset and the next byte at the higher address.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_16 (C: Container; Offset: Unsigned_32; Value: Integer_16);
   pragma Inline (Set_Integer_16);


   -- Read the byte at the offset and the next byte at the higher address as an Unsigned_16.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_16 (C: Container; Offset: Unsigned_32) return Integer_16;
   pragma Inline (Get_Integer_16);


   -- Write Integer_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_32 (C: Container; Offset: Unsigned_32; Value: Integer_32);
   pragma Inline (Set_Integer_32);


   -- Read the byte at the offset and the next 3 bytes at the higher addresses as an Integer_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_32 (C: Container; Offset: Unsigned_32) return Integer_32;
   pragma Inline (Get_Integer_32);


   -- Write Integer_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Integer_64 (C: Container; Offset: Unsigned_32; Value: Integer_64);
   pragma Inline (Set_Integer_64);


   -- Read the byte at the offset and the next 7 bytes at the higher addresses as an Integer_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Integer_64 (C: Container; Offset: Unsigned_32) return Integer_64;
   pragma Inline (Get_Integer_64);


   -- Write Float_32 to the byte at the given offset and the next 3 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Float_32 (C: Container; Offset: Unsigned_32; Value: IEEE_Float_32);
   pragma Inline (Set_Float_32);


   -- Read the byte at the offset and the next 3 bytes at the higher addresses as a Float_32.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Float_32 (C: Container; Offset: Unsigned_32) return IEEE_Float_32;
   pragma Inline (Get_Float_32);


   -- Write Float_64 to the byte at the given offset and the next 7 bytes at the higher addresses.
   -- The endianess will be according to the endianess set for the storage area.
   --
   procedure Set_Float_64 (C: Container; Offset: Unsigned_32; Value: IEEE_Float_64);
   pragma Inline (Set_Float_64);


   -- Read the byte at the offset and the next 7 bytes at the higher addresses as a Float_64.
   -- The endianess will be according to the endianess set for the storage area.
   --
   function Get_Float_64 (C: Container; Offset: Unsigned_32) return IEEE_Float_64;
   pragma Inline (Get_Float_64);


   -- Write all the bytes that make up the string to successive bytes starting at the given offset.
   --
   procedure Set_String (C: Container; Offset: Unsigned_32; Value: String);
   pragma Inline (Set_String);


   -- Return successive bytes starting at the given offset for the given length as a string.
   --
   function Get_String (C: Container; Offset: Unsigned_32; Length: Unsigned_32) return String;
   pragma Inline (Get_String);


   -- Write all the bytes in the array to successive bytes starting at the given offset.
   --
   procedure Set_Unsigned_8_Array (C: Container; Offset: Unsigned_32; Value: BRBON.Unsigned_8_Array);
   pragma Inline (Set_Unsigned_8_Array);


   -- Return successive bytes starting at the given offset for the given length as a unsigned_8 array.
   --
   function Get_Unsigned_8_Array (C: Container; Offset: Unsigned_32; Length: Unsigned_32) return Unsigned_8_Array;
   pragma Inline (Get_Unsigned_8_Array);


   -- Return the CRC-16 over the specified range.
   -- @param Start The first byte to be included in the CRC-16.
   -- @param Count The number of bytes to include in the CRC-16.
   -- @return The CRC-16 of the specified bytes.
   --
   function Get_CRC_16_Over_Range (C: Container; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_CRC_16_Over_Range);


   -- Return the CRC-32 over the specified range.
   -- @param Start The first byte to be included in the CRC-32.
   -- @param Count The number of bytes to include in the CRC-32.
   -- @return The CRC-32 of the specified bytes.
   --
   function Get_CRC_32_Over_Range (C: Container; Start: Unsigned_32; Count: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_CRC_32_Over_Range);


   -- ==========================================================================
   -- For testing purposes
   -- ==========================================================================

   -- Return a part of the store
   --
   procedure Test_Support_Get_Bytes (C: Container; Start: Unsigned_32; Dest: out BRBON.Unsigned_8_Array);

   -- Create a hex dump of the contents
   --
   procedure Test_Support_Hex_Dump (C: Container);


private


   type Container is new Ada.Finalization.Controlled with
      record
         MPtr: Unsigned_8_Array_Ptr;
         Swap: Boolean;
         Deallocate_On_Finalization: Boolean;
      end record;

   procedure Set_Data_Byte_Order (C: in out Container; Value: Byte_Storage_Order);


end BRBON.Container_Package;

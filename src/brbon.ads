-- =====================================================================================================================
--
--  File:       BRBON.ads
--  Project:    BRBON_Ada
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
-- This is the top level package for an Ada API to a BRBON implementation.
--
-- Change the endianness in the BRBON.Configure to match your system. Then use any of the following APIs:
--
-- Use BRBON.Container for low-level endianess aware storage.
-- Use BRBON.Static_Unprotected for a read-only and/or single-pass write of a BRBON formatted memory image.
-- Use BRBON.Static_Protected for an Ada-like read/write BRBON formatted memory image.
-- Use BRBON.Dynamic for path-based, DB-like, read/write access to a BRBON formatted memory image.
-- Use BRBON.Thread_Safe see Dynamic, but with a thread safe API.
--
-- Note that the above packages are implemented in sequence and only as necessary for our purposes. They represent
-- the implementation traject and not all may be complete (or even started) when you read this.
--
-- =====================================================================================================================
-- History
--
-- 0.1.0 - Initial version
--
-- =====================================================================================================================
-- Gnatdoc
--
--  @summary
--  Top level package.
--
--  @description
--  Contains configuration, exceptions and top level type definitions.
--
--  ===============================================================================================
--  Note: BRBON_Ada is developped on an as-needed base, the following API levels are the foreseen
--  implementation path. Missing levels are simply not implemented yet.
--  ===============================================================================================
--
--  To use BRBON, make necessary changes below and then use any of the following API's:
--
--  - BRBON.Container For low level byte based access to a memory area. A container is endianness
--    (byte-order) aware, thus all multi-byte accesses will return the data in the proper endianess.
--
--    This level may be usefull to anybody who needs to read/write file data. All formatting must
--    be done by the API user.
--
--  - BRBON.Static_Unprotected For fastest possible access using the type structure defined by the
--    BRBON standard. However the structure cannot be modified in size after it was created and the
--    API user must make sure no to exceed the byte-counts used during creation.
--
--    This level may be usefull for read-only access to predefined (received) data sets (files).
--    Note that there are no protections in place to prevent type mismatches.
--
--  - BRBON.Static_Protected This is the most ADA-like interface. The BRBON structure is generated
--    once, and can be updated afterwards, but it is not possible to change the byte-count's of the
--    items that were generated during creation. Exceptions will be raised if the API user violates
--    an earlier creation.
--
--  - BRBON.Dynamic This is akin to a database interface, data can be read and written and byte-counts
--    will be changed on the fly as needed. The disadvantage is that data may need to be moved around
--    which can lead to longer access times.
--
--    Does not support multi-tasking.
--
--  - BRBON.Thread_Safe This adds protective mechanisms to the Dynamic interface to support multi-tasking.
--
-- Note: If a package is absent, it has not been implemented yet ;-)
--

with Interfaces; use Interfaces;
with Ada.Finalization;


package BRBON is


   -- The BRBON specification includes the endianness of a block of data.
   --
   type Byte_Storage_Order is (MSB_First, LSB_First);


   -- ==========================================================================
   -- Configurable part starts                                                 =
   --                                                                          =


   -- Change this to the storage order of the platform the code will be used on.
   --
   Machine_Byte_Storage_Order: constant Byte_Storage_Order := LSB_First;


   -- To initialize all empty space to zero set the following flag to true.
   -- Note: This is usefull during testing, but is not needed during deployment.
   --
   Zero_Storage: constant Boolean := True;


   --                                                                          =
   -- Configurable part ends                                                   =
   -- ==========================================================================


   -- This type may be used to quickly access an item in storage without having
   -- to perform a lookup or search.
   --
   type Portal is private;


   -- This is root class of all blocks
   --
   type Block is new Ada.Finalization.Controlled with private;
   
   
   -- Raised when a name has an unexpected or illegal value.
   --
   Name_Error: exception;


   -- Raised when an attempt is made to create an illegal block type
   --
   Illegal_Block_Type: exception;


   -- Raised when somthing does not fit in its alotted space.
   --
   Storage_Warning: exception;


   -- Raised when a BRBON structure contains an illegal type pattern.
   --
   Illegal_Item_Type: exception;


   -- Raised when an attempt is made to execute an incompletely coded routine
   --
   Implementation: exception;


   -- Raised when an illegal byte count is specified
   --
   Byte_Count_Error: exception;


   -- Raised when something (see associated message) goes wrong in an array
   --
   Array_Error: exception;


private

   type Item_Header is
      record
         Type_Field: Types.Item_Type;
         Options_Field: Types.Item_Options;
         Flags_Field: Types.Item_Flags;
         Name_Field_Byte_Count_Field: Unsigned_8;
         Byte_Count_Field: Unsigned_32;
         Parent_Offset_Field: Unsigned_32;
         Small_Value_Field: Unsigned_32;
      end record;

   for Item_Header'Size use Item_Header_Byte_Count * 8;

   for Item_Header use
      record
         Type_Field at 0 range 0..7;
         Options_Field at 1 range 0..7;
         Flags_Field at 2 range 0..7;
         Name_Field_Byte_Count_Field at 3 range 0..7;
         Byte_Count_Field at 4 range 0..31;
         Parent_Offset_Field at 8 range 0..31;
         Small_Value_Field at 12 range 0..31;
      end record;

   type Item_Header_Ptr is access Item_Header;

   type Portal_Type is (Null_Portal, Normal, Element, Field);

   type Portal is
      record
         Is_Type: Portal_Type;
         Is_Valid: Boolean := True;
         --
         Item_Ptr: Item_Package.Item_Header_Ptr;
         Element_Index: Unsigned_32 := 0;
         Column_Index: Unsigned_32 := 0;
      end record;

   function Portal_Factory
     (
      Item_Ptr: Item_Header_Ptr;
      Element_Index: Unsigned_32 := Unsigned_32'Max;
      Column_Index: Unsigned_32 := Unsigned_32'Max
     ) return Portal;


   type Unsigned_8_Array is array (Unsigned_32 range <>) of aliased Unsigned_8;

   type Unsigned_8_Array_Ptr is access Unsigned_8_Array;

   type Block is new Ada.Finalization.Controlled with
      record
         Data: Unsigned_8_Array_Ptr;
         Swap: Boolean;
      end record;

end BRBON;

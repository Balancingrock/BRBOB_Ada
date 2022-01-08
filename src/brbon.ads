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
--  A note about the naming. Names in this library are chosen to create a readable code. If a package
--  name does not end with "_Package" then the most readable name will result when "use"-ing that
--  package. For all other package names, only a use of "BRBON" will create the best readable name.
--
--  Example: The package BRBON.Block.Header contains the function Get_Type. A readable expression
--  is thus Block.Header.Get_Type (A_Block).
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
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Strings.Bounded;


package BRBON is


   -- The BRBON specification includes the endianness of a block of data.
   --
   type Byte_Storage_Order is (MSB_First, LSB_First);

   
   -- Maximum length of names used to locate items or columns.
   --
   Max_Name_Length: constant := 245;
   
   
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


   -- Implemented item types
   
   type Item_Type is
     (
      Illegal,
      Null_Type,
      Bool_Type,
      Int_8_Type, Int_16_Type, Int_32_Type, Int_64_Type,
      UInt_8_Type, UInt_16_Type, UInt_32_Type, UInt_64_Type,
      Float_32_Type, Float_64_Type,
      String_Type, Crc_String_Type,
      Binary_Type, Crc_Binary_Type,
      Array_Type, Dictionary_Type, Sequence_Type, Table_Type,
      UUID_Type,
      RGBA_Type,
      Font_Type
     );

   for Item_Type'Size use 8;

   for Item_Type use
     (
      Illegal         => 0,
      Null_Type       => 16#01#,
      Bool_Type       => 16#02#,
      Int_8_Type      => 16#03#,
      Int_16_Type     => 16#04#,
      Int_32_Type     => 16#05#,
      Int_64_Type     => 16#06#,
      UInt_8_Type     => 16#07#,
      UInt_16_Type    => 16#08#,
      UInt_32_Type    => 16#09#,
      UInt_64_Type    => 16#0A#,
      Float_32_Type   => 16#0B#,
      Float_64_Type   => 16#0C#,
      String_Type     => 16#0D#,
      CRC_String_Type => 16#0E#,
      Binary_Type     => 16#0F#,
      CRC_Binary_Type => 16#10#,
      Array_Type      => 16#11#,
      Dictionary_Type => 16#12#,
      Sequence_Type   => 16#13#,
      Table_Type      => 16#14#,
      UUID_Type       => 16#15#,
      RGBA_Type       => 16#16#,
      Font_Type       => 16#17#
     );
   
   
   -- Possible options for types.
   
   type Item_Options is
      record
         Option_0: Boolean;
         Option_1: Boolean;
         Option_2: Boolean;
         Option_3: Boolean;
         Option_4: Boolean;
         Option_5: Boolean;
         Option_6: Boolean;
         Option_7: Boolean;
      end record;

   for Item_Options'Size use 8;

   for Item_Options use
      record
         Option_0 at 0 range 0..0;
         Option_1 at 0 range 1..1;
         Option_2 at 0 range 2..2;
         Option_3 at 0 range 3..3;
         Option_4 at 0 range 4..4;
         Option_5 at 0 range 5..5;
         Option_6 at 0 range 6..6;
         Option_7 at 0 range 7..7;
      end record;
   
   No_Item_Options: constant Item_Options := (false, false, false, false, false, false, false, false);
   
   -- Possible item flags
   
   type Item_Flags is
      record
         Flag_0: Boolean;
         Flag_1: Boolean;
         Flag_2: Boolean;
         Flag_3: Boolean;
         Flag_4: Boolean;
         Flag_5: Boolean;
         Flag_6: Boolean;
         Flag_7: Boolean;
      end record;

   for Item_Flags'Size use 8;

   for Item_Flags use
      record
         Flag_0 at 0 range 0..0;
         Flag_1 at 0 range 1..1;
         Flag_2 at 0 range 2..2;
         Flag_3 at 0 range 3..3;
         Flag_4 at 0 range 4..4;
         Flag_5 at 0 range 5..5;
         Flag_6 at 0 range 6..6;
         Flag_7 at 0 range 7..7;
      end record;

   No_Item_Flags: constant Item_Flags := (false, false, false, false, false, false, false, false);


   -- For blocks

   type Block_Type is
     (
      Illegal,
      Single_Item_Block
     );
   
   for Block_Type'Size use 16;
   
   for Block_Type use
     (
      Illegal           => 0,
      Single_Item_Block => 1
     );


   type Block_Options is
      record
         Reacquisition_Possible: Boolean;
         Option_1: Boolean;
         Option_2: Boolean;
         Option_3: Boolean;
         Option_4: Boolean;
         Option_5: Boolean;
         Option_6: Boolean;
         Option_7: Boolean;
         Options_8_15: Unsigned_8;
      end record;
      
   for Block_Options'Size use 16;
   
   for Block_Options use
      record 
         Reacquisition_Possible at 0 range 0..0;
         Option_1               at 0 range 1..1;
         Option_2               at 0 range 2..2;
         Option_3               at 0 range 3..3;
         Option_4               at 0 range 4..4;
         Option_5               at 0 range 5..5;
         Option_6               at 0 range 6..6;
         Option_7               at 0 range 7..7;
         Options_8_15           at 1 range 0..7;
      end record;
   
   No_Block_Options: constant Block_Options := (False, False, False, False, False, False, False, False, 0);
   
   
   -- This type may be used to quickly access an item in storage without having
   -- to perform a lookup or search.
   --
   type Portal is private;

   
   -- This is root class of all blocks.
   --
   type Store is new Ada.Finalization.Controlled with private;
   
   
   -- Use name field assistents if a name must be used mutliple times in a call to a BRBON API.
   -- This will speed up access to the designated item.
   --
   type Name_Field_Assistent is private;
   
   Maximum_Item_Name_Length: constant := 245;
   
   package Item_Name_Bounded_String_Package is new Ada.Strings.Bounded.Generic_Bounded_Length (Maximum_Item_Name_Length);
   
   subtype Item_Name is Item_Name_Bounded_String_Package.Bounded_String;
   
   
   -- Returns a name field assistent that can be used to speed up access to items.
   --
   -- Note that assistents are tied to a store and should not be used with different stores.
   -- An exception will be raised if used between incompatible stores.
   --
   function Name_Field_Assistent_Factory (Name: String; S: Store) return Name_Field_Assistent;
   
   
   -- The type of array all items are stored in.
   --
   type Unsigned_8_Array is array (Unsigned_32 range <>) of aliased Unsigned_8;
   
   
   -- All data storage should be allocated dynamically.
   --
   type Unsigned_8_Array_Ptr is access Unsigned_8_Array;
   
   
   -- To release allocated storage.
   --
   procedure Deallocate_Unsigned_8_Array is new Ada.Unchecked_Deallocation (Unsigned_8_Array, Unsigned_8_Array_Ptr);
   
   
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


   -- Raised when the fourth synchronisation byte is wrong
   --
   Byte_Storage_Order_Error: exception;
   
   
   -- Raised when a string is too long
   --
   String_Too_Long: exception;
   
   
   -- Raised when an incompatibility arises, check description for details.
   --
   Incompatible: exception;
   
   
private

   Item_Header_Byte_Count: constant := 16;
   
   
   -- Storage support
   
   type Unsigned_8_Ptr is access all Unsigned_8;
   type Unsigned_16_Ptr is access Unsigned_16;
   type Unsigned_32_Ptr is access Unsigned_32;
   
   function To_Unsigned_16_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_16_Ptr);
   function To_Unsigned_32_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Unsigned_32_Ptr);
   
   
   -- Item Header
   
   type Item_Header is
      record
         Type_Field: Item_Type;
         Options_Field: Item_Options;
         Flags_Field: Item_Flags;
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
   
   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_Ptr);

      
   -- Block
   
   type Store is new Ada.Finalization.Controlled with
      record
         Data: Unsigned_8_Array_Ptr;
         Swap: Boolean;
      end record;
   
   type Store_Pointer is access all Store;
   
   
   -- Portal
   
   type Portal_Type is (Null_Portal, Normal, Element, Field);

   type Portal is
      record
         SPtr: Store_Pointer;
         --
         Is_Type: Portal_Type;
         Is_Valid: Boolean := True;
         --
         Item_Ptr: Item_Header_Ptr;
         Element_Index: Unsigned_32 := 0;
         Column_Index: Unsigned_32 := 0;
      end record;


   type Quick_Check_Value is
      record
         CRC: Unsigned_16;
         Count: Unsigned_8;
         Char: Unsigned_8;
      end record;

   for Quick_Check_Value'Size use 32;

   for Quick_Check_Value use
      record
         CRC at 0 range 0..15;
         Count at 2 range 0..7;
         Char at 3 range 0..7;
      end record;


   type Name_Field_Assistent is
      record
         Quick_Check: Quick_Check_Value;
         Field_Byte_Count: Unsigned_8;
         Swap: Boolean;
         CRC: Unsigned_16;
         Name_Byte_Count: Unsigned_8;
         Name: Item_Name;
      end record;
   
   
   -- Block operations
   
   procedure Set_Data_Byte_Order (B: in out Store; Value: Byte_Storage_Order);

end BRBON;

with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;
with Ada.Strings.Unbounded;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;
with BRBON.Header_Package;
with BRBON.Footer;
with BRBON.Utils;


with Serializable;


package BRBON.Block is


   -- The available block types.
   --
   type Block_Type is
     (
      Illegal,
      Single_Item_Block
     );
   for Block_Type'Size use 16;
   for Block_Type use
     (
      Illegal => 0,
      Single_Item_Block => 1
     );


   -- The available block options
   -- Use the "or" operator to merge options.
   --
   type Block_Options is
    (
     No_Block_Options,
     Reacquisition_Possible
    );
   for Block_Options'Size use 16;
   for Block_Options use
    (
     No_Block_Options       => 0,
     Reacquisition_Possible => 16#01#
    );


   -- The total number of bytes that will be used by the block if it is saved or transferred now.
   -- Note that for the static implementations this is a constant.
   --
   function Byte_Count (B: BRBON.Block) return Unsigned_32;


   -- Save the content of the block to file.
   -- @param Path The location in the filesystem to store the data.
   --
   procedure Write_To_File (B: BRBON.Block; Path: String) renames BRBON.Container.Write_To_File;


private
   
   -- The size of the leading (fixed) part of a block header
   --
   Block_Header_Leading_Byte_Count: constant Unsigned_16 := 96;


   -- The block header layout
   --
   type Block_Header_Leading is
      record
         Synchronization_Byte_1: Unsigned_8;
         Synchronization_Byte_2: Unsigned_8;
         Synchronization_Byte_3: Unsigned_8;
         Synchronization_Byte_4: Unsigned_8;
         Is_Type: Block_Type;
         Options: Block_Options;

         Block_Byte_Count: Unsigned_32;
         Header_Byte_Count: Unsigned_16;
         Encrypted_Header_Byte_Count: Unsigned_16;

         Origin_CRC: Unsigned_16;
         Identifier_CRC: Unsigned_16;
         Extension_CRC: Unsigned_16;
         Path_Prefix_CRC: Unsigned_16;

         Origin_Byte_Count: Unsigned_8;
         Identifier_Byte_Count: Unsigned_8;
         Extension_Byte_Count: Unsigned_8;
         Path_Prefix_Byte_Count: Unsigned_8;
         Origin_Offset: Unsigned_16;
         Identifier_Offset: Unsigned_16;

         Extension_Offset: Unsigned_16;
         Path_Prefix_Offset: Unsigned_16;
         Acquisition_URL_Byte_Count: Unsigned_16;
         Acquisition_URL_Offset: Unsigned_16;

         Target_List_Byte_Count: Unsigned_16;
         Target_List_Offset: Unsigned_16;
         Public_Key_URL_Byte_Count: Unsigned_16;
         Public_Key_Offset: Unsigned_16;

         Creation_Timestamp: Unsigned_64;
         Modification_Timestamp: Unsigned_64;
         Expiry_Timestamp: Unsigned_64;
      end record;

   for Block_Header_Leading'Size use Block_Header_Leading_Byte_Count * 8;

   for Block_Header_Leading use
      record
         Synchronization_Byte_1     at  0 range 0..7;
         Synchronization_Byte_2     at  1 range 0..7;
         Synchronization_Byte_3     at  2 range 0..7;
         Synchronization_Byte_4     at  3 range 0..7;
         Is_Type                    at  4 range 0..15;
         Options                    at  6 range 0..15;

         Block_Byte_Count           at  8 range 0..31;
         Header_Byte_Count          at 12 range 0..15;
         Encrypted_Header           at 14 range 0..15;

         Origin_CRC                 at 16 range 0..15;
         Identifier_CRC             at 18 range 0..15;
         Extension_CRC              at 20 range 0..15;
         Path_Prefix_CRC            at 22 range 0..15;

         Origin_Byte_Count          at 24 range 0..7;
         Identifier_Byte_Count      at 25 range 0..7;
         Extension_Byte_Count       at 26 range 0..7;
         Path_Prefix_Byte_Count     at 27 range 0..7;
         Origin_Offset              at 28 range 0..15;
         Identifier_Offset          at 30 range 0..15;

         Extension_Offset           at 32 range 0..15;
         Path_Prefix_Offset         at 34 range 0..15;
         Acquisition_URL_Byte_Count at 36 range 0..15;
         Acquisition_URL_Offset     at 38 range 0..15;

         Target_List_Byte_Count     at 40 range 0..15;
         Target_List_Offset         at 42 range 0..15;
         Public_Key_URL_Byte_Count  at 44 range 0..15;
         Public_Key_Offset          at 46 range 0..15;

         Creation_Timestamp         at 48 range 0..63;
         Modification_Timestamp     at 64 range 0..63;
         Expiry_Timestamp           at 80 range 0..63;
      end record;

   type Block_Header_Leading_Ptr is access Block_Header_Leading;

   function To_Block_Header_Leading is new Ada.Unchecked_Conversion (Types.Unsigned_8_Ptr, Block_Header_Leading_Ptr);


   -- Initialises a type 1 block (single item).
   -- Note that the type 1 block is very much the default header structure without any additional data.
   --
   procedure Setup (B: BRBON.Block; For_Byte_Storage_Order: BRBON.Byte_Storage_Order; With_Field_Storage_Byte_Count: Unsigned_16);
   
      
   -- The size of the trailing part of the block header
   --
   Block_Header_Trailing_Byte_Count: constant Unsigned_16 := 8;

   type Block_Header_Trailing is
      record
         Reserved_1: Unsigned_32;
         Reserved_2: Unsigned_16;
         CRC: Unsigned_16;
      end record;

   for Block_Header_Trailing'Size use Block_Header_Trailing_Byte_Count * 8;

   for Block_Header_Trailing use
      record
         Reserved_1 at 0 range 0..31;
         Reserved_2 at 4 range 0..15;
         CRC at 6 range 0..15;
      end record;

   type Block_Header_Trailing_Ptr is access Block_Header_Trailing;

   function To_Block_Header_Trailing is new Ada.Unchecked_Conversion (Types.Unsigned_8_Ptr, Block_Header_Trailing_Ptr);

   
--   type Instance is abstract new Ada.Finalization.Controlled with record
--      Container: BRBON.Container.Instance;
--      Memory_Ptr: Array_Of_Unsigned_8_Ptr; -- The Container does not export its pointer, a copy must be kept.
--      Start_Of_Header_Field_Storage: Unsigned_16;
--      Start_Of_Payload: Unsigned_32; -- quasi constant
--      First_Free_Byte_In_Payload: Unsigned_32; -- range self.Start_Of_Payload .. self.Last_Free_Byte_In_Payload + 1
--      Last_Free_Byte_In_Payload: Unsigned_32; -- will never decrease, may increase for some child classes
--   end record;
   
   
   type U32_Getter is access function (B: BRBON.Block) return Unsigned_32;
   
   type U16_Getter is access function (B: BRBON.Block) return Unsigned_16;
   
   type U8_Getter is access function (B: BRBON.Block) return Unsigned_8;
   
   type U32_Setter is access procedure (B: BRBON.Block; Value: Unsigned_32);
   
   type U16_Setter is access procedure (B: BRBON.Block; Value: Unsigned_16);
   
   type U8_Setter is access procedure (B: BRBON.Block; Value: Unsigned_8);

      
   -- =================================================================
   -- | The following operations are for test-use only. Do not use.   |
   -- | (they are not stable and may have unanticipated side effects) |
   -- =================================================================
   
   -- Undocumented, for testing only, do not use!
   --
   function Test_Support_Serializer (I: in out Instance) return Serializable.Instance;


end BRBON.Block;

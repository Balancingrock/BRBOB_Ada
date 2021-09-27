with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Block.Header is

   -- A block header
   --
   type Instance is tagged private;

   -- A block header pointer
   --
   type Instance_Ptr is access all Instance;

   -- The block options
   --
   type Options is
      record
         ReAcquisitionIsPossible: Boolean;
         Bit_01: Boolean;
         Bit_02: Boolean;
         Bit_03: Boolean;
         Bit_04: Boolean;
         Bit_05: Boolean;
         Bit_06: Boolean;
         Bit_07: Boolean;
         Bit_08: Boolean;
         Bit_09: Boolean;
         Bit_10: Boolean;
         Bit_11: Boolean;
         Bit_12: Boolean;
         Bit_13: Boolean;
         Bit_14: Boolean;
         Bit_15: Boolean;
      end record;
   for Options use
      record
         ReAcquisitionIsPossible at 0 range 0..0;
         Bit_01 at 0 range 1..1;
         Bit_02 at 0 range 2..2;
         Bit_03 at 0 range 3..3;
         Bit_04 at 0 range 4..4;
         Bit_05 at 0 range 5..5;
         Bit_06 at 0 range 6..6;
         Bit_07 at 0 range 7..7;
         Bit_08 at 0 range 8..8;
         Bit_09 at 0 range 9..9;
         Bit_10 at 0 range 10..10;
         Bit_11 at 0 range 11..11;
         Bit_12 at 0 range 12..12;
         Bit_13 at 0 range 13..13;
         Bit_14 at 0 range 14..14;
         Bit_15 at 0 range 15..15;
      end record;
   for Options'Size use 16;

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Options, Unsigned_16);
   function To_Options is new Ada.Unchecked_Conversion (Unsigned_16, Options);

   No_Options: constant Options := To_Options (0);

   -- The block header minimum byte count is given by the fixed layout byte count + the trailing long word.
   -- To calculate the block header byte count add the type dependent byte count and field storage byte count.
   --
   Minimum_Byte_Count: constant array (Instance_Type) of Unsigned_16 :=
     (
      0,                -- BRBON.Block.Illegal
      9 * 8 + 0 + 0 + 8 -- BRBON.Block.Single_Item_File
     );


   -- ===================
   -- Low Level Interface
   -- ===================

   -- Set the block synchronization header byte 1 to the expected value.
   --
   procedure Set_Synchronization_Byte_1 (H: in out Instance'class);
   pragma inline (Set_Synchronization_Byte_1);

   -- Verifiy that the first byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_1 (H: in out Instance'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_1);

   -- Set the block synchronization header byte 2 to the expected value.
   --
   procedure Set_Synchronization_Byte_2 (H: in out Instance'class);
   pragma inline (Set_Synchronization_Byte_2);

   -- Verifiy that the second byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_2 (H: in out Instance'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_2);

   -- Set the block synchronization header byte 3 to the expected value.
   --
   procedure Set_Synchronization_Byte_3 (H: in out Instance'class);
   pragma inline (Set_Synchronization_Byte_3);

   -- Verifiy that the thrid byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_3 (H: in out Instance'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_3);

   -- Set the block synchronization header byte 4 to the expected value for the endianness given at creation.
   -- Side effect: Afterwards the endianness can be retrieved with Get_Endianness.
   --
   procedure Set_Synchronization_Byte_4 (H: in out Instance'class);
   pragma inline (Set_Synchronization_Byte_4);

   -- Verifiy that the fourth byte of the block synchronisation header has the correct value.
   -- Side effect: If true, the endianness can be retrieved with Get_Endianness.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_4 (H: in out Instance'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_4);

   -- Returns the endianness of the block header.
   -- Is only reliable after synchronization byte 4 has been set or verified with success.
   --
   function Get_Endianness (H: in out Instance'class) return Endianness;
   pragma inline (Get_Endianness);

   -- Sets the type of the block to the given value.
   --
   procedure Set_Block_Type (H: in out Instance'class; Value: BRBON.Block.Instance_Type);
   pragma inline (Set_Block_Type);

   -- Returns the raw value of the block type.
   --
   function Get_Block_Type (H: in out Instance'class) return BRBON.Block.Instance_Type;
   pragma inline (Get_Block_Type);

   -- Set the value for the first reserved field in the block header.
   --
   procedure Set_Block_Options (H: in out Instance'class; Value: Options);
   pragma inline (Set_Block_Options);

   -- Returns the value of the first reserved field in the block header.
   --
   function Get_Block_Options (H: in out Instance'class) return Options;
   pragma inline (Get_Block_Options);

   -- Set the byte count for the complete block.
   -- This value is optional, and initialized to "not used" = 16#FFFF_FFFF#.
   --
   procedure Set_Block_Byte_Count (H: in out Instance'class; Value: Unsigned_32);
   pragma inline (Set_Block_Byte_Count);

   -- Returns the value of the block byte count.
   --
   function Get_Block_Byte_Count (H: in out Instance'class) return Unsigned_32;
   pragma inline (Get_Block_Byte_Count);

   -- Set the block header byte count.
   --
   procedure Set_Block_Header_Byte_Count (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Header_Byte_Count);

   -- Returns the block header byte count.
   --
   function Get_Block_Header_Byte_Count (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Header_Byte_Count);

   -- Set the block encrypted header byte count.
   --
   procedure Set_Block_Encrypted_Header_Byte_Count (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Encrypted_Header_Byte_Count);

   -- Returns the block encrypted header byte count.
   --
   function Get_Block_Encrypted_Header_Byte_Count (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Encrypted_Header_Byte_Count);

   -- Set the CRC16 value of the origin-field in the block header.
   --
   procedure Set_Block_Origin_Crc16 (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Origin_Crc16);

   -- Returns the CRC16 value of the origin-field in the block header.
   --
   function Get_Block_Origin_Crc16 (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Origin_Crc16);

   -- Set the CRC16 value of the identifier-field in the block header.
   --
   procedure Set_Block_Identifier_Crc16 (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Identifier_Crc16);

   -- Returns the CRC16 value of the identifier-field in the block header.
   --
   function Get_Block_Identifier_Crc16 (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Identifier_Crc16);

   -- Set the CRC16 value of the extension-field in the block header.
   --
   procedure Set_Block_Extension_Crc16 (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Extension_Crc16);

   -- Returns the CRC16 value of the extension-field in the block header.
   --
   function Get_Block_Extension_Crc16 (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Extension_Crc16);

   -- Set the CRC16 value of the path-prefix-field in the block header.
   --
   procedure Set_Block_Path_Prefix_Crc16 (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Path_Prefix_Crc16);

   -- Returns the CRC16 value of the path-prefix-field in the block header.
   --
   function Get_Block_Path_Prefix_Crc16 (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Path_Prefix_Crc16);

   -- Set the byte count of the block-origin field.
   --
   procedure Set_Block_Origin_Byte_Count (H: in out Instance'class; Value: Unsigned_8);
   pragma inline (Set_Block_Origin_Byte_Count);

   -- Returns the byte count of the block-origin field.
   --
   function Get_Block_Origin_Byte_Count (H: in out Instance'class) return Unsigned_8;
   pragma inline (Get_Block_Origin_Byte_Count);

   -- Set the byte count of the block-identifier field.
   --
   procedure Set_Block_Identifier_Byte_Count (H: in out Instance'class; Value: Unsigned_8);
   pragma inline (Set_Block_Identifier_Byte_Count);

   -- Returns the byte count of the block-identifier field.
   --
   function Get_Block_Identifier_Byte_Count (H: in out Instance'class) return Unsigned_8;
   pragma inline (Get_Block_Identifier_Byte_Count);

   -- Set the byte count of the block-extension field.
   --
   procedure Set_Block_Extension_Byte_Count (H: in out Instance'class; Value: Unsigned_8);
   pragma inline (Set_Block_Extension_Byte_Count);

   -- Returns the byte count of the block-extension field.
   --
   function Get_Block_Extension_Byte_Count (H: in out Instance'class) return Unsigned_8;
   pragma inline (Get_Block_Extension_Byte_Count);

   -- Set the byte count of the block-path-prefix field.
   --
   procedure Set_Block_Path_Prefix_Byte_Count (H: in out Instance'class; Value: Unsigned_8);
   pragma inline (Set_Block_Path_Prefix_Byte_Count);

   -- Returns the byte count of the block-path-prefix field.
   --
   function Get_Block_Path_Prefix_Byte_Count (H: in out Instance'class) return Unsigned_8;
   pragma inline (Get_Block_Path_Prefix_Byte_Count);

   -- Set the offset of the block-origin field.
   --
   procedure Set_Block_Origin_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Origin_Offset);

   -- Returns the offset of the block-origin field.
   --
   function Get_Block_Origin_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Origin_Offset);

   -- Set the offset of the block-identifier field.
   --
   procedure Set_Block_Identifier_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Identifier_Offset);

   -- Returns the offset of the block-identifier field.
   --
   function Get_Block_Identifier_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Identifier_Offset);

   -- Set the offset of the block-extension field.
   --
   procedure Set_Block_Extension_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Extension_Offset);

   -- Returns the offset of the block-extension field.
   --
   function Get_Block_Extension_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Extension_Offset);

   -- Set the offset of the block-path-prefix field.
   --
   procedure Set_Block_Path_Prefix_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Path_Prefix_Offset);

   -- Returns the offset of the block-path-prefix field.
   --
   function Get_Block_Path_Prefix_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Path_Prefix_Offset);

   -- Set the value for the byte count of the Acquisition URL field.
   --
   procedure Set_Block_Acquisition_URL_Byte_Count (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Acquisition_URL_Byte_Count);

   -- Returns the value of the byte count of the Acquisition URL field.
   --
   function Get_Block_Acquisition_URL_Byte_Count (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Acquisition_URL_Byte_Count);

   -- Set the value for the offset of the Acquisition URL field.
   --
   procedure Set_Block_Acquisition_URL_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Acquisition_URL_Offset);

   -- Returns the value of the offset of the Acquisition URL field.
   --
   function Get_Block_Acquisition_URL_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Acquisition_URL_Offset);

   -- Set the byte count of the target list field
   --
   procedure Set_Block_Target_List_Byte_Count (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Target_List_Byte_Count);

   -- Return the byte count of the target list field
   --
   function Get_Block_Target_List_Byte_Count (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Target_List_Byte_Count);

   -- Set the offset of the target list field
   --
   procedure Set_Block_Target_List_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Target_List_Offset);

   -- Return the offset of the target list field
   --
   function Get_Block_Target_List_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Target_List_Offset);

   -- Set the byte count of the public key url field
   --
   procedure Set_Block_Public_Key_URL_Byte_Count (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Public_Key_URL_Byte_Count);

   -- Return the byte count of the public key url field
   --
   function Get_Block_Public_Key_URL_Byte_Count (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Public_Key_URL_Byte_Count);

   -- Set the offset of the public key url field
   --
   procedure Set_Block_Public_Key_URL_Offset (H: in out Instance'class; Value: Unsigned_16);
   pragma inline (Set_Block_Public_Key_URL_Offset);

   -- Return the ofset of the public key url field
   --
   function Get_Block_Public_Key_URL_Offset (H: in out Instance'class) return Unsigned_16;
   pragma inline (Get_Block_Public_Key_URL_Offset);

   -- Set the block creation timestamp
   --
   procedure Set_Block_Creation_Timestamp (H: in out Instance'class; Value: Unsigned_64);
   pragma inline (Set_Block_Creation_Timestamp);

   -- Return the block creation timestamp
   --
   function Get_Block_Creation_Timestamp (H: in out Instance'class) return Unsigned_64;
   pragma inline (Get_Block_Creation_Timestamp);

   -- Set the block modification timestamp
   --
   procedure Set_Block_Modification_Timestamp (H: in out Instance'class; Value: Unsigned_64);
   pragma inline (Set_Block_Modification_Timestamp);

   -- Return the block modification timestamp
   --
   function Get_Block_Modification_Timestamp (H: in out Instance'class) return Unsigned_64;
   pragma inline (Get_Block_Modification_Timestamp);

   -- Set the block expiry timestamp
   --
   procedure Set_Block_Expiry_Timestamp (H: in out Instance'class; Value: Unsigned_64);
   pragma inline (Set_Block_Expiry_Timestamp);

   -- Return the block expiry timestamp
   --
   function Get_Block_Expiry_Timestamp (H: in out Instance'class) return Unsigned_64;
   pragma inline (Get_Block_Expiry_Timestamp);

   -- Set the reserved field 1 value, first 32 bits.
   --
   procedure Set_Reserved_1a (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_32; Value: Unsigned_32);
   pragma inline (Set_Reserved_1a);

   -- Return the reserved 1 value, first 32 bits.
   --
   function Get_Reserved_1a (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_32) return Unsigned_32;
   pragma inline (Get_Reserved_1a);

   -- Set the reserved field 1 value, last 16 bits.
   --
   procedure Set_Reserved_1b (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_32; Value: Unsigned_16);
   pragma inline (Set_Reserved_1b);

   -- Return the reserved 1 value, first 16 bits.
   --
   function Get_Reserved_1b (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_32) return Unsigned_16;
   pragma inline (Get_Reserved_1b);

   -- Set the CRC16 value for the entire block header.
   --
   procedure Set_Block_Header_Crc16 (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_32; Value: Unsigned_16);
   pragma inline (Set_Block_Header_Crc16);

   -- Return the CRC16 value for the entire block header.
   --
   function Get_Block_Header_Crc16 (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_32) return Unsigned_16;
   pragma inline (Get_Block_Header_Crc16);

   -- Update the block header crc value in accordance with the block header contents
   --
   procedure Update_Block_Header_Crc16 (H: in out Instance'class);

private

   type Instance is tagged
      record
         Store: Container.Instance;
         Endianness: Types.Endianness; -- Set after verification/assignment of synch byte 4
      end record;

end BRBON.Block.Header;

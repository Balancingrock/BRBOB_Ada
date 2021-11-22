with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Header is


   -- The size of the fixed part of a block header
   --
   Fixed_Part_Byte_Count: constant Unsigned_16 := 9 * 8;

   -- The size of the fixed part of a block header after the type dependent header and the field storage
   --
   Past_Storage_Field_Byte_Count: constant Unsigned_16 := 8;

   -- ===================
   -- Low Level Interface
   -- ===================

   -- Set the block synchronization header byte 1 to the expected value.
   --
   procedure Set_Synchronization_Byte_1 (C: in out BRBON.Container.Instance);
   pragma inline (Set_Synchronization_Byte_1);

   -- Verifiy that the first byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_1 (C: in out BRBON.Container.Instance) return Boolean;
   pragma inline (Verify_Synchronization_Byte_1);

   -- Set the block synchronization header byte 2 to the expected value.
   --
   procedure Set_Synchronization_Byte_2 (C: in out BRBON.Container.Instance);
   pragma inline (Set_Synchronization_Byte_2);

   -- Verifiy that the second byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_2 (C: in out BRBON.Container.Instance) return Boolean;
   pragma inline (Verify_Synchronization_Byte_2);

   -- Set the block synchronization header byte 3 to the expected value.
   --
   procedure Set_Synchronization_Byte_3 (C: in out BRBON.Container.Instance);
   pragma inline (Set_Synchronization_Byte_3);

   -- Verifiy that the thrid byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_3 (C: in out BRBON.Container.Instance) return Boolean;
   pragma inline (Verify_Synchronization_Byte_3);

   -- Set the block synchronization header byte 4 to the expected value for the endianness given at creation.
   -- Side effect: Afterwards the endianness can be retrieved with Get_Endianness.
   --
   procedure Set_Synchronization_Byte_4 (C: in out BRBON.Container.Instance);
   pragma inline (Set_Synchronization_Byte_4);

   -- Verifiy that the fourth byte of the block synchronisation header has the correct value.
   -- Side effect: If true, the endianness can be retrieved with Get_Endianness.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_4 (C: in out BRBON.Container.Instance) return Boolean;
   pragma inline (Verify_Synchronization_Byte_4);

   -- Returns the endianness of the block header.
   -- Is only reliable after synchronization byte 4 has been set or verified with success.
   --
   function Get_Endianness (C: in out BRBON.Container.Instance) return Endianness;
   pragma inline (Get_Endianness);

   -- Sets the type of the block to the given value.
   --
   procedure Set_Block_Type (C: in out BRBON.Container.Instance; Value: Block_Type);
   pragma inline (Set_Block_Type);

   -- Returns the raw value of the block type.
   --
   function Get_Block_Type (C: in out BRBON.Container.Instance) return Block_Type;
   pragma inline (Get_Block_Type);

   -- Set the value for the first reserved field in the block header.
   --
   procedure Set_Block_Options (C: in out BRBON.Container.Instance; Value: Block_Options);
   pragma inline (Set_Block_Options);

   -- Returns the value of the first reserved field in the block header.
   --
   function Get_Block_Options (C: in out BRBON.Container.Instance) return Block_Options;
   pragma inline (Get_Block_Options);

   -- Set the byte count for the complete block.
   -- This value is optional, and initialized to "not used" = 16#FFFF_FFFF#.
   --
   procedure Set_Block_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_32);
   pragma inline (Set_Block_Byte_Count);

   -- Returns the value of the block byte count.
   --
   function Get_Block_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_32;
   pragma inline (Get_Block_Byte_Count);

   -- Set the block header byte count.
   --
   procedure Set_Block_Header_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Header_Byte_Count);

   -- Returns the block header byte count.
   --
   function Get_Block_Header_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Header_Byte_Count);

   -- Set the block encrypted header byte count.
   --
   procedure Set_Block_Encrypted_Header_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Encrypted_Header_Byte_Count);

   -- Returns the block encrypted header byte count.
   --
   function Get_Block_Encrypted_Header_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Encrypted_Header_Byte_Count);

   -- Set the CRC16 value of the origin-field in the block header.
   --
   procedure Set_Block_Origin_Crc16 (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Origin_Crc16);

   -- Returns the CRC16 value of the origin-field in the block header.
   --
   function Get_Block_Origin_Crc16 (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Origin_Crc16);

   -- Set the CRC16 value of the identifier-field in the block header.
   --
   procedure Set_Block_Identifier_Crc16 (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Identifier_Crc16);

   -- Returns the CRC16 value of the identifier-field in the block header.
   --
   function Get_Block_Identifier_Crc16 (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Identifier_Crc16);

   -- Set the CRC16 value of the extension-field in the block header.
   --
   procedure Set_Block_Extension_Crc16 (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Extension_Crc16);

   -- Returns the CRC16 value of the extension-field in the block header.
   --
   function Get_Block_Extension_Crc16 (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Extension_Crc16);

   -- Set the CRC16 value of the path-prefix-field in the block header.
   --
   procedure Set_Block_Path_Prefix_Crc16 (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Path_Prefix_Crc16);

   -- Returns the CRC16 value of the path-prefix-field in the block header.
   --
   function Get_Block_Path_Prefix_Crc16 (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Path_Prefix_Crc16);

   -- Set the byte count of the block-origin field.
   --
   procedure Set_Block_Origin_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_8);
   pragma inline (Set_Block_Origin_Byte_Count);

   -- Returns the byte count of the block-origin field.
   --
   function Get_Block_Origin_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_8;
   pragma inline (Get_Block_Origin_Byte_Count);

   -- Set the byte count of the block-identifier field.
   --
   procedure Set_Block_Identifier_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_8);
   pragma inline (Set_Block_Identifier_Byte_Count);

   -- Returns the byte count of the block-identifier field.
   --
   function Get_Block_Identifier_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_8;
   pragma inline (Get_Block_Identifier_Byte_Count);

   -- Set the byte count of the block-extension field.
   --
   procedure Set_Block_Extension_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_8);
   pragma inline (Set_Block_Extension_Byte_Count);

   -- Returns the byte count of the block-extension field.
   --
   function Get_Block_Extension_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_8;
   pragma inline (Get_Block_Extension_Byte_Count);

   -- Set the byte count of the block-path-prefix field.
   --
   procedure Set_Block_Path_Prefix_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_8);
   pragma inline (Set_Block_Path_Prefix_Byte_Count);

   -- Returns the byte count of the block-path-prefix field.
   --
   function Get_Block_Path_Prefix_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_8;
   pragma inline (Get_Block_Path_Prefix_Byte_Count);

   -- Set the offset of the block-origin field.
   --
   procedure Set_Block_Origin_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Origin_Offset);

   -- Returns the offset of the block-origin field.
   --
   function Get_Block_Origin_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Origin_Offset);

   -- Set the offset of the block-identifier field.
   --
   procedure Set_Block_Identifier_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Identifier_Offset);

   -- Returns the offset of the block-identifier field.
   --
   function Get_Block_Identifier_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Identifier_Offset);

   -- Set the offset of the block-extension field.
   --
   procedure Set_Block_Extension_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Extension_Offset);

   -- Returns the offset of the block-extension field.
   --
   function Get_Block_Extension_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Extension_Offset);

   -- Set the offset of the block-path-prefix field.
   --
   procedure Set_Block_Path_Prefix_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Path_Prefix_Offset);

   -- Returns the offset of the block-path-prefix field.
   --
   function Get_Block_Path_Prefix_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Path_Prefix_Offset);

   -- Set the value for the byte count of the Acquisition URL field.
   --
   procedure Set_Block_Acquisition_URL_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Acquisition_URL_Byte_Count);

   -- Returns the value of the byte count of the Acquisition URL field.
   --
   function Get_Block_Acquisition_URL_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Acquisition_URL_Byte_Count);

   -- Set the value for the offset of the Acquisition URL field.
   --
   procedure Set_Block_Acquisition_URL_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Acquisition_URL_Offset);

   -- Returns the value of the offset of the Acquisition URL field.
   --
   function Get_Block_Acquisition_URL_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Acquisition_URL_Offset);

   -- Set the byte count of the target list field
   --
   procedure Set_Block_Target_List_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Target_List_Byte_Count);

   -- Return the byte count of the target list field
   --
   function Get_Block_Target_List_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Target_List_Byte_Count);

   -- Set the offset of the target list field
   --
   procedure Set_Block_Target_List_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Target_List_Offset);

   -- Return the offset of the target list field
   --
   function Get_Block_Target_List_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Target_List_Offset);

   -- Set the byte count of the public key url field
   --
   procedure Set_Block_Public_Key_URL_Byte_Count (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Public_Key_URL_Byte_Count);

   -- Return the byte count of the public key url field
   --
   function Get_Block_Public_Key_URL_Byte_Count (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Public_Key_URL_Byte_Count);

   -- Set the offset of the public key url field
   --
   procedure Set_Block_Public_Key_URL_Offset (C: in out BRBON.Container.Instance; Value: Unsigned_16);
   pragma inline (Set_Block_Public_Key_URL_Offset);

   -- Return the ofset of the public key url field
   --
   function Get_Block_Public_Key_URL_Offset (C: in out BRBON.Container.Instance) return Unsigned_16;
   pragma inline (Get_Block_Public_Key_URL_Offset);

   -- Set the block creation timestamp
   --
   procedure Set_Block_Creation_Timestamp (C: in out BRBON.Container.Instance; Value: Unsigned_64);
   pragma inline (Set_Block_Creation_Timestamp);

   -- Return the block creation timestamp
   --
   function Get_Block_Creation_Timestamp (C: in out BRBON.Container.Instance) return Unsigned_64;
   pragma inline (Get_Block_Creation_Timestamp);

   -- Set the block modification timestamp
   --
   procedure Set_Block_Modification_Timestamp (C: in out BRBON.Container.Instance; Value: Unsigned_64);
   pragma inline (Set_Block_Modification_Timestamp);

   -- Return the block modification timestamp
   --
   function Get_Block_Modification_Timestamp (C: in out BRBON.Container.Instance) return Unsigned_64;
   pragma inline (Get_Block_Modification_Timestamp);

   -- Set the block expiry timestamp
   --
   procedure Set_Block_Expiry_Timestamp (C: in out BRBON.Container.Instance; Value: Unsigned_64);
   pragma inline (Set_Block_Expiry_Timestamp);

   -- Return the block expiry timestamp
   --
   function Get_Block_Expiry_Timestamp (C: in out BRBON.Container.Instance) return Unsigned_64;
   pragma inline (Get_Block_Expiry_Timestamp);

   -- Set the reserved field 1 value, first 32 bits.
   --
   procedure Set_Reserved_1a (C: in out BRBON.Container.Instance; For_Block_Header_Byte_Count: Unsigned_16; Value: Unsigned_32);
   pragma inline (Set_Reserved_1a);

   -- Return the reserved 1 value, first 32 bits.
   --
   function Get_Reserved_1a (C: in out BRBON.Container.Instance; For_Block_Header_Byte_Count: Unsigned_16) return Unsigned_32;
   pragma inline (Get_Reserved_1a);

   -- Set the reserved field 1 value, last 16 bits.
   --
   procedure Set_Reserved_1b (C: in out BRBON.Container.Instance; For_Block_Header_Byte_Count: Unsigned_16; Value: Unsigned_16);
   pragma inline (Set_Reserved_1b);

   -- Return the reserved 1 value, first 16 bits.
   --
   function Get_Reserved_1b (C: in out BRBON.Container.Instance; For_Block_Header_Byte_Count: Unsigned_16) return Unsigned_16;
   pragma inline (Get_Reserved_1b);

   -- Set the CRC16 value for the entire block header.
   --
   procedure Set_Block_Header_Crc16 (C: in out BRBON.Container.Instance; For_Block_Header_Byte_Count: Unsigned_16; Value: Unsigned_16);
   pragma inline (Set_Block_Header_Crc16);

   -- Return the CRC16 value for the entire block header.
   --
   function Get_Block_Header_Crc16 (C: in out BRBON.Container.Instance; For_Block_Header_Byte_Count: Unsigned_16) return Unsigned_16;
   pragma inline (Get_Block_Header_Crc16);

   -- Update the block header crc value in accordance with the block header contents.
   -- Note: All values in the header must have been set to their final values before calling this operation.
   --
   procedure Update_Block_Header_Crc16 (C: in out BRBON.Container.Instance);


   -- Exception raised when some header element is out of bounds or has an unexpected value.
   -- The error text has additional information.
   --
   Block_Header_Error: Exception;


end BRBON.Header;

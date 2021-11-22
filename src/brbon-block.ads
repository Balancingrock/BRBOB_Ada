with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;
with BRBON.Header;
with BRBON.Footer;

with Serializable;


package BRBON.Block is


   type Instance is abstract new Ada.Finalization.Controlled with private;
      
   
   -- The total number of bytes that will be used by the block if it is saved or transferred.
   --
   function Byte_Count (I: in out Instance) return Unsigned_32 is abstract;

   
   -- The byte count of the area that can be used for item storage.
   --
   function Free_Area_Byte_Count (I: in out Instance) return Unsigned_32 is abstract;
   
   
   -- Writes the block to file.
   -- First the header and footer will be updated to create a valid block.
   --
   procedure Write_To_File (I: in out Instance; To_Path: String);

   
   -- ==========================================================================
   -- Header access start
   -- 
   
   -- Set the block synchronization header byte 1 to the expected value.
   --
   procedure Header_Set_Synchronization_Byte_1 (I: in out Instance);
   pragma inline (Header_Set_Synchronization_Byte_1);
   
   -- Verifiy that the first byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Header_Verify_Synchronization_Byte_1 (I: in out Instance) return Boolean;
   pragma inline (Header_Verify_Synchronization_Byte_1);
   
   -- Set the block synchronization header byte 2 to the expected value.
   --
   procedure Header_Set_Synchronization_Byte_2 (I: in out Instance);
   pragma inline (Header_Set_Synchronization_Byte_2);
   
   -- Verifiy that the second byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Header_Verify_Synchronization_Byte_2 (I: in out Instance) return Boolean;
   pragma inline (Header_Verify_Synchronization_Byte_2);
   
   -- Set the block synchronization header byte 3 to the expected value.
   --
   procedure Header_Set_Synchronization_Byte_3 (I: in out Instance);
   pragma inline (Header_Set_Synchronization_Byte_3);

   -- Verifiy that the thrid byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Header_Verify_Synchronization_Byte_3 (I: in out Instance) return Boolean;
   pragma inline (Header_Verify_Synchronization_Byte_3);

   -- Set the block synchronization header byte 4 to the expected value for the endianness given at creation.
   -- Side effect: Afterwards the endianness can be retrieved with Get_Endianness.
   --
   procedure Header_Set_Synchronization_Byte_4 (I: in out Instance);
   pragma inline (Header_Set_Synchronization_Byte_4);

   -- Verifiy that the fourth byte of the block synchronisation header has the correct value.
   -- Side effect: If true, the endianness can be retrieved with Get_Endianness.
   -- @return True if the synchronization byte is as expected.
   --
   function Header_Verify_Synchronization_Byte_4 (I: in out Instance) return Boolean;
   pragma inline (Header_Verify_Synchronization_Byte_4);

   -- Returns the endianness of the block header.
   -- Is only reliable after synchronization byte 4 has been set or verified with success.
   --
   function Header_Get_Endianness (I: in out Instance) return Endianness;
   pragma inline (Header_Get_Endianness);

   -- Sets the type of the block to the given value.
   --
   procedure Header_Set_Type (I: in out Instance; Value: Block_Type);
   pragma inline (Header_Set_Type);

   -- Returns the raw value of the block type.
   --
   function Header_Get_Type (I: in out Instance) return Block_Type;
   pragma inline (Header_Get_Type);
   function Get_Type (I: in out Instance) return Block_Type renames Header_Get_Type;
   
   -- Set the value for the first reserved field in the block header.
   --
   procedure Header_Set_Options (I: in out Instance; Value: Block_Options);
   pragma inline (Header_Set_Options);

   -- Returns the value of the first reserved field in the block header.
   --
   function Header_Get_Options (I: in out Instance) return Block_Options;
   pragma inline (Header_Get_Options);

   -- Set the byte count for the complete block.
   -- This value is optional, and initialized to "not used" = 16#FFFF_FFFF#.
   --
   procedure Header_Set_Byte_Count (I: in out Instance; Value: Unsigned_32);
   pragma inline (Header_Set_Byte_Count);

   -- Returns the value of the block byte count.
   --
   function Header_Get_Byte_Count (I: in out Instance) return Unsigned_32;
   pragma inline (Header_Get_Byte_Count);
   function Get_Byte_Count (I: in out Instance) return Unsigned_32 renames Header_Get_Byte_Count;

   -- Set the block header byte count.
   --
   procedure Header_Set_Header_Byte_Count (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Header_Byte_Count);

   -- Returns the block header byte count.
   --
   function Header_Get_Header_Byte_Count (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Header_Byte_Count);

   -- Set the block encrypted header byte count.
   --
   procedure Header_Set_Encrypted_Header_Byte_Count (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Encrypted_Header_Byte_Count);

   -- Returns the block encrypted header byte count.
   --
   function Header_Get_Encrypted_Header_Byte_Count (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Encrypted_Header_Byte_Count);

   -- Set the CRC16 value of the origin-field in the block header.
   --
   procedure Header_Set_Origin_Crc16 (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Origin_Crc16);

   -- Returns the CRC16 value of the origin-field in the block header.
   --
   function Header_Get_Origin_Crc16 (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Origin_Crc16);

   -- Set the CRC16 value of the identifier-field in the block header.
   --
   procedure Header_Set_Identifier_Crc16 (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Identifier_Crc16);

   -- Returns the CRC16 value of the identifier-field in the block header.
   --
   function Header_Get_Identifier_Crc16 (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Identifier_Crc16);

   -- Set the CRC16 value of the extension-field in the block header.
   --
   procedure Header_Set_Extension_Crc16 (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Extension_Crc16);

   -- Returns the CRC16 value of the extension-field in the block header.
   --
   function Header_Get_Extension_Crc16 (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Extension_Crc16);

   -- Set the CRC16 value of the path-prefix-field in the block header.
   --
   procedure Header_Set_Path_Prefix_Crc16 (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Path_Prefix_Crc16);

   -- Returns the CRC16 value of the path-prefix-field in the block header.
   --
   function Header_Get_Path_Prefix_Crc16 (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Path_Prefix_Crc16);

   -- Set the byte count of the block-origin field.
   --
   procedure Header_Set_Origin_Byte_Count (I: in out Instance; Value: Unsigned_8);
   pragma inline (Header_Set_Origin_Byte_Count);

   -- Returns the byte count of the block-origin field.
   --
   function Header_Get_Origin_Byte_Count (I: in out Instance) return Unsigned_8;
   pragma inline (Header_Get_Origin_Byte_Count);

   -- Set the byte count of the block-identifier field.
   --
   procedure Header_Set_Identifier_Byte_Count (I: in out Instance; Value: Unsigned_8);
   pragma inline (Header_Set_Identifier_Byte_Count);

   -- Returns the byte count of the block-identifier field.
   --
   function Header_Get_Identifier_Byte_Count (I: in out Instance) return Unsigned_8;
   pragma inline (Header_Get_Identifier_Byte_Count);

   -- Set the byte count of the block-extension field.
   --
   procedure Header_Set_Extension_Byte_Count (I: in out Instance; Value: Unsigned_8);
   pragma inline (Header_Set_Extension_Byte_Count);

   -- Returns the byte count of the block-extension field.
   --
   function Header_Get_Extension_Byte_Count (I: in out Instance) return Unsigned_8;
   pragma inline (Header_Get_Extension_Byte_Count);

   -- Set the byte count of the block-path-prefix field.
   --
   procedure Header_Set_Path_Prefix_Byte_Count (I: in out Instance; Value: Unsigned_8);
   pragma inline (Header_Set_Path_Prefix_Byte_Count);

   -- Returns the byte count of the block-path-prefix field.
   --
   function Header_Get_Path_Prefix_Byte_Count (I: in out Instance) return Unsigned_8;
   pragma inline (Header_Get_Path_Prefix_Byte_Count);

   -- Set the offset of the block-origin field.
   --
   procedure Header_Set_Origin_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Origin_Offset);

   -- Returns the offset of the block-origin field.
   --
   function Header_Get_Origin_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Origin_Offset);

   -- Set the offset of the block-identifier field.
   --
   procedure Header_Set_Identifier_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Identifier_Offset);

   -- Returns the offset of the block-identifier field.
   --
   function Header_Get_Identifier_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Identifier_Offset);

   -- Set the offset of the block-extension field.
   --
   procedure Header_Set_Extension_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Extension_Offset);

   -- Returns the offset of the block-extension field.
   --
   function Header_Get_Extension_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Extension_Offset);

   -- Set the offset of the block-path-prefix field.
   --
   procedure Header_Set_Path_Prefix_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Path_Prefix_Offset);

   -- Returns the offset of the block-path-prefix field.
   --
   function Header_Get_Path_Prefix_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Path_Prefix_Offset);

   -- Set the value for the byte count of the Acquisition URL field.
   --
   procedure Header_Set_Acquisition_URL_Byte_Count (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Acquisition_URL_Byte_Count);

   -- Returns the value of the byte count of the Acquisition URL field.
   --
   function Header_Get_Acquisition_URL_Byte_Count (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Acquisition_URL_Byte_Count);

   -- Set the value for the offset of the Acquisition URL field.
   --
   procedure Header_Set_Acquisition_URL_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Acquisition_URL_Offset);

   -- Returns the value of the offset of the Acquisition URL field.
   --
   function Header_Get_Acquisition_URL_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Acquisition_URL_Offset);

   -- Set the byte count of the target list field
   --
   procedure Header_Set_Target_List_Byte_Count (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Target_List_Byte_Count);

   -- Return the byte count of the target list field
   --
   function Header_Get_Target_List_Byte_Count (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Target_List_Byte_Count);

   -- Set the offset of the target list field
   --
   procedure Header_Set_Target_List_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Target_List_Offset);

   -- Return the offset of the target list field
   --
   function Header_Get_Target_List_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Target_List_Offset);

   -- Set the byte count of the public key url field
   --
   procedure Header_Set_Public_Key_URL_Byte_Count (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Public_Key_URL_Byte_Count);

   -- Return the byte count of the public key url field
   --
   function Header_Get_Public_Key_URL_Byte_Count (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Public_Key_URL_Byte_Count);

   -- Set the offset of the public key url field
   --
   procedure Header_Set_Public_Key_URL_Offset (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Public_Key_URL_Offset);

   -- Return the ofset of the public key url field
   --
   function Header_Get_Public_Key_URL_Offset (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Public_Key_URL_Offset);

   -- Set the block creation timestamp
   --
   procedure Header_Set_Creation_Timestamp (I: in out Instance; Value: Unsigned_64);
   pragma inline (Header_Set_Creation_Timestamp);

   -- Return the block creation timestamp
   --
   function Header_Get_Creation_Timestamp (I: in out Instance) return Unsigned_64;
   pragma inline (Header_Get_Creation_Timestamp);

   -- Set the block modification timestamp
   --
   procedure Header_Set_Modification_Timestamp (I: in out Instance; Value: Unsigned_64);
   pragma inline (Header_Set_Modification_Timestamp);

   -- Return the block modification timestamp
   --
   function Header_Get_Modification_Timestamp (I: in out Instance) return Unsigned_64;
   pragma inline (Header_Get_Modification_Timestamp);

   -- Set the block expiry timestamp
   --
   procedure Header_Set_Expiry_Timestamp (I: in out Instance; Value: Unsigned_64);
   pragma inline (Header_Set_Expiry_Timestamp);

   -- Return the block expiry timestamp
   --
   function Header_Get_Expiry_Timestamp (I: in out Instance) return Unsigned_64;
   pragma inline (Header_Get_Expiry_Timestamp);

   -- Set the reserved field 1 value, first 32 bits.
   --
   procedure Header_Set_Reserved_1a (I: in out Instance; Value: Unsigned_32);
   pragma inline (Header_Set_Reserved_1a);

   -- Return the reserved 1 value, first 32 bits.
   --
   function Header_Get_Reserved_1a (I: in out Instance) return Unsigned_32;
   pragma inline (Header_Get_Reserved_1a);

   -- Set the reserved field 1 value, last 16 bits.
   --
   procedure Header_Set_Reserved_1b (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Reserved_1b);

   -- Return the reserved 1 value, first 16 bits.
   --
   function Header_Get_Reserved_1b (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Reserved_1b);

   -- Set the CRC16 value for the entire block header.
   --
   procedure Header_Set_Block_Header_Crc16 (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Block_Header_Crc16);

   -- Return the CRC16 value for the entire block header.
   --
   function Header_Get_Block_Header_Crc16 (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Block_Header_Crc16);

   -- Update the block header crc value in accordance with the block header contents.
   -- Note: All values in the header must have been set to their final values before calling this operation.
   --
   procedure Header_Update_Header_Crc16 (I: in out Instance) is abstract;

   -- Header access ends
   -- ==========================================================================
   
   
   -- =================================================================
   -- | The following operations are for test-use only. Do not use.   |
   -- | (they are not stable and may have unanticipated side effects) |
   -- =================================================================
   
   -- Undocumented, for testing only, do not use!
   --
   function Test_Serializer (I: in out Instance) return Serializable.Instance;
   
   procedure Set_Synchronization_Byte_1 (I: in out Instance);
   
private
   

   type Instance is abstract new Ada.Finalization.Controlled with record
      Container: BRBON.Container.Instance;
      Memory_Ptr: Array_Of_Unsigned_8_Ptr; -- The Container does not export its pointer, a copy must be kept.
      Start_Of_Header_Field_Storage: Unsigned_16;
      First_Free_Byte_In_Header_Field_Storage: Unsigned_16; -- range self.Start_Of_Header_Field_Storage .. self.Last_Free_Byte_In_header_Field_Storage + 1
      Last_Free_Byte_In_header_Field_Storage: Unsigned_16; -- quasi constant
      Start_Of_Payload: Unsigned_32; -- quasi constant
      First_Free_Byte_In_Payload: Unsigned_32; -- range self.Start_Of_Payload .. self.Last_Free_Byte_In_Payload + 1
      Last_Free_Byte_In_Payload: Unsigned_32; -- will never decrease, may increase for some child classes
   end record;

   
   -- Ensures that all fields in the block and header structure are consistent with the content and size of the block.
   -- This affects things like byte-count(s), CRC-values, footer content and maybe more.
   --
   procedure Ensure_Block_Consistency (I: in out Instance);
   
   
   -- Adds the given string to the header field storage area and returns the offset of the start location.
   -- Returns zero if there is insufficient space available.
   --
   function Add_To_Header_Field (I: in out Instance; Value: String) return Unsigned_16;
   
      
end BRBON.Block;

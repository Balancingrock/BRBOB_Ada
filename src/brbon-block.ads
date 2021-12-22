with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;
with Ada.Strings.Unbounded;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;
with BRBON.Header;
with BRBON.Footer;
with BRBON.Utils;


with Serializable;


package BRBON.Block is
 

   type Instance is abstract new Ada.Finalization.Controlled with private;
      
   
   
   -- The total number of bytes that will be used by the block if it is saved or transferred now.
   --
   function Byte_Count (I: in out Instance) return Unsigned_32;

   
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

   -- Set the reserved field 1 value, first 16 bits.
   -- Note: If the header-byte-count is not set this operation will fail and possibly overwrite
   -- other header or user data. Most likely leading to an invalid block.
   --
   procedure Header_Set_Reserved_1a (I: in out Instance; Value: Unsigned_32);
   pragma inline (Header_Set_Reserved_1a);

   -- Return the reserved 1 value, first 16 bits.
   -- Note: If the header-byte-count is not set this operation will most likely return the wrong value.
   --
   function Header_Get_Reserved_1a (I: in out Instance) return Unsigned_32;
   pragma inline (Header_Get_Reserved_1a);

   -- Set the reserved field 1 value, last 16 bits.
   -- Note: If the header-byte-count is not set this operation will fail and possibly overwrite
   -- other header or user data. Most likely leading to an invalid block.
   --
   procedure Header_Set_Reserved_1b (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Reserved_1b);

   -- Return the reserved 1 value, first 16 bits.
   -- Note: If the header-byte-count is not set this operation will most likely return the wrong value.
   --
   function Header_Get_Reserved_1b (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Reserved_1b);

   -- Set the CRC16 value for the entire block header.
   -- Note: If the header-byte-count is not set this operation will fail and possibly overwrite
   -- other header or user data. Most likely leading to an invalid block.
   --
   procedure Header_Set_Header_Crc16 (I: in out Instance; Value: Unsigned_16);
   pragma inline (Header_Set_Header_Crc16);

   -- Return the CRC16 value for the entire block header.
   -- Note: If the header-byte-count is not set this operation will most likely return the wrong value.
   --
   function Header_Get_Header_Crc16 (I: in out Instance) return Unsigned_16;
   pragma inline (Header_Get_Header_Crc16);

   --
   -- Header access ends
   -- ==========================================================================
   
   
   -- ==========================================================================
   -- API
   -- ==========================================================================
      
   -- Update the header crc value.
   -- Note: Updates to the header after calling this operation will invalidate the CRC.
   --
   procedure Update_Header_CRC (I: in out Instance);
   
   
   -- Update the block CRC.
   -- Note: Any updates to the block after calling this operation will invalidate the CRC.
   --
   procedure Update_Block_CRC (I: in out Instance);
   
   
   -- Set the origin of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   -- This operation has side effects: All strings, byte-counts, offsets and CRC16 values in the field storage
   -- will be updated and the strings may be shifted (in the field storage).
   --
   procedure Set_Origin (I: in out Instance; Value: String);
   
   
   -- Returns the origin of the block as a string.
   -- If no origin is present, an empty string will be returned.
   --
   function Get_Origin (I: in out Instance) return String;
   
   
   -- Set the identifier of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   -- This operation has side effects: All strings, byte-counts, offsets and CRC16 values in the field storage
   -- will be updated and the strings may be shifted (in the field storage).
   --
   procedure Set_Identifier (I: in out Instance; Value: String);
   
   
   -- Returns the identifier of the block as a string.
   -- If no identifier is present, an empty string will be returned.
   --
   function Get_Identifier (I: in out Instance) return String;
   
   
   -- Set the extension of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   -- This operation has side effects: All strings, byte-counts, offsets and CRC16 values in the field storage
   -- will be updated and the strings may be shifted (in the field storage).
   --
   procedure Set_Extension (I: in out Instance; Value: String);
   
   
   -- Returns the extension of the block as a string.
   -- If no extension is present, an empty string will be returned.
   --
   function Get_Extension (I: in out Instance) return String;
   
   
   -- Set the path prefix of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   -- This operation has side effects: All strings, byte-counts, offsets and CRC16 values in the field storage
   -- will be updated and the strings may be shifted (in the field storage).
   --
   procedure Set_Path_Prefix (I: in out Instance; Value: String);
   
   
   -- Returns the path prefix of the block as a string.
   -- If no path prefix is present, an empty string will be returned.
   --
   function Get_Path_Prefix (I: in out Instance) return String;
   
   
   -- Set the acquisition URL of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   procedure Set_Acquisition_URL (I: in out Instance; Value: String);
   
   
   -- Returns the acquisition URL of the block as a string.
   -- If no acquisition URL is present, an empty string will be returned.
   --
   function Get_Acqquisition_URL(I: in out Instance) return String;
   
   
   -- Set the target list of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   -- This operation has side effects: All strings, byte-counts, offsets and CRC16 values in the field storage
   -- will be updated and the strings may be shifted (in the field storage).
   --
   procedure Set_Target_List (I: in out Instance; Value: String);
   
   
   -- Returns the target list of the block as a string.
   -- If no target list is present, an empty string will be returned.
   --
   function Get_Target_List (I: in out Instance) return String;
   
   
   -- Set the public key URL of the block to the specified string.
   -- Will raise the Memory_Error if the string cannot be accomodated.
   --
   -- This operation has side effects: All strings, byte-counts, offsets and CRC16 values in the field storage
   -- will be updated and the strings may be shifted (in the field storage).
   --
   procedure Set_Public_Key_URL (I: in out Instance; Value: String);
   
   
   -- Returns the public key URL of the block as a string.
   -- If no public key URL is present, an empty string will be returned.
   --
   function Get_Public_Key_URL(I: in out Instance) return String;
   
   
   -- Returns the number of free bytes in the field storage.
   --
   function Field_Storage_Free_Bytes (I: in out Instance) return Unsigned_16;
   
   
   -- =================================================================
   -- | The following operations are for test-use only. Do not use.   |
   -- | (they are not stable and may have unanticipated side effects) |
   -- =================================================================
   
   -- Undocumented, for testing only, do not use!
   --
   function Test_Serializer (I: in out Instance) return Serializable.Instance;

   
   -- ==========================================================================
   -- Service operations for child classes of the block
   --
   
   -- Creates a block header in the given block for a single item block.
   -- When the Field_Storage_Byte_Count is 1, it will automatically size the field to contain all the given strings.
   -- Note that a field storage size of 1 is impossible, as it will always be a multiple of 8 bytes.
   --
   -- This is not a BRBON-API call hence no default values are given. Note that all values will be taken as is and
   -- applied without checks. Failure to ensure proper values will result in an invalid block header with possible
   -- end user data lost.
   --
   procedure Create_Single_Item_Block_Header
     (
      In_Block: in out Instance'Class;
      Field_Storage_Byte_Count: Unsigned_16;
      Header_Byte_Count: Unsigned_16;
      Options: Block_Options;
      Origin: String;
      Identifier: String;
      Extension: String;
      Path_Prefix: String;
      Acquisition_URL: String;
      Target_List: String;
      Public_Key_URL: String;
      Creation_Timestamp: Unsigned_64;
      Expiry_Timestamp: Unsigned_64
     );
   
   
private
   
   type Field_Storage_Strings is
      record
         Origin: Ada.Strings.Unbounded.Unbounded_String;
         Identifier: Ada.Strings.Unbounded.Unbounded_String;
         Extension: Ada.Strings.Unbounded.Unbounded_String;
         Path_Prefix: Ada.Strings.Unbounded.Unbounded_String;
         Acquisition_URL: Ada.Strings.Unbounded.Unbounded_String;
         Target_List: Ada.Strings.Unbounded.Unbounded_String;
         Public_Key_URL: Ada.Strings.Unbounded.Unbounded_String;
      end record;
   
   
   type Instance is abstract new Ada.Finalization.Controlled with record
      Container: BRBON.Container.Instance;
      Memory_Ptr: Array_Of_Unsigned_8_Ptr; -- The Container does not export its pointer, a copy must be kept.
      Start_Of_Header_Field_Storage: Unsigned_16;
      Start_Of_Payload: Unsigned_32; -- quasi constant
      First_Free_Byte_In_Payload: Unsigned_32; -- range self.Start_Of_Payload .. self.Last_Free_Byte_In_Payload + 1
      Last_Free_Byte_In_Payload: Unsigned_32; -- will never decrease, may increase for some child classes
   end record;

   
   -- Returns all field storage strings
   --
   function Read_Field_Storage_Strings (I: in out Instance) return Field_Storage_Strings;
   
   
   -- Updates the field storage strings to the new values.
   -- If the field storage is not large enough, a Memory_Error will be raised.
   --
   procedure Write_Field_Storage_Strings (I: in out Instance; Strings: Field_Storage_Strings);
   
   
   -- Returns the offset of the highest used byte in the field-storage.
   --
   -- function Field_Storage_Highest_Used_Byte_Offset (I: in out Instance) return Unsigned_16;
      
   
   -- Ensures that all fields in the block and header structure are consistent with the content and size of the block.
   -- This affects things like byte-count(s), CRC-values, footer content and maybe more.
   --
   procedure Ensure_Block_Consistency (I: in out Instance);
   
   
   type U32_Getter is access function (I: in out Instance) return Unsigned_32;
   
   type U16_Getter is access function (I: in out Instance) return Unsigned_16;
   
   type U8_Getter is access function (I: in out Instance) return Unsigned_8;
   
   type U32_Setter is access procedure (I: in out Instance; Value: Unsigned_32);
   
   type U16_Setter is access procedure (I: in out Instance; Value: Unsigned_16);
   
   type U8_Setter is access procedure (I: in out Instance; Value: Unsigned_8);

   
end BRBON.Block;

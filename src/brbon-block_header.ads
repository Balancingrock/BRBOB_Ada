with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; -- use BRBON.Container;


package BRBON.Block_Header is

   Block_Header_Type_1_Byte_Count: Unsigned_32 := 70;

   -- A block header
   --
   type Block_Header is tagged private;

   -- A block header pointer
   --
   type Block_Header_Ptr is access Block_Header;

   -- The memory area for a block header type 1
   --
   subtype Block_Header_Type_1_Memory_Area is Array_Of_Unsigned_8 (0 .. Block_Header_Type_1_Byte_Count - 1);

   -- The pointer to a memory area for a block header type 1
   --
   type Block_Header_Type_1_Memory_Area_Ptr is access Block_Header_Type_1_Memory_Area;

   -- Create a type 1 block header in the given memory area with the given endianness.
   -- @param Memory_Area_Ptr The memory area where the block header should be constrcuted & maintained.
   -- @param Using_Endianness The endianness to be used in the header.
   -- @return A block header.
   --
   function Block_Header_Type_1_Factory (Memory_Area_Ptr: Block_Header_Type_1_Memory_Area_Ptr; Using_Endianness: Endianness) return Block_Header;

   -- Set the block synchronization header byte 1 to the expected value.
   --
   procedure Set_Synchronization_Byte_1 (H: in out Block_Header'class);
   pragma inline (Set_Synchronization_Byte_1);

   -- Verifiy that the first byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_1 (H: in out Block_Header'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_1);

   -- Set the block synchronization header byte 2 to the expected value.
   --
   procedure Set_Synchronization_Byte_2 (H: in out Block_Header'class);
   pragma inline (Set_Synchronization_Byte_2);

   -- Verifiy that the second byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_2 (H: in out Block_Header'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_2);

   -- Set the block synchronization header byte 3 to the expected value.
   --
   procedure Set_Synchronization_Byte_3 (H: in out Block_Header'class);
   pragma inline (Set_Synchronization_Byte_3);

   -- Verifiy that the thrid byte of the block synchronisation header has the correct value.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_3 (H: in out Block_Header'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_3);

   -- Set the block synchronization header byte 4 to the expected value for the endianness given at creation.
   -- Side effect: Afterwards the endianness can be retrieved with Get_Endianness.
   --
   procedure Set_Synchronization_Byte_4 (H: in out Block_Header'class);
   pragma inline (Set_Synchronization_Byte_4);

   -- Verifiy that the fourth byte of the block synchronisation header has the correct value.
   -- Side effect: If true, the endianness can be retrieved with Get_Endianness.
   -- @return True if the synchronization byte is as expected.
   --
   function Verify_Synchronization_Byte_4 (H: in out Block_Header'class) return Boolean;
   pragma inline (Verify_Synchronization_Byte_4);

   -- Returns the endianness of the block header.
   -- Is only reliable after synchronization byte 4 has been set or verified with success.
   --
   function Get_Endianness (H: in out Block_Header'class) return Endianness;
   pragma inline (Get_Endianness);

   -- Sets the type of the block to the given value.
   --
   procedure Set_Block_Type (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Type);

   -- Returns the raw value of the block type.
   --
   function Get_Block_Type (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Type);

   -- Set the value for the first reserved field in the block header.
   --
   procedure Set_Reserved_1 (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Reserved_1);

   -- Returns the value of the first reserved field in the block header.
   --
   function Get_Reserved_1 (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Reserved_1);

   -- Set the byte count for the complete block.
   -- This value is optional, and initialized to "not used" = 16#FFFF_FFFF#.
   --
   procedure Set_Block_Byte_Count (H: in out Block_Header'class; Value: Unsigned_32);
   pragma inline (Set_Block_Byte_Count);

   -- Returns the value of the block byte count.
   --
   function Get_Block_Byte_Count (H: in out Block_Header'class) return Unsigned_32;
   pragma inline (Get_Block_Byte_Count);

   -- Set the block header byte count.
   --
   procedure Set_Block_Header_Byte_Count (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Header_Byte_Count);

   -- Returns the block header byte count.
   --
   function Get_Block_Header_Byte_Count (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Header_Byte_Count);

   -- Set the block encrypted header byte count.
   --
   procedure Set_Block_Encrypted_Header_Byte_Count (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Encrypted_Header_Byte_Count);

   -- Returns the block encrypted header byte count.
   --
   function Get_Block_Encrypted_Header_Byte_Count (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Encrypted_Header_Byte_Count);

   -- Set the CRC16 value of the origin-field in the block header.
   --
   procedure Set_Block_Origin_Crc16 (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Origin_Crc16);

   -- Returns the CRC16 value of the origin-field in the block header.
   --
   function Get_Block_Origin_Crc16 (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Origin_Crc16);

   -- Set the CRC16 value of the identifier-field in the block header.
   --
   procedure Set_Block_Identifier_Crc16 (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Identifier_Crc16);

   -- Returns the CRC16 value of the identifier-field in the block header.
   --
   function Get_Block_Identifier_Crc16 (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Identifier_Crc16);

   -- Set the CRC16 value of the extension-field in the block header.
   --
   procedure Set_Block_Extension_Crc16 (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Extension_Crc16);

   -- Returns the CRC16 value of the extension-field in the block header.
   --
   function Get_Block_Extension_Crc16 (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Extension_Crc16);

   -- Set the CRC16 value of the path-prefix-field in the block header.
   --
   procedure Set_Block_Path_Prefix_Crc16 (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Path_Prefix_Crc16);

   -- Returns the CRC16 value of the path-prefix-field in the block header.
   --
   function Get_Block_Path_Prefix_Crc16 (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Path_Prefix_Crc16);

   -- Set the byte count of the block-origin field.
   --
   procedure Set_Block_Origin_Byte_Count (H: in out Block_Header'class; Value: Unsigned_8);
   pragma inline (Set_Block_Origin_Byte_Count);

   -- Returns the byte count of the block-origin field.
   --
   function Get_Block_Origin_Byte_Count (H: in out Block_Header'class) return Unsigned_8;
   pragma inline (Get_Block_Origin_Byte_Count);

   -- Set the byte count of the block-identifier field.
   --
   procedure Set_Block_Identifier_Byte_Count (H: in out Block_Header'class; Value: Unsigned_8);
   pragma inline (Set_Block_Identifier_Byte_Count);

   -- Returns the byte count of the block-identifier field.
   --
   function Get_Block_Identifier_Byte_Count (H: in out Block_Header'class) return Unsigned_8;
   pragma inline (Get_Block_Identifier_Byte_Count);

   -- Set the byte count of the block-extension field.
   --
   procedure Set_Block_Extension_Byte_Count (H: in out Block_Header'class; Value: Unsigned_8);
   pragma inline (Set_Block_Extension_Byte_Count);

   -- Returns the byte count of the block-extension field.
   --
   function Get_Block_Extension_Byte_Count (H: in out Block_Header'class) return Unsigned_8;
   pragma inline (Get_Block_Extension_Byte_Count);

   -- Set the byte count of the block-path-prefix field.
   --
   procedure Set_Block_Path_Prefix_Byte_Count (H: in out Block_Header'class; Value: Unsigned_8);
   pragma inline (Set_Block_Path_Prefix_Byte_Count);

   -- Returns the byte count of the block-path-prefix field.
   --
   function Get_Block_Path_Prefix_Byte_Count (H: in out Block_Header'class) return Unsigned_8;
   pragma inline (Get_Block_Path_Prefix_Byte_Count);

   -- Set the offset of the block-origin field.
   --
   procedure Set_Block_Origin_Offset (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Origin_Offset);

   -- Returns the offset of the block-origin field.
   --
   function Get_Block_Origin_Offset (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Origin_Offset);

   -- Set the offset of the block-identifier field.
   --
   procedure Set_Block_Identifier_Offset (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Identifier_Offset);

   -- Returns the offset of the block-identifier field.
   --
   function Get_Block_Identifier_Offset (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Identifier_Offset);

   -- Set the offset of the block-extension field.
   --
   procedure Set_Block_Extension_Offset (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Extension_Offset);

   -- Returns the offset of the block-extension field.
   --
   function Get_Block_Extension_Offset (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Extension_Offset);

   -- Set the offset of the block-path-prefix field.
   --
   procedure Set_Block_Path_Prefix_Offset (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Path_Prefix_Offset);

   -- Returns the offset of the block-path-prefix field.
   --
   function Get_Block_Path_Prefix_Offset (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Path_Prefix_Offset);

   -- Set the value for the first reserved field in the block header.
   --
   procedure Set_Reserved_2 (H: in out Block_Header'class; Value: Unsigned_32);
   pragma inline (Set_Reserved_2);

   -- Returns the value of the first reserved field in the block header.
   --
   function Get_Reserved_2 (H: in out Block_Header'class) return Unsigned_32;
   pragma inline (Get_Reserved_2);

   -- Set the byte count of the target list field
   --
   procedure Set_Block_Target_List_Byte_Count (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Target_List_Byte_Count);

   -- Return the byte count of the target list field
   --
   function Get_Block_Target_List_Byte_Count (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Target_List_Byte_Count);

   -- Set the offset of the target list field
   --
   procedure Set_Block_Target_List_Offset (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Target_List_Offset);

   -- Return the offset of the target list field
   --
   function Get_Block_Target_List_Offset (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Target_List_Offset);

   -- Set the byte count of the public key url field
   --
   procedure Set_Block_Public_Key_URL_Byte_Count (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Public_Key_URL_Byte_Count);

   -- Return the byte count of the public key url field
   --
   function Get_Block_Public_Key_URL_Byte_Count (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Public_Key_URL_Byte_Count);

   -- Set the offset of the public key url field
   --
   procedure Set_Block_Public_Key_URL_Offset (H: in out Block_Header'class; Value: Unsigned_16);
   pragma inline (Set_Block_Public_Key_URL_Offset);

   -- Return the ofset of the public key url field
   --
   function Get_Block_Public_Key_URL_Offset (H: in out Block_Header'class) return Unsigned_16;
   pragma inline (Get_Block_Public_Key_URL_Offset);

   -- Set the block creation timestamp
   --
   procedure Set_Block_Creation_Timestamp (H: in out Block_Header'class; Value: Unsigned_64);
   pragma inline (Set_Block_Creation_Timestamp);

   -- Return the block creation timestamp
   --
   function Get_Block_Creation_Timestamp (H: in out Block_Header'class) return Unsigned_64;
   pragma inline (Get_Block_Creation_Timestamp);

   -- Set the block modification timestamp
   --
   procedure Set_Block_Modification_Timestamp (H: in out Block_Header'class; Value: Unsigned_64);
   pragma inline (Set_Block_Modification_Timestamp);

   -- Return the block modification timestamp
   --
   function Get_Block_Modification_Timestamp (H: in out Block_Header'class) return Unsigned_64;
   pragma inline (Get_Block_Modification_Timestamp);

   -- Set the block expiry timestamp
   --
   procedure Set_Block_Expiry_Timestamp (H: in out Block_Header'class; Value: Unsigned_64);
   pragma inline (Set_Block_Expiry_Timestamp);

   -- Return the block expiry timestamp
   --
   function Get_Block_Expiry_Timestamp (H: in out Block_Header'class) return Unsigned_64;
   pragma inline (Get_Block_Expiry_Timestamp);

   -- Set the reserved field 3 value, first 32 bits.
   --
   procedure Set_Reserved_3a (H: in out Block_Header'class; For_Block_Header_Byte_Count: Unsigned_32; Value: Unsigned_32);
   pragma inline (Set_Reserved_3a);

   -- Return the reserved 3 value, first 32 bits.
   --
   function Get_Reserved_3a (H: in out Block_Header'class; For_Block_Header_Byte_Count: Unsigned_32) return Unsigned_32;
   pragma inline (Get_Reserved_3a);

   -- Set the reserved field 3 value, last 16 bits.
   --
   procedure Set_Reserved_3b (H: in out Block_Header'class; For_Block_Header_Byte_Count: Unsigned_32; Value: Unsigned_16);
   pragma inline (Set_Reserved_3b);

   -- Return the reserved 3 value, first 16 bits.
   --
   function Get_Reserved_3b (H: in out Block_Header'class; For_Block_Header_Byte_Count: Unsigned_32) return Unsigned_16;
   pragma inline (Get_Reserved_3b);

   -- Set the CRC16 value for the entire block header.
   --
   procedure Set_Block_Header_Crc16 (H: in out Block_Header'class; For_Block_Header_Byte_Count: Unsigned_32; Value: Unsigned_16);
   pragma inline (Set_Block_Header_Crc16);

   -- Return the CRC16 value for the entire block header.
   --
   function Get_Block_Header_Crc16 (H: in out Block_Header'class; For_Block_Header_Byte_Count: Unsigned_32) return Unsigned_16;
   pragma inline (Get_Block_Header_Crc16);

private

   type Block_Header is tagged
      record
         Store: BRBON.Container.Store;
         Endianness: BRBON.Types.Endianness; -- Set after verification/assignment of synch byte 4
      end record;

end BRBON.Block_Header;

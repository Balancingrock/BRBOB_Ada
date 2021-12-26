with Interfaces; use Interfaces;

package BRBON.Block.Header is


   -- Returns the byte order (endianness) of the block.
   --
   function Get_Byte_Storage_Order (B: BRBON.Block) return BRBON.Byte_Storage_Order;
   pragma inline (Get_Byte_Storage_Order);


   -- Returns the block type.
   --
   function Get_Type (B: BRBON.Block) return BRBON.Block.Block_Type;
   pragma inline (Get_Type);

   
   -- Set the block options value.
   --
   procedure Set_Options (B: BRBON.Block; Value: BRBON.Block.Block_Options);
   pragma inline (Set_Options);


   -- Returns the block options value.
   --
   function Get_Options (B: BRBON.Block) return BRBON.Block.Block_Options;
   pragma inline (Get_Options);


   -- Returns the value of the block byte count.
   --
   function Get_Byte_Count (B: BRBON.Block) return Unsigned_32;
   pragma inline (Get_Byte_Count);


   -- Returns the block header byte count.
   --
   function Get_Header_Byte_Count (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Header_Byte_Count);


   -- Returns the block encrypted header byte count.
   --
   function Get_Encrypted_Header_Byte_Count (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Encrypted_Header_Byte_Count);


   -- Origin

   -- Sets the value of the origin field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Origin (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the origin field
   -- Returns an empty string if the value is not set.
   --
   function Get_Origin (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the origin-field in the block header.
   --
   function Get_Origin_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Origin_CRC);


   -- Returns the byte count of the origin-field in the block header.
   --
   function Get_Origin_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Origin_Byte_Count);


   -- Identifier
   
   -- Sets the value of the identifier field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Identifier (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the identifier field
   -- Returns an empty string if the value is not set.
   --
   function Get_Identifier (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the identifier-field in the block header.
   --
   function Get_Identifier_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Identifier_CRC);


   -- Returns the byte count of the identifier-field in the block header.
   --
   function Get_Identifier_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Identifier_Byte_Count);


   -- Extension
   
   -- Sets the value of the extension field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Extension (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the extension field
   -- Returns an empty string if the value is not set.
   --
   function Get_Extension (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the extension-field in the block header.
   --
   function Get_Extension_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Extension_CRC);


   -- Returns the byte count of the extension-field in the block header.
   --
   function Get_Extension_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Extension_Byte_Count);


   -- Path prefix
   
   -- Sets the value of the path prefix field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Path_Prefix (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the path prefix field
   -- Returns an empty string if the value is not set.
   --
   function Get_Path_Prefix (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the path-prefix-field in the block header.
   --
   function Get_Path_Prefix_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Path_Prefix_CRC);


   -- Returns the byte count of the path-prefix-field in the block header.
   --
   function Get_Path_Prefix_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Path_Prefix_Byte_Count);



   -- Acquisition URL
   
   -- Sets the value of the acquisition URL field.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Acquisition_URL (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the acquisition URL field
   -- Returns an empty string if the value is not set.
   --
   function Get_Acquisition_URL (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the acquisition_URL-field in the block header.
   --
   function Get_Acquisition_URL_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Acquisition_URL_CRC);


   -- Returns the byte count of the acquisition_URL-field in the block header.
   --
   function Get_Acquisition_URL_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Acquisition_URL_Byte_Count);


   -- Target List
   
   -- Sets the value of the target list field.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Extension (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the extension field
   -- Returns an empty string if the value is not set.
   --
   function Get_Extension (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the extension-field in the block header.
   --
   function Get_Extension_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Extension_CRC);


   -- Returns the byte count of the extension-field in the block header.
   --
   function Get_Extension_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Extension_Byte_Count);


   -- Public Key URL
   
   -- Sets the value of the public key URL field.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Public_Key_URL (B: BRBON.Block; Value: String);
   
   
   -- Returns the value of the public key URL field
   -- Returns an empty string if the value is not set.
   --
   function Get_Public_Key_URL (B: BRBON.Block) return String;
   

   -- Returns the CRC (ARC) value of the Public_Key_URL-field in the block header.
   --
   function Get_Public_Key_URL_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Public_Key_URL_CRC);


   -- Returns the byte count of the Public_Key_URL-field in the block header.
   --
   function Get_Public_Key_URL_Byte_Count (B: BRBON.Block) return Unsigned_8;
   pragma inline (Get_Public_Key_URL_Byte_Count);


   -- Set the block creation timestamp
   --
   procedure Set_Creation_Timestamp (B: BRBON.Block; Value: Unsigned_64);
   pragma inline (Set_Creation_Timestamp);


   -- Return the block creation timestamp
   --
   function Get_Creation_Timestamp (B: BRBON.Block) return Unsigned_64;
   pragma inline (Get_Creation_Timestamp);


   -- Set the block modification timestamp
   --
   procedure Set_Modification_Timestamp (B: BRBON.Block; Value: Unsigned_64);
   pragma inline (Set_Modification_Timestamp);


   -- Return the block modification timestamp
   --
   function Get_Modification_Timestamp (B: BRBON.Block) return Unsigned_64;
   pragma inline (Get_Modification_Timestamp);


   -- Set the block expiry timestamp
   --
   procedure Set_Expiry_Timestamp (B: BRBON.Block; Value: Unsigned_64);
   pragma inline (Set_Expiry_Timestamp);


   -- Return the block expiry timestamp
   --
   function Get_Expiry_Timestamp (B: BRBON.Block) return Unsigned_64;
   pragma inline (Get_Expiry_Timestamp);


   -- Return the stored CRC (ARC) value for the complete block header.
   --
   function Get_Header_CRC (B: BRBON.Block) return Unsigned_16;
   pragma inline (Get_Header_CRC);

         
   -- Update the header CRC value.
   -- Note: Updates to the header after calling this operation will invalidate the CRC.
   --
   procedure Update_Header_CRC (B: BRBON.Block);
   
   
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
         Is_Type: Types.Block_Type;                -- 2 bytes
         Options: Types.Block_Options;             -- 2 bytes

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


   -- Support type to update the strings in the storage field of the header.
   --
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


   -- The byte count for the field storage strings
   --
   function Byte_Count (FFS: Field_Storage_Strings) return Unsigned_16;
   

   -- Returns all field storage strings
   --
   function Read_Field_Storage_Strings (FFS: Field_Storage_Strings) return Field_Storage_Strings;
   
   
   -- Updates the field storage strings to the new values.
   -- If the field storage is not large enough, a Memory_Error will be raised.
   --
   procedure Write_Field_Storage_Strings (FFS: Field_Storage_Strings; Strings: Field_Storage_Strings);
         
   
   -- Expected synchronization values
   --
   Synch_Byte_1_Expected_Value:           constant Unsigned_8 := 16#96#;
   Synch_Byte_2_Expected_Value:           constant Unsigned_8 := 16#7F#;
   Synch_Byte_3_Expected_Value:           constant Unsigned_8 := 16#81#;
   Synch_Byte_4_LSB_First_Expected_Value: constant Unsigned_8 := 16#5A#;
   Synch_Byte_4_MSB_First_Expected_Value: constant Unsigned_8 := 16#A5#;


end BRBON.Header_Package;

with Interfaces; use Interfaces;

with Serializable;

with BRBON.Container_Package;


package BRBON.Container_Package.Block_Package is
   
   
   -- The block type
   --
   type Block is new Container with private;
   
   
   -- A pointer to a block
   --
   type Block_Ptr is access Block;
   
   
   -- Create a new block in the given memory area.
   --
   function Factory (Memory_Ptr: Unsigned_8_Array_Ptr; Byte_Order: Byte_Storage_Order) return Block;
   
   
   -- Create a new block of the given size.
   --
   function Factory (Byte_Count: Unsigned_32; Byte_Order: Byte_Storage_Order) return Block;
   
   
   -- Create a new block with the content from the referenced file.
   -- Will raise an exception if the file does not contain a BRBON block.
   --
   function Factory (Filepath: String) return Block;
   
   
   -- Returns the byte order (endianness) of the block.
   --
   function Get_Byte_Storage_Order (B: Block) return Byte_Storage_Order;
   pragma inline (Get_Byte_Storage_Order);


   -- Returns the block type.
   --
   function Get_Block_Type (B: Block) return Block_Type;
   pragma inline (Get_Block_Type);


   -- Set the block options value.
   --
   procedure Set_Block_Options (B: Block; Value: Block_Options);
   pragma inline (Set_Block_Options);


   -- Returns the block options value.
   --
   function Get_Block_Options (B: Block) return Block_Options;
   pragma inline (Get_Block_Options);


   -- Returns the value of the block byte count.
   --
   function Get_Block_Byte_Count (B: Block) return Unsigned_32;
   pragma inline (Get_Block_Byte_Count);


   -- Returns the block header byte count.
   --
   function Get_Header_Byte_Count (B: Block) return Unsigned_16;
   pragma inline (Get_Header_Byte_Count);


   -- Returns the block encrypted header byte count.
   --
   function Get_Encrypted_Header_Byte_Count (B: Block) return Unsigned_16;
   pragma inline (Get_Encrypted_Header_Byte_Count);


   -- Origin

   -- Sets the value of the origin field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Origin (B: Block; Value: String);


   -- Returns the value of the origin field
   -- Returns an empty string if the value is not set.
   --
   function Get_Origin (B: Block) return String;


   -- Returns the CRC (ARC) value of the origin-field in the block header.
   --
   function Get_Origin_CRC (B: Block) return Unsigned_16;
   pragma inline (Get_Origin_CRC);


   -- Returns the byte count of the origin-field in the block header.
   --
   function Get_Origin_Byte_Count (B: Block) return Unsigned_8;
   pragma inline (Get_Origin_Byte_Count);


   -- Identifier

   -- Sets the value of the identifier field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Identifier (B: Block; Value: String);


   -- Returns the value of the identifier field
   -- Returns an empty string if the value is not set.
   --
   function Get_Identifier (B: Block) return String;


   -- Returns the CRC (ARC) value of the identifier-field in the block header.
   --
   function Get_Identifier_CRC (B: Block) return Unsigned_16;
   pragma inline (Get_Identifier_CRC);


   -- Returns the byte count of the identifier-field in the block header.
   --
   function Get_Identifier_Byte_Count (B: Block) return Unsigned_8;
   pragma inline (Get_Identifier_Byte_Count);


   -- Extension

   -- Sets the value of the extension field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Extension (B: Block; Value: String);


   -- Returns the value of the extension field
   -- Returns an empty string if the value is not set.
   --
   function Get_Extension (B: Block) return String;


   -- Returns the CRC (ARC) value of the extension-field in the block header.
   --
   function Get_Extension_CRC (B: Block) return Unsigned_16;
   pragma inline (Get_Extension_CRC);


   -- Returns the byte count of the extension-field in the block header.
   --
   function Get_Extension_Byte_Count (B: Block) return Unsigned_8;
   pragma inline (Get_Extension_Byte_Count);


   -- Path prefix

   -- Sets the value of the path prefix field.
   -- Raises the String_To_Long error if there are more than 255 characters.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Path_Prefix (B: Block; Value: String);


   -- Returns the value of the path prefix field
   -- Returns an empty string if the value is not set.
   --
   function Get_Path_Prefix (B: Block) return String;


   -- Returns the CRC (ARC) value of the path-prefix-field in the block header.
   --
   function Get_Path_Prefix_CRC (B: Block) return Unsigned_16;
   pragma inline (Get_Path_Prefix_CRC);


   -- Returns the byte count of the path-prefix-field in the block header.
   --
   function Get_Path_Prefix_Byte_Count (B: Block) return Unsigned_8;
   pragma inline (Get_Path_Prefix_Byte_Count);


   -- Acquisition URL

   -- Sets the value of the acquisition URL field.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Acquisition_URL (B: Block; Value: String);


   -- Returns the value of the acquisition URL field
   -- Returns an empty string if the value is not set.
   --
   function Get_Acquisition_URL (B: Block) return String;


   -- Returns the byte count of the acquisition_URL-field in the block header.
   --
   function Get_Acquisition_URL_Byte_Count (B: Block) return Unsigned_16;
   pragma inline (Get_Acquisition_URL_Byte_Count);


   -- Target List

   -- Sets the value of the target list field.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Target_List (B: Block; Value: String);


   -- Returns the value of the extension field
   -- Returns an empty string if the value is not set.
   --
   function Get_Target_List (B: Block) return String;


   -- Returns the byte count of the extension-field in the block header.
   --
   function Get_Target_List_Byte_Count (B: Block) return Unsigned_16;
   pragma inline (Get_Target_List_Byte_Count);


   -- Public Key URL

   -- Sets the value of the public key URL field.
   -- Raises the Header_Field_Error if the value cannot be fitted into the header field.
   --
   procedure Set_Public_Key_URL (B: Block; Value: String);


   -- Returns the value of the public key URL field
   -- Returns an empty string if the value is not set.
   --
   function Get_Public_Key_URL (B: Block) return String;


   -- Returns the byte count of the Public_Key_URL-field in the block header.
   --
   function Get_Public_Key_URL_Byte_Count (B: Block) return Unsigned_16;
   pragma inline (Get_Public_Key_URL_Byte_Count);


   -- Set the block creation timestamp
   --
   procedure Set_Creation_Timestamp (B: Block; Value: Timestamp);
   pragma inline (Set_Creation_Timestamp);


   -- Return the block creation timestamp
   --
   function Get_Creation_Timestamp (B: Block) return Timestamp;
   pragma inline (Get_Creation_Timestamp);


   -- Set the block modification timestamp
   --
   procedure Set_Modification_Timestamp (B: Block; Value: Timestamp);
   pragma inline (Set_Modification_Timestamp);


   -- Return the block modification timestamp
   --
   function Get_Modification_Timestamp (B: Block) return Timestamp;
   pragma inline (Get_Modification_Timestamp);


   -- Set the block expiry timestamp
   --
   procedure Set_Expiry_Timestamp (B: Block; Value: Timestamp);
   pragma inline (Set_Expiry_Timestamp);


   -- Return the block expiry timestamp
   --
   function Get_Expiry_Timestamp (B: Block) return Timestamp;
   pragma inline (Get_Expiry_Timestamp);


   -- Return the stored CRC (ARC) value for the complete block header.
   --
   function Get_Header_CRC (B: Block) return Unsigned_16;
   pragma inline (Get_Header_CRC);


   -- Update the header CRC value.
   -- Note: Updates to the header after calling this operation will invalidate the CRC.
   --
   procedure Update_Header_CRC (B: Block);


private
   
   
   -- The block descriptor
   --
   type Block is new Container with
      record
         Start_Of_Header_Field_Storage: Unsigned_16 := 0; -- constant (for now)
         Start_Of_Payload: Unsigned_32 := 0; -- quasi constant
         First_Free_Byte_In_Payload: Unsigned_32 := 0; -- range self.Start_Of_Payload .. self.Last_Free_Byte_In_Payload + 1
         Last_Free_Byte_In_Payload: Unsigned_32 := 0; -- will never decrease, may increase for some child classes
      end record;
   

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

         --
         
         Block_Byte_Count: Unsigned_32;
         
         Header_Byte_Count: Unsigned_16;
         Encrypted_Header_Byte_Count: Unsigned_16;

         --
         
         Origin_CRC: Unsigned_16;
         Identifier_CRC: Unsigned_16;
         
         Extension_CRC: Unsigned_16;
         Path_Prefix_CRC: Unsigned_16;

         --
         
         Origin_Byte_Count: Unsigned_8;
         Identifier_Byte_Count: Unsigned_8;
         Extension_Byte_Count: Unsigned_8;
         Path_Prefix_Byte_Count: Unsigned_8;
         
         Origin_Offset: Unsigned_16;
         Identifier_Offset: Unsigned_16;

         --
         
         Extension_Offset: Unsigned_16;
         Path_Prefix_Offset: Unsigned_16;
         
         Acquisition_URL_Byte_Count: Unsigned_16;
         Acquisition_URL_Offset: Unsigned_16;

         --
         
         Target_List_Byte_Count: Unsigned_16;
         Target_List_Offset: Unsigned_16;
         
         Public_Key_URL_Byte_Count: Unsigned_16;
         Public_Key_URL_Offset: Unsigned_16;

         --
         
         Creation_Timestamp: Timestamp;
         Modification_Timestamp: Timestamp;
         Expiry_Timestamp: Timestamp;
      end record;

--   for Block_Header_Leading'Size use Block_Header_Leading_Byte_Count * 8;

   for Block_Header_Leading use
      record
         Synchronization_Byte_1       at  0 range 0..7;
         Synchronization_Byte_2       at  1 range 0..7;
         Synchronization_Byte_3       at  2 range 0..7;
         Synchronization_Byte_4       at  3 range 0..7;
         Is_Type                      at  4 range 0..15;
         Options                      at  6 range 0..15;

         Block_Byte_Count             at  8 range 0..31;
         Header_Byte_Count            at 12 range 0..15;
         Encrypted_Header_Byte_Count  at 14 range 0..15;

         Origin_CRC                   at 16 range 0..15;
         Identifier_CRC               at 18 range 0..15;
         Extension_CRC                at 20 range 0..15;
         Path_Prefix_CRC              at 22 range 0..15;

         Origin_Byte_Count            at 24 range 0..7;
         Identifier_Byte_Count        at 25 range 0..7;
         Extension_Byte_Count         at 26 range 0..7;
         Path_Prefix_Byte_Count       at 27 range 0..7;
         Origin_Offset                at 28 range 0..15;
         Identifier_Offset            at 30 range 0..15;

         Extension_Offset             at 32 range 0..15;
         Path_Prefix_Offset           at 34 range 0..15;
         Acquisition_URL_Byte_Count   at 36 range 0..15;
         Acquisition_URL_Offset       at 38 range 0..15;

         Target_List_Byte_Count       at 40 range 0..15;
         Target_List_Offset           at 42 range 0..15;
         Public_Key_URL_Byte_Count    at 44 range 0..15;
         Public_Key_URL_Offset        at 46 range 0..15;

         Creation_Timestamp           at 48 range 0..63;
         Modification_Timestamp       at 64 range 0..63;
         Expiry_Timestamp             at 80 range 0..63;
      end record;

   Block_Header_Leading_Byte_Count: constant Unsigned_16 := Block_Header_Leading'Size / 8;
   
   type Block_Header_Leading_Ptr is access Block_Header_Leading;

   function To_Block_Header_Leading_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Block_Header_Leading_Ptr);

   function Get_Block_Header_Leading_Ptr (B: Block) return Block_Header_Leading_Ptr is ( To_Block_Header_Leading_Ptr (B.MPtr (0)'Access));
   pragma inline (Get_Block_Header_Leading_Ptr);
   
   Start_Of_Header_Field_Storage: constant array (Block_Type) of Unsigned_16 := (0, Block_Header_Leading_Byte_Count);
   

   -- Private header access function
   
   procedure Set_Block_Type (B: Block; Value: BRBON.Block_Type);
   
   function Get_Origin_Offset (B: Block) return Unsigned_16;
   procedure Set_Origin_Offset (B: Block; Value: Unsigned_16);

   procedure Set_Origin_Byte_Count (B: Block; Value: Unsigned_8);
   procedure Set_Origin_CRC (B: Block; Value: Unsigned_16);
   
   function Get_Extension_Offset (B: Block) return Unsigned_16;
   procedure Set_Extension_Offset (B: Block; Value: Unsigned_16);

   procedure Set_Extension_Byte_Count (B: Block; Value: Unsigned_8);
   procedure Set_Extension_CRC (B: Block; Value: Unsigned_16);
   
   function Get_Identifier_Offset (B: Block) return Unsigned_16;
   procedure Set_Identifier_Offset (B: Block; Value: Unsigned_16);
   
   procedure Set_Identifier_Byte_Count (B: Block; Value: Unsigned_8);
   procedure Set_Identifier_CRC (B: Block; Value: Unsigned_16);
   
   function Get_Path_Prefix_Offset (B: Block) return Unsigned_16;
   procedure Set_Path_Prefix_Offset (B: Block; Value: Unsigned_16);
   
   procedure Set_Path_Prefix_Byte_Count (B: Block; Value: Unsigned_8);
   procedure Set_Path_Prefix_CRC (B: Block; Value: Unsigned_16);
   
   function Get_Acquisition_URL_Offset (B: Block) return Unsigned_16;
   procedure Set_Acquisition_URL_Offset (B: Block; Value: Unsigned_16);
   
   procedure Set_Acquisition_URL_Byte_Count (B: Block; Value: Unsigned_16);
   
   function Get_Target_List_Offset (B: Block) return Unsigned_16;
   procedure Set_Target_List_Offset (B: Block; Value: Unsigned_16);
   
   procedure Set_Target_List_Byte_Count (B: Block; Value: Unsigned_16);
   
   function Get_Public_Key_URL_Offset (B: Block) return Unsigned_16;
   procedure Set_Public_Key_URL_Offset (B: Block; Value: Unsigned_16);
   
   procedure Set_Public_Key_URL_Byte_Count (B: Block; Value: Unsigned_16);
   
   
   -- Initialises a type 1 block (single item).
   -- Note that the type 1 block is very much the default header structure without any additional data.
   --
   procedure Setup (B: Block; For_Byte_Storage_Order: Byte_Storage_Order; With_Field_Storage_Byte_Count: Unsigned_16);
   
   procedure Create_Single_Item_Block_Header
     (
      In_Block: Block;
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
      Creation_Timestamp: Timestamp;
      Expiry_Timestamp: Timestamp
     );
      
   
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

   function To_Block_Header_Trailing_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Block_Header_Trailing_Ptr);

   function Get_Block_Header_Trailing_Ptr (B: Block) return Block_Header_Trailing_Ptr is (To_Block_Header_Trailing_Ptr (B.MPtr (Unsigned_32 (B.Get_Header_Byte_Count - Block_Header_Trailing_Byte_Count))'Access));

   function Get_Block_Header_Trailing_Reserved_1 (B: Block) return Unsigned_32;
   function Get_Block_Header_Trailing_Reserved_2 (B: Block) return Unsigned_16;
   function Get_Block_Header_CRC (B: Block) return Unsigned_16;
   procedure Set_Block_Header_Trailing_Reserved_1 (B: Block; Value: Unsigned_32);
   procedure Set_Block_Header_Trailing_Reserved_2 (B: Block; Value: Unsigned_16);
   procedure Set_Block_Header_CRC (B: Block; Value: Unsigned_16);

      
   -- Footer
   
   type Block_Footer is
      record
         Reserved: Unsigned_32;
         CRC: Unsigned_32;
      end record;
   
   for Block_Footer'Size use (4 + 4) * 8;
   
   for Block_Footer use
      record
         Reserved at 0 range 0..31;
         CRC at 4 range 0..31;
      end record;
   
   type Block_Footer_Ptr is access Block_Footer;
   
   function To_Block_Footer_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Block_Footer_Ptr);
   
   Block_Footer_Byte_Count: Array (Block_Type) of Unsigned_32 :=
     (
      Illegal => 0,
      Single_Item_Block => 8
     );
   
   function Get_Block_Footer_Ptr (B: Block) return Block_Footer_Ptr is (To_Block_Footer_Ptr (B.MPtr (Unsigned_32 (B.Get_Block_Byte_Count - Block_Footer_Byte_Count (B.Get_Block_Type)))'Access));
   
   function Get_Block_Footer_Reserved (B: Block) return Unsigned_32;
   function Get_Block_Footer_CRC (B: Block) return Unsigned_32;
   procedure Set_Block_Footer_Reserved (B: Block; Value: Unsigned_32);
   procedure Set_Block_Footer_CRC (B: Block; Value: Unsigned_32);
   
   
   -- Signatures

   type U32_Getter is access function (B: Block) return Unsigned_32;
   
   type U16_Getter is access function (B: Block) return Unsigned_16;
   
   type U8_Getter is access function (B: Block) return Unsigned_8;
   
   type U32_Setter is access procedure (B: Block; Value: Unsigned_32);
   
   type U16_Setter is access procedure (B: Block; Value: Unsigned_16);
   
   type U8_Setter is access procedure (B: Block; Value: Unsigned_8);

      
   -- Internal access and construction operations of/for the header
   --
   procedure Set_Header_Synchronization_Bytes (B: Block);
   
   
   -- =================================================================
   -- | The following operations are for test-use only. Do not use.   |
   -- | (they are not stable and may have unanticipated side effects) |
   -- =================================================================
   
   -- Undocumented, for testing only, do not use!
   --
   function Test_Support_Serializer (B: Block) return Serializable.Instance;


end BRBON.Container_Package.Block_Package;

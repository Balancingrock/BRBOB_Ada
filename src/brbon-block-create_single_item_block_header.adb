with BRBON.Block; use BRBON.Block;

separate (BRBON.Block)

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
  ) is

   B: Instance'Class renames In_Block;

   FS: Field_Storage_Strings :=
     (
      Ada.Strings.Unbounded.To_Unbounded_String (Origin),
      Ada.Strings.Unbounded.To_Unbounded_String (Identifier),
      Ada.Strings.Unbounded.To_Unbounded_String (Extension),
      Ada.Strings.Unbounded.To_Unbounded_String (Path_Prefix),
      Ada.Strings.Unbounded.To_Unbounded_String (Acquisition_URL),
      Ada.Strings.Unbounded.To_Unbounded_String (Target_List),
      Ada.Strings.Unbounded.To_Unbounded_String (Public_Key_URL)
     );

begin

   B.Header_Set_Synchronization_Byte_1;
   B.Header_Set_Synchronization_Byte_1;
   B.Header_Set_Synchronization_Byte_2;
   B.Header_Set_Synchronization_Byte_3;
   B.Header_Set_Synchronization_Byte_4; -- Assumes the endianness is already set in the container
   B.Header_Set_Type (Types.Single_Item);
   B.Header_Set_Options (Options);

   B.Header_Set_Byte_Count (B.Container.Byte_Count);
   B.Header_Set_Header_Byte_Count (Header_Byte_Count); -- Type 1 does not use the type dependent header or the field storage
   B.Header_Set_Encrypted_Header_Byte_Count (0);

   B.Write_Field_Storage_Strings (FS);

   B.Header_Set_Creation_Timestamp (Creation_Timestamp);
   B.Header_Set_Modification_Timestamp (Creation_Timestamp);
   B.Header_Set_Expiry_Timestamp (Expiry_Timestamp);

   B.Header_Set_Reserved_1a (0);
   B.Header_Set_Reserved_1b (0);
   B.Header_Update_Header_Crc16;


end Create_Single_Item_Block_Header;

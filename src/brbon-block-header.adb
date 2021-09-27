with Crc_Package;

package body BRBON.Block.Header is

   Block_Synchronization_Byte_1_Offset:      constant Unsigned_32 := 16#00#; -- 1 byte
   Block_Synchronization_Byte_2_Offset:      constant Unsigned_32 := 16#01#; -- 1 byte
   Block_Synchronization_Byte_3_Offset:      constant Unsigned_32 := 16#02#; -- 1 byte
   Block_Synchronization_Byte_4_Offset:      constant Unsigned_32 := 16#03#; -- 1 byte
   Block_Type_Offset:                        constant Unsigned_32 := 16#04#; -- 2 bytes
   Block_Options_Offset:                     constant Unsigned_32 := 16#06#; -- 2 bytes

   Block_Byte_Count_Offset:                  constant Unsigned_32 := 16#08#; -- 4 bytes
   Block_Header_Byte_Count_Offset:           constant Unsigned_32 := 16#0C#; -- 2 Bytes
   Block_Encrypted_Header_Byte_Count_Offset: constant Unsigned_32 := 16#0E#; -- 2 Bytes

   Block_Origin_CRC16_Offset:                constant Unsigned_32 := 16#10#; -- 2 Bytes
   Block_Identifier_CRC16_Offset:            constant Unsigned_32 := 16#12#; -- 2 Bytes
   Block_Extension_CRC16_Offset:             constant Unsigned_32 := 16#14#; -- 2 Bytes
   Block_Path_Prefix_CRC16_Offset:           constant Unsigned_32 := 16#16#; -- 2 Bytes

   Block_Origin_Byte_Count_Offset:           constant Unsigned_32 := 16#18#; -- 1 Byte
   Block_Identifier_Byte_Count_Offset:       constant Unsigned_32 := 16#19#; -- 1 Byte
   Block_Extension_Byte_Count_Offset:        constant Unsigned_32 := 16#1A#; -- 1 Byte
   Block_Path_Prefix_Byte_Count_Offset:      constant Unsigned_32 := 16#1B#; -- 1 Byte
   Block_Origin_Offset_Offset:               constant Unsigned_32 := 16#1C#; -- 2 Bytes
   Block_Identifier_Offset_Offset:           constant Unsigned_32 := 16#1E#; -- 2 Bytes

   Block_Extension_Offset_Offset:            constant Unsigned_32 := 16#20#; -- 2 Bytes
   Block_Path_Prefix_Offset_Offset:          constant Unsigned_32 := 16#22#; -- 2 Bytes
   Block_Acquisition_URL_Byte_Count_Offset:  constant Unsigned_32 := 16#24#; -- 2 Bytes
   Block_Acquisition_URL_Offset_Offset:      constant Unsigned_32 := 16#26#; -- 2 Bytes

   Block_Target_List_Byte_Count_Offset:      constant Unsigned_32 := 16#28#; -- 2 Bytes
   Block_Target_List_Offset_Offset:          constant Unsigned_32 := 16#2A#; -- 2 Bytes
   Block_Public_Key_URL_Byte_Count_Offset:   constant Unsigned_32 := 16#2C#; -- 2 Bytes
   Block_Public_Key_URL_Offset_Offset:       constant Unsigned_32 := 16#2E#; -- 2 Bytes

   Block_Creation_Timestamp_Offset:          constant Unsigned_32 := 16#30#; -- 8 Bytes
   Block_Modification_Timestamp_Offset:      constant Unsigned_32 := 16#38#; -- 8 Bytes
   Block_Expiry_Timestamp_Offset:            constant Unsigned_32 := 16#40#; -- 8 Bytes

   Block_Type_Dependant_Header_Offset:       constant Unsigned_32 := 16#48#; -- N * 8 Bytes

   Block_Header_Field_Storage_Type_1_Offset: constant Unsigned_32 := 16#48#; -- M * 8 Bytes

   Block_Reserved_1a_Offset:                 constant Unsigned_32 := 8;
   Block_Reserved_1b_Offset:                 constant Unsigned_32 := 4;
   Block_Header_CRC16_Offset:                constant Unsigned_32 := 2;

   Block_Synchronization_Byte_1_Expected_Value: constant Unsigned_8 := 16#96#;
   Block_Synchronization_Byte_2_Expected_Value: constant Unsigned_8 := 16#7F#;
   Block_Synchronization_Byte_3_Expected_Value: constant Unsigned_8 := 16#81#;
   Block_Synchronization_Byte_4_Little_Endian_Expected_Value: constant Unsigned_8 := 16#5A#;
   Block_Synchronization_Byte_4_Big_Endian_Expected_Value: constant Unsigned_8 := 16#A5#;


   procedure Set_Synchronization_Byte_1 (H: in out Instance'class) is
   begin
      H.Store.Set_Unsigned_8 (Block_Synchronization_Byte_1_Offset, Block_Synchronization_Byte_1_Expected_Value);
   end Set_Synchronization_Byte_1;


   function Verify_Synchronization_Byte_1 (H: in out Instance'class) return Boolean is
   begin
      return H.Store.Get_Unsigned_8 (Block_Synchronization_Byte_1_Offset) = Block_Synchronization_Byte_1_Expected_Value;
   end Verify_Synchronization_Byte_1;


   procedure Set_Synchronization_Byte_2 (H: in out Instance'class) is
   begin
      H.Store.Set_Unsigned_8 (Block_Synchronization_Byte_2_Offset, Block_Synchronization_Byte_2_Expected_Value);
   end Set_Synchronization_Byte_2;


   function Verify_Synchronization_Byte_2 (H: in out Instance'class) return Boolean is
   begin
      return H.Store.Get_Unsigned_8 (Block_Synchronization_Byte_2_Offset) = Block_Synchronization_Byte_2_Expected_Value;
   end Verify_Synchronization_Byte_2;


   procedure Set_Synchronization_Byte_3 (H: in out Instance'class) is
   begin
      H.Store.Set_Unsigned_8 (Block_Synchronization_Byte_3_Offset, Block_Synchronization_Byte_3_Expected_Value);
   end Set_Synchronization_Byte_3;


   function Verify_Synchronization_Byte_3 (H: in out Instance'class) return Boolean is
   begin
      return H.Store.Get_Unsigned_8 (Block_Synchronization_Byte_3_Offset) = Block_Synchronization_Byte_3_Expected_Value;
   end Verify_Synchronization_Byte_3;


   procedure Set_Synchronization_Byte_4 (H: in out Instance'class) is
   begin
      if H.Endianness = Little then
         H.Store.Set_Unsigned_8 (Block_Synchronization_Byte_4_Offset, Block_Synchronization_Byte_4_Little_Endian_Expected_Value);
      else
         H.Store.Set_Unsigned_8 (Block_Synchronization_Byte_4_Offset, Block_Synchronization_Byte_4_Big_Endian_Expected_Value);
      end if;
   end Set_Synchronization_Byte_4;


   function Verify_Synchronization_Byte_4 (H: in out Instance'class) return Boolean is
   begin
      if H.Store.Get_Unsigned_8 (Block_Synchronization_Byte_4_Offset) = Block_Synchronization_Byte_4_Little_Endian_Expected_Value then
         H.Endianness := Little;
         return true;
      elsif H.Store.Get_Unsigned_8 (Block_Synchronization_Byte_4_Offset) = Block_Synchronization_Byte_4_Big_Endian_Expected_Value then
         H.Endianness := Big;
         return true;
      else
         return false;
      end if;
   end Verify_Synchronization_Byte_4;


   function Get_Endianness (H: in out Instance'class) return Endianness is
   begin
      return H.Endianness;
   end Get_Endianness;


   function Verify_Synchronization_Bytes (H: in out Instance'class) return Boolean is
   begin
      --if Verify_Synchronization_Byte_1 (H) = false then return false; end if;
      if Verify_Synchronization_Byte_2 (H) = false then return false; end if;
      if Verify_Synchronization_Byte_3 (H) = false then return false; end if;
      return Verify_Synchronization_Byte_4 (H);
   end Verify_Synchronization_Bytes;


   procedure Set_Block_Type (H: in out Instance'class; Value: Block.Instance_Type) is
   begin
      H.Store.Set_Unsigned_16 (Block_Type_Offset, To_Unsigned_16 (Value));
   end Set_Block_Type;


   function Get_Block_Type (H: in out Instance'class) return Block.Instance_Type is
   begin
      return To_Block_Instance_Type (H.Store.Get_Unsigned_16 (Block_Type_Offset));
   end Get_Block_Type;


   procedure Set_Block_Options (H: in out Instance'class; Value: Options) is
   begin
      H.Store.Set_Unsigned_16 (Block_Options_Offset, To_Unsigned_16 (Value));
   end Set_Block_Options;


   function Get_Block_Options (H: in out Instance'class) return Options is
   begin
      return To_Options (H.Store.Get_Unsigned_16 (Block_Options_Offset));
   end Get_Block_Options;


   procedure Set_Block_Byte_Count (H: in out Instance'class; Value: Unsigned_32) is
   begin
      H.Store.Set_Unsigned_32 (Block_Byte_Count_Offset, Value);
   end Set_Block_Byte_Count;


   function Get_Block_Byte_Count (H: in out Instance'class) return Unsigned_32 is
   begin
      return H.Store.Get_Unsigned_32 (Block_Byte_Count_Offset);
   end Get_Block_Byte_Count;


   procedure Set_Block_Header_Byte_Count (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Header_Byte_Count_Offset, Value);
   end Set_Block_Header_Byte_Count;


   function Get_Block_Header_Byte_Count (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Header_Byte_Count_Offset);
   end Get_Block_Header_Byte_Count;


   procedure Set_Block_Encrypted_Header_Byte_Count (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Encrypted_Header_Byte_Count_Offset, Value);
   end Set_Block_Encrypted_Header_Byte_Count;


   function Get_Block_Encrypted_Header_Byte_Count (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Encrypted_Header_Byte_Count_Offset);
   end Get_Block_Encrypted_Header_Byte_Count;


   procedure Set_Block_Origin_Crc16 (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Origin_CRC16_Offset, Value);
   end Set_Block_Origin_Crc16;


   function Get_Block_Origin_Crc16 (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Origin_CRC16_Offset);
   end Get_Block_Origin_Crc16;


   procedure Set_Block_Identifier_Crc16 (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Identifier_CRC16_Offset, Value);
   end Set_Block_Identifier_Crc16;


   function Get_Block_Identifier_Crc16 (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Identifier_CRC16_Offset);
   end Get_Block_Identifier_Crc16;


   procedure Set_Block_Extension_Crc16 (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Extension_CRC16_Offset, Value);
   end Set_Block_Extension_Crc16;


   function Get_Block_Extension_Crc16 (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Extension_CRC16_Offset);
   end Get_Block_Extension_Crc16;


   procedure Set_Block_Path_Prefix_Crc16 (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Path_Prefix_CRC16_Offset, Value);
   end Set_Block_Path_Prefix_Crc16;


   function Get_Block_Path_Prefix_Crc16 (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Path_Prefix_CRC16_Offset);
   end Get_Block_Path_Prefix_Crc16;


   procedure Set_Block_Origin_Byte_Count (H: in out Instance'class; Value: Unsigned_8) is
   begin
      H.Store.Set_Unsigned_8 (Block_Origin_Byte_Count_Offset, Value);
   end Set_Block_Origin_Byte_Count;


   function Get_Block_Origin_Byte_Count (H: in out Instance'class) return Unsigned_8 is
   begin
      return H.Store.Get_Unsigned_8 (Block_Origin_Byte_Count_Offset);
   end Get_Block_Origin_Byte_Count;


   procedure Set_Block_Identifier_Byte_Count (H: in out Instance'class; Value: Unsigned_8) is
   begin
      H.Store.Set_Unsigned_8 (Block_Identifier_Byte_Count_Offset, Value);
   end Set_Block_Identifier_Byte_Count;


   function Get_Block_Identifier_Byte_Count (H: in out Instance'class) return Unsigned_8 is
   begin
      return H.Store.Get_Unsigned_8 (Block_Identifier_Byte_Count_Offset);
   end Get_Block_Identifier_Byte_Count;


   procedure Set_Block_Extension_Byte_Count (H: in out Instance'class; Value: Unsigned_8) is
   begin
      H.Store.Set_Unsigned_8 (Block_Extension_Byte_Count_Offset, Value);
   end Set_Block_Extension_Byte_Count;


   function Get_Block_Extension_Byte_Count (H: in out Instance'class) return Unsigned_8 is
   begin
      return H.Store.Get_Unsigned_8 (Block_Extension_Byte_Count_Offset);
   end Get_Block_Extension_Byte_Count;


   procedure Set_Block_Path_Prefix_Byte_Count (H: in out Instance'class; Value: Unsigned_8) is
   begin
      H.Store.Set_Unsigned_8 (Block_Path_Prefix_Byte_Count_Offset, Value);
   end Set_Block_Path_Prefix_Byte_Count;


   function Get_Block_Path_Prefix_Byte_Count (H: in out Instance'class) return Unsigned_8 is
   begin
      return H.Store.Get_Unsigned_8 (Block_Path_Prefix_Byte_Count_Offset);
   end Get_Block_Path_Prefix_Byte_Count;


   procedure Set_Block_Origin_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Origin_Offset_Offset, Value);
   end Set_Block_Origin_Offset;


   function Get_Block_Origin_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Origin_Offset_Offset);
   end Get_Block_Origin_Offset;


   procedure Set_Block_Identifier_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Identifier_Offset_Offset, Value);
   end Set_Block_Identifier_Offset;


   function Get_Block_Identifier_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Identifier_Offset_Offset);
   end Get_Block_Identifier_Offset;


   procedure Set_Block_Extension_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Extension_Offset_Offset, Value);
   end Set_Block_Extension_Offset;


   function Get_Block_Extension_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Extension_Offset_Offset);
   end Get_Block_Extension_Offset;


   procedure Set_Block_Path_Prefix_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Path_Prefix_Offset_Offset, Value);
   end Set_Block_Path_Prefix_Offset;


   function Get_Block_Path_Prefix_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Path_Prefix_Offset_Offset);
   end Get_Block_Path_Prefix_Offset;


   procedure Set_Block_Acquisition_URL_Byte_Count (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Acquisition_URL_Byte_Count_Offset, Value);
   end Set_Block_Acquisition_URL_Byte_Count;


   function Get_Block_Acquisition_URL_Byte_Count (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Acquisition_URL_Byte_Count_Offset);
   end Get_Block_Acquisition_URL_Byte_Count;


   procedure Set_Block_Acquisition_URL_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Acquisition_URL_Offset_Offset, Value);
   end Set_Block_Acquisition_URL_Offset;


   function Get_Block_Acquisition_URL_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Acquisition_URL_Offset_Offset);
   end Get_Block_Acquisition_URL_Offset;


   procedure Set_Block_Target_List_Byte_Count (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Target_List_Byte_Count_Offset, Value);
   end Set_Block_Target_List_Byte_Count;


   function Get_Block_Target_List_Byte_Count (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Target_List_Byte_Count_Offset);
   end Get_Block_Target_List_Byte_Count;


   procedure Set_Block_Target_List_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Target_List_Offset_Offset, Value);
   end Set_Block_Target_List_Offset;


   function Get_Block_Target_List_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Target_List_Offset_Offset);
   end Get_Block_Target_List_Offset;


   procedure Set_Block_Public_Key_URL_Byte_Count (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Public_Key_URL_Byte_Count_Offset, Value);
   end Set_Block_Public_Key_URL_Byte_Count;


   function Get_Block_Public_Key_URL_Byte_Count (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Public_Key_URL_Byte_Count_Offset);
   end Get_Block_Public_Key_URL_Byte_Count;


   procedure Set_Block_Public_Key_URL_Offset (H: in out Instance'class; Value: Unsigned_16) is
   begin
      H.Store.Set_Unsigned_16 (Block_Public_Key_URL_Offset_Offset, Value);
   end Set_Block_Public_Key_URL_Offset;


   function Get_Block_Public_Key_URL_Offset (H: in out Instance'class) return Unsigned_16 is
   begin
      return H.Store.Get_Unsigned_16 (Block_Public_Key_URL_Offset_Offset);
   end Get_Block_Public_Key_URL_Offset;


   procedure Set_Block_Creation_Timestamp (H: in out Instance'class; Value: Unsigned_64) is
   begin
      H.Store.Set_Unsigned_64 (Block_Creation_Timestamp_Offset, Value);
   end Set_Block_Creation_Timestamp;


   function Get_Block_Creation_Timestamp (H: in out Instance'class) return Unsigned_64 is
   begin
      return H.Store.Get_Unsigned_64 (Block_Creation_Timestamp_Offset);
   end Get_Block_Creation_Timestamp;


   procedure Set_Block_Modification_Timestamp (H: in out Instance'class; Value: Unsigned_64) is
   begin
      H.Store.Set_Unsigned_64 (Block_Modification_Timestamp_Offset, Value);
   end Set_Block_Modification_Timestamp;


   function Get_Block_Modification_Timestamp (H: in out Instance'class) return Unsigned_64 is
   begin
      return H.Store.Get_Unsigned_64 (Block_Modification_Timestamp_Offset);
   end Get_Block_Modification_Timestamp;


   procedure Set_Block_Expiry_Timestamp (H: in out Instance'class; Value: Unsigned_64) is
   begin
      H.Store.Set_Unsigned_64 (Block_Expiry_Timestamp_Offset, Value);
   end Set_Block_Expiry_Timestamp;


   function Get_Block_Expiry_Timestamp (H: in out Instance'class) return Unsigned_64 is
   begin
      return H.Store.Get_Unsigned_64 (Block_Expiry_Timestamp_Offset);
   end Get_Block_Expiry_Timestamp;


   procedure Set_Reserved_1a (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_16; Value: Unsigned_32) is
      Offset: Unsigned_32 := Unsigned_32 (For_Block_Header_Byte_Count) - Block_Reserved_1a_Offset;
   begin
      H.Store.Set_Unsigned_32 (Offset, Value);
   end Set_Reserved_1a;


   function Get_Reserved_1a (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_16) return Unsigned_32 is
      Offset: Unsigned_32 := Unsigned_32 (For_Block_Header_Byte_Count) - Block_Reserved_1a_Offset;
   begin
      return H.Store.Get_Unsigned_32 (Offset);
   end Get_Reserved_1a;


   procedure Set_Reserved_1b (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_16; Value: Unsigned_16) is
      Offset: Unsigned_32 := Unsigned_32 (For_Block_Header_Byte_Count) - Block_Reserved_1b_Offset;
   begin
      H.Store.Set_Unsigned_16 (Offset, Value);
   end Set_Reserved_1b;


   function Get_Reserved_1b (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_16) return Unsigned_16 is
      Offset: Unsigned_32 := Unsigned_32 (For_Block_Header_Byte_Count) - Block_Reserved_1b_Offset;
   begin
      return H.Store.Get_Unsigned_16 (Offset);
   end Get_Reserved_1b;


   procedure Set_Block_Header_Crc16 (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_16; Value: Unsigned_16) is
      Offset: Unsigned_32 := Unsigned_32 (For_Block_Header_Byte_Count) - Block_Header_CRC16_Offset;
   begin
      H.Store.Set_Unsigned_16 (Offset, Value);
   end Set_Block_Header_Crc16;


   function Get_Block_Header_Crc16 (H: in out Instance'class; For_Block_Header_Byte_Count: Unsigned_16) return Unsigned_16 is
      Offset: Unsigned_32 := Unsigned_32 (For_Block_Header_Byte_Count) - Block_Header_CRC16_Offset;
   begin
      return H.Store.Get_Unsigned_16 (Offset);
   end Get_Block_Header_Crc16;


   procedure Update_Block_Header_Crc16 (H: in out Instance'class) is
      Byte_Count: constant Unsigned_16 := H.Get_Block_Header_Byte_Count;
      Crc16: constant Unsigned_16 := H.Store.Get_CRC_16_Over_Range (Start => 0, Count => Unsigned_32 (Byte_Count));
   begin
      H.Set_Block_Header_Crc16 (For_Block_Header_Byte_Count => Byte_Count, Value => Crc16);
   end Update_Block_Header_Crc16;


end Brbon.Block.Header;

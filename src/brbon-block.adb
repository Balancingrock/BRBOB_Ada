with Interfaces; use Interfaces;

with Ada.Exceptions;

with BRBON; use BRBON;
with BRBON.Header;
with BRBON.Container;


package body BRBON.Block is

   -- ==========================================================================
   -- == BRBON.Header.Header_Interface implementation                         ==
   -- ==========================================================================

   procedure Header_Set_Synchronization_Byte_1 (I: in out Instance) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Synchronization_Byte_1_Offset, Header.Block_Synchronization_Byte_1_Expected_Value);
   end Header_Set_Synchronization_Byte_1;


   function Header_Verify_Synchronization_Byte_1 (I: in out Instance) return Boolean is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Synchronization_Byte_1_Offset) = Header.Block_Synchronization_Byte_1_Expected_Value;
   end Header_Verify_Synchronization_Byte_1;


   procedure Header_Set_Synchronization_Byte_2 (I: in out Instance) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Synchronization_Byte_2_Offset, Header.Block_Synchronization_Byte_2_Expected_Value);
   end Header_Set_Synchronization_Byte_2;


   function Header_Verify_Synchronization_Byte_2 (I: in out Instance) return Boolean is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Synchronization_Byte_2_Offset) = Header.Block_Synchronization_Byte_2_Expected_Value;
   end Header_Verify_Synchronization_Byte_2;


   procedure Header_Set_Synchronization_Byte_3 (I: in out Instance) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Synchronization_Byte_3_Offset, Header.Block_Synchronization_Byte_3_Expected_Value);
   end Header_Set_Synchronization_Byte_3;


   function Header_Verify_Synchronization_Byte_3 (I: in out Instance) return Boolean is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Synchronization_Byte_3_Offset) = Header.Block_Synchronization_Byte_3_Expected_Value;
   end Header_Verify_Synchronization_Byte_3;


   procedure Header_Set_Synchronization_Byte_4 (I: in out Instance) is
   begin
      if I.Container.Uses_Endianness = Little then
         I.Container.Set_Unsigned_8 (Header.Block_Synchronization_Byte_4_Offset, Header.Block_Synchronization_Byte_4_Little_Endian_Expected_Value);
      else
         I.Container.Set_Unsigned_8 (Header.Block_Synchronization_Byte_4_Offset, Header.Block_Synchronization_Byte_4_Big_Endian_Expected_Value);
      end if;
   end Header_Set_Synchronization_Byte_4;


   function Header_Verify_Synchronization_Byte_4 (I: in out Instance) return Boolean is
   begin
      if I.Container.Get_Unsigned_8 (Header.Block_Synchronization_Byte_4_Offset) = Header.Block_Synchronization_Byte_4_Little_Endian_Expected_Value then
         I.Container.Set_Data_Endianness (Little);
         return true;
      elsif I.Container.Get_Unsigned_8 (Header.Block_Synchronization_Byte_4_Offset) = Header.Block_Synchronization_Byte_4_Big_Endian_Expected_Value then
         I.Container.Set_Data_Endianness (Big);
         return true;
      else
         return false;
      end if;
   end Header_Verify_Synchronization_Byte_4;


   function Header_Get_Endianness (I: in out Instance) return Endianness is
   begin
      return I.Container.Uses_Endianness;
   end Header_Get_Endianness;


   function Header_Verify_Synchronization_Bytes (I: in out Instance) return Boolean is
   begin
      if Header_Verify_Synchronization_Byte_1 (I) = false then return false; end if;
      if Header_Verify_Synchronization_Byte_2 (I) = false then return false; end if;
      if Header_Verify_Synchronization_Byte_3 (I) = false then return false; end if;
      return Header_Verify_Synchronization_Byte_4 (I);
   end Header_Verify_Synchronization_Bytes;


   procedure Header_Set_Type (I: in out Instance; Value: Block_Type) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Type_Offset, To_Unsigned_16 (Value));
   end Header_Set_Type;


   function Header_Get_Type (I: in out Instance) return Block_Type is
   begin
      return To_Block_Type (I.Container.Get_Unsigned_16 (Header.Block_Type_Offset));
   end Header_Get_Type;


   procedure Header_Set_Options (I: in out Instance; Value: Block_Options) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Options_Offset, To_Unsigned_16 (Value));
   end Header_Set_Options;


   function Header_Get_Options (I: in out Instance) return Block_Options is
   begin
      return To_Block_Options (I.Container.Get_Unsigned_16 (Header.Block_Options_Offset));
   end Header_Get_Options;


   procedure Header_Set_Byte_Count (I: in out Instance; Value: Unsigned_32) is
   begin
      I.Container.Set_Unsigned_32 (Header.Block_Byte_Count_Offset, Value);
   end Header_Set_Byte_Count;


   function Header_Get_Byte_Count (I: in out Instance) return Unsigned_32 is
   begin
      return I.Container.Get_Unsigned_32 (Header.Block_Byte_Count_Offset);
   end Header_Get_Byte_Count;


   procedure Header_Set_Header_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Header_Byte_Count_Offset, Value);
   end Header_Set_Header_Byte_Count;


   function Header_Get_Header_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Header_Byte_Count_Offset);
   end Header_Get_Header_Byte_Count;


   procedure Header_Set_Encrypted_Header_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Encrypted_Header_Byte_Count_Offset, Value);
   end Header_Set_Encrypted_Header_Byte_Count;


   function Header_Get_Encrypted_Header_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Encrypted_Header_Byte_Count_Offset);
   end Header_Get_Encrypted_Header_Byte_Count;


   procedure Header_Set_Origin_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Origin_CRC16_Offset, Value);
   end Header_Set_Origin_Crc16;


   function Header_Get_Origin_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Origin_CRC16_Offset);
   end Header_Get_Origin_Crc16;


   procedure Header_Set_Identifier_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Identifier_CRC16_Offset, Value);
   end Header_Set_Identifier_Crc16;


   function Header_Get_Identifier_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Identifier_CRC16_Offset);
   end Header_Get_Identifier_Crc16;


   procedure Header_Set_Extension_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Extension_CRC16_Offset, Value);
   end Header_Set_Extension_Crc16;


   function Header_Get_Extension_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Extension_CRC16_Offset);
   end Header_Get_Extension_Crc16;


   procedure Header_Set_Path_Prefix_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Path_Prefix_CRC16_Offset, Value);
   end Header_Set_Path_Prefix_Crc16;


   function Header_Get_Path_Prefix_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Path_Prefix_CRC16_Offset);
   end Header_Get_Path_Prefix_Crc16;


   procedure Header_Set_Origin_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Origin_Byte_Count_Offset, Value);
   end Header_Set_Origin_Byte_Count;


   function Header_Get_Origin_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Origin_Byte_Count_Offset);
   end Header_Get_Origin_Byte_Count;


   procedure Header_Set_Identifier_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Identifier_Byte_Count_Offset, Value);
   end Header_Set_Identifier_Byte_Count;


   function Header_Get_Identifier_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Identifier_Byte_Count_Offset);
   end Header_Get_Identifier_Byte_Count;


   procedure Header_Set_Extension_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Extension_Byte_Count_Offset, Value);
   end Header_Set_Extension_Byte_Count;


   function Header_Get_Extension_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Extension_Byte_Count_Offset);
   end Header_Get_Extension_Byte_Count;


   procedure Header_Set_Path_Prefix_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Block_Path_Prefix_Byte_Count_Offset, Value);
   end Header_Set_Path_Prefix_Byte_Count;


   function Header_Get_Path_Prefix_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Block_Path_Prefix_Byte_Count_Offset);
   end Header_Get_Path_Prefix_Byte_Count;


   procedure Header_Set_Origin_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Origin_Offset_Offset, Value);
   end Header_Set_Origin_Offset;


   function Header_Get_Origin_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Origin_Offset_Offset);
   end Header_Get_Origin_Offset;


   procedure Header_Set_Identifier_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Identifier_Offset_Offset, Value);
   end Header_Set_Identifier_Offset;


   function Header_Get_Identifier_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Identifier_Offset_Offset);
   end Header_Get_Identifier_Offset;


   procedure Header_Set_Extension_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Extension_Offset_Offset, Value);
   end Header_Set_Extension_Offset;


   function Header_Get_Extension_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Extension_Offset_Offset);
   end Header_Get_Extension_Offset;


   procedure Header_Set_Path_Prefix_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Path_Prefix_Offset_Offset, Value);
   end Header_Set_Path_Prefix_Offset;


   function Header_Get_Path_Prefix_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Path_Prefix_Offset_Offset);
   end Header_Get_Path_Prefix_Offset;


   procedure Header_Set_Acquisition_URL_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Acquisition_URL_Byte_Count_Offset, Value);
   end Header_Set_Acquisition_URL_Byte_Count;


   function Header_Get_Acquisition_URL_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Acquisition_URL_Byte_Count_Offset);
   end Header_Get_Acquisition_URL_Byte_Count;


   procedure Header_Set_Acquisition_URL_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Acquisition_URL_Offset_Offset, Value);
   end Header_Set_Acquisition_URL_Offset;


   function Header_Get_Acquisition_URL_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Acquisition_URL_Offset_Offset);
   end Header_Get_Acquisition_URL_Offset;


   procedure Header_Set_Target_List_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Target_List_Byte_Count_Offset, Value);
   end Header_Set_Target_List_Byte_Count;


   function Header_Get_Target_List_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Target_List_Byte_Count_Offset);
   end Header_Get_Target_List_Byte_Count;


   procedure Header_Set_Target_List_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Target_List_Offset_Offset, Value);
   end Header_Set_Target_List_Offset;


   function Header_Get_Target_List_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Target_List_Offset_Offset);
   end Header_Get_Target_List_Offset;


   procedure Header_Set_Public_Key_URL_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Public_Key_URL_Byte_Count_Offset, Value);
   end Header_Set_Public_Key_URL_Byte_Count;


   function Header_Get_Public_Key_URL_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Public_Key_URL_Byte_Count_Offset);
   end Header_Get_Public_Key_URL_Byte_Count;


   procedure Header_Set_Public_Key_URL_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Block_Public_Key_URL_Offset_Offset, Value);
   end Header_Set_Public_Key_URL_Offset;


   function Header_Get_Public_Key_URL_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Block_Public_Key_URL_Offset_Offset);
   end Header_Get_Public_Key_URL_Offset;


   procedure Header_Set_Creation_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      I.Container.Set_Unsigned_64 (Header.Block_Creation_Timestamp_Offset, Value);
   end Header_Set_Creation_Timestamp;


   function Header_Get_Creation_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return I.Container.Get_Unsigned_64 (Header.Block_Creation_Timestamp_Offset);
   end Header_Get_Creation_Timestamp;


   procedure Header_Set_Modification_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      I.Container.Set_Unsigned_64 (Header.Block_Modification_Timestamp_Offset, Value);
   end Header_Set_Modification_Timestamp;


   function Header_Get_Modification_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return I.Container.Get_Unsigned_64 (Header.Block_Modification_Timestamp_Offset);
   end Header_Get_Modification_Timestamp;


   procedure Header_Set_Expiry_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      I.Container.Set_Unsigned_64 (Header.Block_Expiry_Timestamp_Offset, Value);
   end Header_Set_Expiry_Timestamp;


   function Header_Get_Expiry_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return I.Container.Get_Unsigned_64 (Header.Block_Expiry_Timestamp_Offset);
   end Header_Get_Expiry_Timestamp;


   procedure Header_Set_Reserved_1a (I: in out Instance; Value: Unsigned_32) is
      Offset: Unsigned_32 := Header.Block_Reserved_1a_Offset;
   begin
      I.Container.Set_Unsigned_32 (Offset, Value);
   end Header_Set_Reserved_1a;


   function Header_Get_Reserved_1a (I: in out Instance) return Unsigned_32 is
      Offset: Unsigned_32 := Header.Block_Reserved_1a_Offset;
   begin
      return I.Container.Get_Unsigned_32 (Offset);
   end Header_Get_Reserved_1a;


   procedure Header_Set_Reserved_1b (I: in out Instance; Value: Unsigned_16) is
      Offset: Unsigned_32 := Header.Block_Reserved_1b_Offset;
   begin
      I.Container.Set_Unsigned_16 (Offset, Value);
   end Header_Set_Reserved_1b;


   function Header_Get_Reserved_1b (I: in out Instance) return Unsigned_16 is
      Offset: Unsigned_32 := Header.Block_Reserved_1b_Offset;
   begin
      return I.Container.Get_Unsigned_16 (Offset);
   end Header_Get_Reserved_1b;


   procedure Header_Set_Header_Crc16 (I: in out Instance; Value: Unsigned_16) is
      Offset: Unsigned_32 := Header.Block_Header_CRC16_Offset;
   begin
      I.Container.Set_Unsigned_16 (Offset, Value);
   end Header_Set_Header_Crc16;


   function Header_Get_Header_Crc16 (I: in out Instance) return Unsigned_16 is
      Offset: Unsigned_32 := Header.Block_Header_CRC16_Offset;
   begin
      return I.Container.Get_Unsigned_16 (Offset);
   end Header_Get_Header_Crc16;


   procedure Header_Update_Block_Header_Crc16 (I: in out Instance) is
      Byte_Count: constant Unsigned_16 := Unsigned_16 (Header_Get_Byte_Count (I));
      Crc16: constant Unsigned_16 := I.Container.Get_CRC_16_Over_Range (Start => 0, Count => Unsigned_32 (Byte_Count));
   begin
      Header_Set_Header_Crc16 (I, Value => Crc16);
   end Header_Update_Block_Header_Crc16;


   -- =====================================================
   -- Block defined operations


   procedure Write_To_File (I: in out Instance; To_Path: String) is
   begin
      I.Ensure_Block_Consistency;
      I.Container.Write_To_File (To_Path);
   end Write_To_File;


   function Test_Serializer (I: in out Instance) return Serializable.Instance is
   begin
      return Serializable.Create_Without_Copy
        (Use_In_Place => I.Memory_Ptr,
         First        => I.Memory_Ptr.all'First,
         Last         => I.Memory_Ptr.all'Last);
   end Test_Serializer;


   procedure Ensure_Block_Consistency (I: in out Instance) is
   begin
      raise BRBON.Types.Not_Implemented_Yet;
   end Ensure_Block_Consistency;


   function Add_To_Header_Field (I: in out Instance; Value: String) return Unsigned_16 is
      Free_Bytes: Unsigned_16 := (I.Last_Free_Byte_In_Header_Field_Storage + 1) - I.First_Free_Byte_In_Header_Field_Storage; -- Sequence prevents negative intermediate
      Origin_Offset: Unsigned_16;
   begin
      -- Check if there is space available for the string in the header field
      if Value'Length > Free_Bytes then
         return 0; -- not enough space available
      else
         Container.Set_String (I.Container, Unsigned_32 (I.First_Free_Byte_In_Header_Field_Storage), Value);
         Origin_Offset := I.First_Free_Byte_In_Header_Field_Storage;
         I.First_Free_Byte_In_Header_Field_Storage := I.First_Free_Byte_In_Header_Field_Storage + Unsigned_16 (Value'Length);
         return Origin_Offset;
      end if;
   end Add_To_Header_Field;


--   function Get_Block_Origin (I: in out Instance) return String is
--      Offset: Unsigned_32 := Unsigned_32 (Get_Block_Origin_Offset (I.Container));
--      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Block_Origin_Byte_Count (I.Container));
--      Crc_16: Unsigned_16 := Get_Block_Origin_Crc16 (I.Container);
--      Str: String := C.Get_String (Offset, Byte_Count);
--      Cal_Crc_16: Unsigned_16 := CRC_Package.Calculate_CRC_16 (Str);
--   begin
--      if Crc_16 = Cal_Crc_16 then
--         return Str;
--      else
--         Ada.Exceptions.Raise_Exception (Block_Header_Error'Identity, "Origin CRC-16 validation error.\n The stored CRC-16 is not equal to the CRC-16 calculated over the stored string");
--      end if;
--   end Get_Block_Origin;

end BRBON.Block;

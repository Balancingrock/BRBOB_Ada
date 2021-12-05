with Interfaces; use Interfaces;

with Ada.Exceptions;

with CRC_Package;

with BRBON; use BRBON;
with BRBON.Header;
with BRBON.Container;


package body BRBON.Block is


   -- ==========================================================================
   -- Header Interface
   -- ==========================================================================

   procedure Header_Set_Synchronization_Byte_1 (I: in out Instance) is
   begin
      I.Container.Set_Unsigned_8 (Header.Synchronization_Byte_1_Offset, Header.Synchronization_Byte_1_Expected_Value);
   end Header_Set_Synchronization_Byte_1;


   function Header_Verify_Synchronization_Byte_1 (I: in out Instance) return Boolean is
   begin
      return I.Container.Get_Unsigned_8 (Header.Synchronization_Byte_1_Offset) = Header.Synchronization_Byte_1_Expected_Value;
   end Header_Verify_Synchronization_Byte_1;


   procedure Header_Set_Synchronization_Byte_2 (I: in out Instance) is
   begin
      I.Container.Set_Unsigned_8 (Header.Synchronization_Byte_2_Offset, Header.Synchronization_Byte_2_Expected_Value);
   end Header_Set_Synchronization_Byte_2;


   function Header_Verify_Synchronization_Byte_2 (I: in out Instance) return Boolean is
   begin
      return I.Container.Get_Unsigned_8 (Header.Synchronization_Byte_2_Offset) = Header.Synchronization_Byte_2_Expected_Value;
   end Header_Verify_Synchronization_Byte_2;


   procedure Header_Set_Synchronization_Byte_3 (I: in out Instance) is
   begin
      I.Container.Set_Unsigned_8 (Header.Synchronization_Byte_3_Offset, Header.Synchronization_Byte_3_Expected_Value);
   end Header_Set_Synchronization_Byte_3;


   function Header_Verify_Synchronization_Byte_3 (I: in out Instance) return Boolean is
   begin
      return I.Container.Get_Unsigned_8 (Header.Synchronization_Byte_3_Offset) = Header.Synchronization_Byte_3_Expected_Value;
   end Header_Verify_Synchronization_Byte_3;


   procedure Header_Set_Synchronization_Byte_4 (I: in out Instance) is
   begin
      if I.Container.Uses_Endianness = Little then
         I.Container.Set_Unsigned_8 (Header.Synchronization_Byte_4_Offset, Header.Synchronization_Byte_4_Little_Endian_Expected_Value);
      else
         I.Container.Set_Unsigned_8 (Header.Synchronization_Byte_4_Offset, Header.Synchronization_Byte_4_Big_Endian_Expected_Value);
      end if;
   end Header_Set_Synchronization_Byte_4;


   function Header_Verify_Synchronization_Byte_4 (I: in out Instance) return Boolean is
   begin
      if I.Container.Get_Unsigned_8 (Header.Synchronization_Byte_4_Offset) = Header.Synchronization_Byte_4_Little_Endian_Expected_Value then
         I.Container.Set_Data_Endianness (Little);
         return true;
      elsif I.Container.Get_Unsigned_8 (Header.Synchronization_Byte_4_Offset) = Header.Synchronization_Byte_4_Big_Endian_Expected_Value then
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
      I.Container.Set_Unsigned_16 (Header.Type_Offset, To_Unsigned_16 (Value));
   end Header_Set_Type;


   function Header_Get_Type (I: in out Instance) return Block_Type is
   begin
      return To_Block_Type (I.Container.Get_Unsigned_16 (Header.Type_Offset));
   end Header_Get_Type;


   procedure Header_Set_Options (I: in out Instance; Value: Block_Options) is
   begin
      I.Container.Set_Unsigned_16 (Header.Options_Offset, To_Unsigned_16 (Value));
   end Header_Set_Options;


   function Header_Get_Options (I: in out Instance) return Block_Options is
   begin
      return To_Block_Options (I.Container.Get_Unsigned_16 (Header.Options_Offset));
   end Header_Get_Options;


   procedure Header_Set_Byte_Count (I: in out Instance; Value: Unsigned_32) is
   begin
      I.Container.Set_Unsigned_32 (Header.Byte_Count_Offset, Value);
   end Header_Set_Byte_Count;


   function Header_Get_Byte_Count (I: in out Instance) return Unsigned_32 is
   begin
      return I.Container.Get_Unsigned_32 (Header.Byte_Count_Offset);
   end Header_Get_Byte_Count;


   procedure Header_Set_Header_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Header_Byte_Count_Offset, Value);
   end Header_Set_Header_Byte_Count;


   function Header_Get_Header_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Header_Byte_Count_Offset);
   end Header_Get_Header_Byte_Count;


   procedure Header_Set_Encrypted_Header_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Encrypted_Header_Byte_Count_Offset, Value);
   end Header_Set_Encrypted_Header_Byte_Count;


   function Header_Get_Encrypted_Header_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Encrypted_Header_Byte_Count_Offset);
   end Header_Get_Encrypted_Header_Byte_Count;


   procedure Header_Set_Origin_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Origin_CRC16_Offset, Value);
   end Header_Set_Origin_Crc16;


   function Header_Get_Origin_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Origin_CRC16_Offset);
   end Header_Get_Origin_Crc16;


   procedure Header_Set_Identifier_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Identifier_CRC16_Offset, Value);
   end Header_Set_Identifier_Crc16;


   function Header_Get_Identifier_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Identifier_CRC16_Offset);
   end Header_Get_Identifier_Crc16;


   procedure Header_Set_Extension_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Extension_CRC16_Offset, Value);
   end Header_Set_Extension_Crc16;


   function Header_Get_Extension_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Extension_CRC16_Offset);
   end Header_Get_Extension_Crc16;


   procedure Header_Set_Path_Prefix_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Path_Prefix_CRC16_Offset, Value);
   end Header_Set_Path_Prefix_Crc16;


   function Header_Get_Path_Prefix_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Path_Prefix_CRC16_Offset);
   end Header_Get_Path_Prefix_Crc16;


   procedure Header_Set_Origin_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Origin_Byte_Count_Offset, Value);
   end Header_Set_Origin_Byte_Count;


   function Header_Get_Origin_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Origin_Byte_Count_Offset);
   end Header_Get_Origin_Byte_Count;


   procedure Header_Set_Identifier_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Identifier_Byte_Count_Offset, Value);
   end Header_Set_Identifier_Byte_Count;


   function Header_Get_Identifier_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Identifier_Byte_Count_Offset);
   end Header_Get_Identifier_Byte_Count;


   procedure Header_Set_Extension_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Extension_Byte_Count_Offset, Value);
   end Header_Set_Extension_Byte_Count;


   function Header_Get_Extension_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Extension_Byte_Count_Offset);
   end Header_Get_Extension_Byte_Count;


   procedure Header_Set_Path_Prefix_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      I.Container.Set_Unsigned_8 (Header.Path_Prefix_Byte_Count_Offset, Value);
   end Header_Set_Path_Prefix_Byte_Count;


   function Header_Get_Path_Prefix_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return I.Container.Get_Unsigned_8 (Header.Path_Prefix_Byte_Count_Offset);
   end Header_Get_Path_Prefix_Byte_Count;


   procedure Header_Set_Origin_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Origin_Offset_Offset, Value);
   end Header_Set_Origin_Offset;


   function Header_Get_Origin_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Origin_Offset_Offset);
   end Header_Get_Origin_Offset;


   procedure Header_Set_Identifier_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Identifier_Offset_Offset, Value);
   end Header_Set_Identifier_Offset;


   function Header_Get_Identifier_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Identifier_Offset_Offset);
   end Header_Get_Identifier_Offset;


   procedure Header_Set_Extension_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Extension_Offset_Offset, Value);
   end Header_Set_Extension_Offset;


   function Header_Get_Extension_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Extension_Offset_Offset);
   end Header_Get_Extension_Offset;


   procedure Header_Set_Path_Prefix_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Path_Prefix_Offset_Offset, Value);
   end Header_Set_Path_Prefix_Offset;


   function Header_Get_Path_Prefix_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Path_Prefix_Offset_Offset);
   end Header_Get_Path_Prefix_Offset;


   procedure Header_Set_Acquisition_URL_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Acquisition_URL_Byte_Count_Offset, Value);
   end Header_Set_Acquisition_URL_Byte_Count;


   function Header_Get_Acquisition_URL_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Acquisition_URL_Byte_Count_Offset);
   end Header_Get_Acquisition_URL_Byte_Count;


   procedure Header_Set_Acquisition_URL_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Acquisition_URL_Offset_Offset, Value);
   end Header_Set_Acquisition_URL_Offset;


   function Header_Get_Acquisition_URL_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Acquisition_URL_Offset_Offset);
   end Header_Get_Acquisition_URL_Offset;


   procedure Header_Set_Target_List_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Target_List_Byte_Count_Offset, Value);
   end Header_Set_Target_List_Byte_Count;


   function Header_Get_Target_List_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Target_List_Byte_Count_Offset);
   end Header_Get_Target_List_Byte_Count;


   procedure Header_Set_Target_List_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Target_List_Offset_Offset, Value);
   end Header_Set_Target_List_Offset;


   function Header_Get_Target_List_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Target_List_Offset_Offset);
   end Header_Get_Target_List_Offset;


   procedure Header_Set_Public_Key_URL_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Public_Key_URL_Byte_Count_Offset, Value);
   end Header_Set_Public_Key_URL_Byte_Count;


   function Header_Get_Public_Key_URL_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Public_Key_URL_Byte_Count_Offset);
   end Header_Get_Public_Key_URL_Byte_Count;


   procedure Header_Set_Public_Key_URL_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      I.Container.Set_Unsigned_16 (Header.Public_Key_URL_Offset_Offset, Value);
   end Header_Set_Public_Key_URL_Offset;


   function Header_Get_Public_Key_URL_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return I.Container.Get_Unsigned_16 (Header.Public_Key_URL_Offset_Offset);
   end Header_Get_Public_Key_URL_Offset;


   procedure Header_Set_Creation_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      I.Container.Set_Unsigned_64 (Header.Creation_Timestamp_Offset, Value);
   end Header_Set_Creation_Timestamp;


   function Header_Get_Creation_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return I.Container.Get_Unsigned_64 (Header.Creation_Timestamp_Offset);
   end Header_Get_Creation_Timestamp;


   procedure Header_Set_Modification_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      I.Container.Set_Unsigned_64 (Header.Modification_Timestamp_Offset, Value);
   end Header_Set_Modification_Timestamp;


   function Header_Get_Modification_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return I.Container.Get_Unsigned_64 (Header.Modification_Timestamp_Offset);
   end Header_Get_Modification_Timestamp;


   procedure Header_Set_Expiry_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      I.Container.Set_Unsigned_64 (Header.Expiry_Timestamp_Offset, Value);
   end Header_Set_Expiry_Timestamp;


   function Header_Get_Expiry_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return I.Container.Get_Unsigned_64 (Header.Expiry_Timestamp_Offset);
   end Header_Get_Expiry_Timestamp;


   procedure Header_Set_Reserved_1a (I: in out Instance; Value: Unsigned_32) is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1a_Distance_Before_Header_End;
   begin
      I.Container.Set_Unsigned_32 (Offset, Value);
   end Header_Set_Reserved_1a;


   function Header_Get_Reserved_1a (I: in out Instance) return Unsigned_32 is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1a_Distance_Before_Header_End;
   begin
      return I.Container.Get_Unsigned_32 (Offset);
   end Header_Get_Reserved_1a;


   procedure Header_Set_Reserved_1b (I: in out Instance; Value: Unsigned_16) is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1b_Distance_Before_Header_End;
   begin
      I.Container.Set_Unsigned_16 (Offset, Value);
   end Header_Set_Reserved_1b;


   function Header_Get_Reserved_1b (I: in out Instance) return Unsigned_16 is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1b_Distance_Before_Header_End;
   begin
      return I.Container.Get_Unsigned_16 (Offset);
   end Header_Get_Reserved_1b;


   procedure Header_Set_Header_Crc16 (I: in out Instance; Value: Unsigned_16) is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Header_CRC16_Distance_Before_Header_End;
   begin
      I.Container.Set_Unsigned_16 (Offset, Value);
   end Header_Set_Header_Crc16;


   function Header_Get_Header_Crc16 (I: in out Instance) return Unsigned_16 is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Header_CRC16_Distance_Before_Header_End;
   begin
      return I.Container.Get_Unsigned_16 (Offset);
   end Header_Get_Header_Crc16;


   function Read_Field_Storage_Strings (I: in out Instance) return Field_Storage_Strings is
   begin
      return
        (
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Origin),
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Identifier),
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Extension),
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Path_Prefix),
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Acqquisition_URL),
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Target_List),
         Ada.Strings.Unbounded.To_Unbounded_String (I.Get_Public_Key_Url)
        );
   end Read_Field_Storage_Strings;


   procedure Write_Field_Storage_Strings (I: in out Instance; Strings: Field_Storage_Strings) is

      Offset: Unsigned_32 := BRBON.Header.Header_Field_Storage_Type_1_Offset;
      Free_Bytes: Unsigned_16 := I.Header_Get_Header_Byte_Count;

      procedure Add
        (
         UStr: Ada.Strings.Unbounded.Unbounded_String;
         Set_Offset: U16_Setter;
         Set_Byte_Count: U8_Setter;
         Set_CRC_16: U16_Setter
        )
      is
         Str: String := Ada.Strings.Unbounded.To_String (UStr);
      begin
         Set_Offset (I, Unsigned_16 (Offset));
         Set_Byte_Count (I, Unsigned_8 (Str'Length));
         if Str'Length > 0 then
            if Str'Length > Free_Bytes then
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "String length exceeds available area");
            else
               I.Container.Set_String (Offset, Str);
               Offset := Offset + Unsigned_32 (Str'Length);
               Free_Bytes := Free_Bytes - Unsigned_16 (Str'Length);
            end if;
            Set_CRC_16 (I, CRC_Package.Calculate_CRC_16 (Str));
         else
            Set_CRC_16 (I, 0);
            Set_Offset (I, 0);
         end if;
      end Add;

      procedure Add
        (
         UStr: Ada.Strings.Unbounded.Unbounded_String;
         Set_Offset: U16_Setter;
         Set_Byte_Count: U16_Setter
        )
      is
         Str: String := Ada.Strings.Unbounded.To_String (UStr);
      begin
         Set_Offset (I, Unsigned_16 (Offset));
         Set_Byte_Count (I, Unsigned_16 (Str'Length));
         if Str'Length > 0 then
            if Str'Length > Free_Bytes then
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "String length exceeds available area");
            else
               I.Container.Set_String (Offset, Str);
               Offset := Offset + Unsigned_32 (Str'Length);
               Free_Bytes := Free_Bytes - Unsigned_16 (Str'Length);
            end if;
         else
            Set_Offset (I, 0);
         end if;
      end Add;

   begin

      Add (Strings.Origin, Header_Set_Origin_Offset'Access, Header_Set_Origin_Byte_Count'Access, Header_Set_Origin_Crc16'Access);
      Add (Strings.Identifier, Header_Set_Identifier_Offset'Access, Header_Set_Identifier_Byte_Count'Access, Header_Set_Identifier_Crc16'Access);
      Add (Strings.Extension, Header_Set_Extension_Offset'Access, Header_Set_Extension_Byte_Count'Access, Header_Set_Extension_Crc16'Access);
      Add (Strings.Path_Prefix, Header_Set_Path_Prefix_Offset'Access, Header_Set_Path_Prefix_Byte_Count'Access, Header_Set_Path_Prefix_Crc16'Access);

      Add (Strings.Acquisition_URL, Header_Set_Acquisition_URL_Offset'Access, Header_Set_Acquisition_URL_Byte_Count'Access);
      Add (Strings.Target_List, Header_Set_Target_List_Offset'Access, Header_Set_Target_List_Byte_Count'Access);
      Add (Strings.Public_Key_URL, Header_Set_Public_Key_URL_Offset'Access, Header_Set_Public_Key_URL_Byte_Count'Access);

   end Write_Field_Storage_Strings;


   function Field_Storage_Strings_Byte_Count (F: Field_Storage_Strings) return Unsigned_16 is
      Sum: Unsigned_16 := 0;
   begin
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Origin));
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Identifier));
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Extension));
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Path_Prefix));
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Acquisition_URL));
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Target_List));
      Sum := Sum + Unsigned_16 (Ada.Strings.Unbounded.Length (F.Public_Key_URL));
      return Sum;
   end Field_Storage_Strings_Byte_Count;


   function Field_Storage_Free_Bytes (I: in out Instance) return Unsigned_16 is
      FS: Field_Storage_Strings := I.Read_Field_Storage_Strings;
   begin
      return I.Header_Get_Header_Byte_Count - Field_Storage_Strings_Byte_Count (FS);
   end Field_Storage_Free_Bytes;


   procedure Set_Origin (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Origin := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new origin (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
      end if;
   end Set_Origin;


   function Get_Origin (I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Origin_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Origin_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Origin;


   procedure Set_Identifier (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Identifier := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new identifier (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
      end if;
   end Set_Identifier;


   function Get_Identifier (I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Identifier_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Identifier_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Identifier;


   procedure Set_Extension (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Extension := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new extension (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
         end if;
   end Set_Extension;


   function Get_Extension (I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Extension_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Extension_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Extension;


   procedure Set_Path_Prefix (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Path_Prefix := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new path prefix (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
         end if;
   end Set_Path_Prefix;


   function Get_Path_Prefix (I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Path_Prefix_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Path_Prefix_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Path_Prefix;


   procedure Set_Acquisition_URL (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Acquisition_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new acquisition URL (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
      end if;
   end Set_Acquisition_URL;


   function Get_Acqquisition_URL(I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Acquisition_URL_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Acquisition_URL_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Acqquisition_URL;


   procedure Set_Target_List (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Target_List := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new target list (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
      end if;
   end Set_Target_List;


   function Get_Target_List (I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Target_List_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Target_List_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Target_List;


   procedure Set_Public_Key_URL (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Public_Key_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Error'Identity, "Not enough space available in block header field storage for new public key URL (" & Value & ")");
      else
         Write_Field_Storage_Strings (I, Field_Strings);
      end if;
   end Set_Public_Key_URL;


   function Get_Public_Key_URL(I: in out Instance) return String is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Public_Key_URL_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (I.Header_Get_Public_Key_URL_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return I.Container.Get_String (Offset, Byte_Count);
      end if;
   end Get_Public_Key_URL;


   --


   function Byte_Count (I: in out Instance) return Unsigned_32 is
   begin
      return I.Header_Get_Byte_Count;
   end Byte_Count;


   procedure Update_Block_CRC (I: in out Instance) is
      BC: constant Unsigned_32 := I.Header_Get_Byte_Count;
      Crc: constant Unsigned_32 := I.Container.Get_CRC_32_Over_Range (Start => 0, Count => BC - 4);
   begin
      I.Container.Set_Unsigned_32 (Offset => BC - 4, Value => Crc);
   end Update_Block_CRC;


   procedure Update_Header_CRC (I: in out Instance) is
      HC: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count);
      Crc: Unsigned_16 := I.Container.Get_CRC_16_Over_Range (Start => 0, Count => HC);
   begin
      I.Header_Set_Header_Crc16 (Crc);
   end Update_Header_CRC;


   -- ========================================================
   -- Child support operations
   -- ========================================================

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
     ) is separate;


   -- =====================================================
   -- Block API
   -- ======================================================

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
         I.Update_Header_CRC;
         I.Update_Block_CRC;
   end Ensure_Block_Consistency;


end BRBON.Block;

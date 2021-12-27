with Interfaces; use Interfaces;

with Ada.Exceptions;
with Ada.Strings.Unbounded;


with CRC_Package;

with BRBON; use BRBON;
with BRBON.Container;




package body BRBON.Block is

   -- ----------------------
   -- Internal definitions -
   -- ----------------------

   -- Expected synchronization values
   --
   Synch_Byte_1_Expected_Value:           constant Unsigned_8 := 16#96#;
   Synch_Byte_2_Expected_Value:           constant Unsigned_8 := 16#7F#;
   Synch_Byte_3_Expected_Value:           constant Unsigned_8 := 16#81#;
   Synch_Byte_4_LSB_First_Expected_Value: constant Unsigned_8 := 16#5A#;
   Synch_Byte_4_MSB_First_Expected_Value: constant Unsigned_8 := 16#A5#;

   Synch_Byte_4_LUT: constant array (Byte_Storage_Order, Boolean) of Unsigned_8 :=
     (
      (Synch_Byte_4_MSB_First_Expected_Value, Synch_Byte_4_LSB_First_Expected_Value),
      (Synch_Byte_4_LSB_First_Expected_Value, Synch_Byte_4_MSB_First_Expected_Value)
     );


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


   -----------------------------------------------------------------------------

   function Read_Field_Storage_Strings (S: BRBON.Store) return Field_Storage_Strings is
   begin
      return
        (
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Origin (S)),
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Identifier (S)),
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Extension (S)),
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Path_Prefix (S)),
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Acquisition_URL (S)),
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Target_List (S)),
         Ada.Strings.Unbounded.To_Unbounded_String (Get_Public_Key_Url (S))
        );
   end Read_Field_Storage_Strings;


   -----------------------------------------------------------------------------

   procedure Write_Field_Storage_Strings (S: in out BRBON.Store; Strings: Field_Storage_Strings) is

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
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "String length exceeds available area");
            else
               Container.Set_String (I.Container, Offset, Str);
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
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "String length exceeds available area");
            else
               Container.Set_String (I.Container, Offset, Str);
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


   -----------------------------------------------------------------------------

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

   -----------------------------------------------------------------------------

   function Get_Byte_Storage_Order (S: BRBON.Store) return BRBON.Byte_Storage_Order is
   begin
      if S.Swap then
         if BRBON.Machine_Byte_Storage_Order = BRBON.MSB_First then
            return BRBON.LSB_First;
         else
            return BRBON.MSB_First;
         end if;
      else
         return BRBON.Machine_Byte_Storage_Order;
      end if;
   end Get_Byte_Storage_Order;


   -----------------------------------------------------------------------------

   function Get_Block_Type (S: BRBON.Store) return BRBON.Block_Type is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Is_Type;
   end Get_Block_Type;


   -----------------------------------------------------------------------------

   procedure Set_Block_Options (S: BRBON.Store; Value: BRBON.Block_Options) is
   begin
      To_Block_Header_Leading_Ptr (S.Data (0)'Access).Options := Value;
   end Set_Block_Options;


   -----------------------------------------------------------------------------

   function Get_Block_Options (S: BRBON.Store) return BRBON.Block_Options is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Options;
   end Get_Block_Options;


   -----------------------------------------------------------------------------

   function Get_Block_Byte_Count (S: BRBON.Store) return Unsigned_32 is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Block_Byte_Count;
   end Get_Block_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Header_Byte_Count (S: BRBON.Store) return Unsigned_16 is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Header_Byte_Count;
   end Get_Header_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Encrypted_Header_Byte_Count (S: BRBON.Store) return Unsigned_16 is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Encrypted_Header_Byte_Count;
   end Get_Encrypted_Header_Byte_Count;


   -----------------------------------------------------------------------------

   procedure Set_Origin (S: BRBON.Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (BRBON.String_Too_Long'Identity, "The specified origin string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Origin := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if FSS.Byte_Count > FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (BRBON.Memory_Error'Identity, "The new origin string cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (FSS);
   end Set_Origin;


   -----------------------------------------------------------------------------

   function Get_Origin (S: BRBON.Store) return String is
      HPtr: Block_Header_Leading_Ptr := To_Block_Header_Leading_Ptr (S.Data (0)'Access);
      Offset: Unsigned_32 := Unsigned_32 (HPtr.Origin_Offset);
      Byte_Count: Unsigned_32 := Unsigned_32 (HPtr.Origin_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Offset, Byte_Count);
      end if;
   end Get_Origin;


   -----------------------------------------------------------------------------

   function Get_Origin_CRC (S: BRBON.Store) return Unsigned_16 is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Origin_CRC;
   end Get_Origin_CRC;


   -----------------------------------------------------------------------------

   function Get_Origin_Byte_Count (S: BRBON.Store) return Unsigned_8 is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Origin_Byte_Count;
   end Get_Origin_Byte_Count;


   -----------------------------------------------------------------------------

   procedure Set_Identifier (S: BRBON.Store; Value: String) is
   begin
      null;
   end Set_Identifier;


   -----------------------------------------------------------------------------

   function Get_Identifier (S: BRBON.Store) return String;


   -----------------------------------------------------------------------------

   function Get_Identifier_CRC (S: BRBON.Store) return Unsigned_16;


   -----------------------------------------------------------------------------

   function Get_Identifier_Byte_Count (S: BRBON.Store) return Unsigned_8;


   -----------------------------------------------------------------------------

   procedure Set_Extension (S: BRBON.Store; Value: String);


-- -----------------------------------------------------------------------------

   function Get_Extension (S: BRBON.Store) return String;


-- -----------------------------------------------------------------------------

   function Get_Extension_CRC (S: BRBON.Store) return Unsigned_16;


-- -----------------------------------------------------------------------------

   function Get_Extension_Byte_Count (S: BRBON.Store) return Unsigned_8;


-- -----------------------------------------------------------------------------

   procedure Set_Path_Prefix (S: BRBON.Store; Value: String);


-- -----------------------------------------------------------------------------

   function Get_Path_Prefix (S: BRBON.Store) return String;


-- -----------------------------------------------------------------------------

   function Get_Path_Prefix_CRC (S: BRBON.Store) return Unsigned_16;


-- -----------------------------------------------------------------------------

   function Get_Path_Prefix_Byte_Count (S: BRBON.Store) return Unsigned_8;


-- -----------------------------------------------------------------------------

   procedure Set_Acquisition_URL (S: BRBON.Store; Value: String);


-- -----------------------------------------------------------------------------

   function Get_Acquisition_URL (S: BRBON.Store) return String;


-- -----------------------------------------------------------------------------

   function Get_Acquisition_URL_CRC (S: BRBON.Store) return Unsigned_16;


-- -----------------------------------------------------------------------------

   function Get_Acquisition_URL_Byte_Count (S: BRBON.Store) return Unsigned_8;


-- -----------------------------------------------------------------------------

   procedure Set_Public_Key_URL (S: BRBON.Store; Value: String);


-- -----------------------------------------------------------------------------

   function Get_Public_Key_URL (S: BRBON.Store) return String;


-- -----------------------------------------------------------------------------

   function Get_Public_Key_URL_CRC (S: BRBON.Store) return Unsigned_16;


-- -----------------------------------------------------------------------------

   function Get_Public_Key_URL_Byte_Count (S: BRBON.Store) return Unsigned_8;


-- -----------------------------------------------------------------------------

   procedure Set_Creation_Timestamp (S: BRBON.Store; Value: Unsigned_64);


-- -----------------------------------------------------------------------------

   function Get_Creation_Timestamp (S: BRBON.Store) return Unsigned_64;


-- -----------------------------------------------------------------------------

   procedure Set_Modification_Timestamp (S: BRBON.Store; Value: Unsigned_64);


   -----------------------------------------------------------------------------

   function Get_Modification_Timestamp (S: BRBON.Store) return Unsigned_64;


   -----------------------------------------------------------------------------

   procedure Set_Expiry_Timestamp (S: BRBON.Store; Value: Unsigned_64);


   -----------------------------------------------------------------------------

   function Get_Expiry_Timestamp (S: BRBON.Store) return Unsigned_64;


   -----------------------------------------------------------------------------

   function Get_Header_CRC (S: BRBON.Store) return Unsigned_16;


   -----------------------------------------------------------------------------

   procedure Update_Header_CRC (S: BRBON.Store);


   -- ==========================================================================


   -- --------------------------------------------------------------------------

   procedure Set_Header_Synchronization_Bytes (S: Store) is
      HPtr: BRBON.Block.Header_Leading_Ptr := To_Block_Header_Leading_Ptr (S.Data (0)'Access);
   begin
      HPtr.Synchronization_Byte_1 := Synch_Byte_1_Expected_Value;
      HPtr.Synchronization_Byte_2 := Synch_Byte_2_Expected_Value;
      HPtr.Synchronization_Byte_3 := Synch_Byte_3_Expected_Value;
      HPtr.Synchronization_Byte_4 := Synch_Byte_4_LUT (BRBON.Machine_Byte_Storage_Order, S.Swap);
   end Set_Header_Synchronization_Bytes;


   -- --------------------------------------------------------------------------

   function Verify_Header_Synchronization_Bytes (S: Store) return Boolean is
      HPtr: BRBON.Block.Header_Leading_Ptr := To_Block_Header_Leading_Ptr (S.Data (0)'Access);
   begin
      if HPtr.Synchronization_Byte_1 /= Synch_Byte_1_Expected_Value then return false; end if;
      if HPtr.Synchronization_Byte_2 /= Synch_Byte_2_Expected_Value then return false; end if;
      if HPtr.Synchronization_Byte_3 /= Synch_Byte_3_Expected_Value then return false; end if;
      if
        HPtr.Synchronization_Byte_4 /= Synch_Byte_4_MSB_First_Expected_Value
        and then HPtr.Synchronization_Byte_4 /= Synch_Byte_4_LSB_First_Expected_Value
      then
         return false;
      end if;
      return true;
   end Verify_Header_Synchronization_Bytes;


   -- --------------------------------------------------------------------------

   function Get_Endianness (I: in out Instance) return Endianness is
      HPtr: BRBON.Block.Header_Leading_Ptr := To_Block_Header_Leading_Ptr (S.Data (0)'Access);
   begin
      if HPtr.Synchronization_Byte_4 = Synch_Byte_4_MSB_First_Expected_Value then
         return MSB_First;
      else
         if HPtr.Synchronization_Byte_4 = Synch_Byte_4_LSB_First_Expected_Value then
            return LSB_First;
         else
            Ada.Exceptions.Raise_Exception (BRBON.Byte_Storage_Order_Error'Identity, "Byte four in the block header has an illegal value");
         end if;
      end if;
   end Get_Endianness;


   procedure Header_Set_Type (I: in out Instance; Value: Block_Type) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Type_Offset, To_Unsigned_16 (Value));
   end Header_Set_Type;


   function Header_Get_Type (I: in out Instance) return Block_Type is
   begin
      return To_Block_Type (Container.Get_Unsigned_16 (I.Container, Header.Type_Offset));
   end Header_Get_Type;


   procedure Header_Set_Options (I: in out Instance; Value: Block_Options) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Options_Offset, To_Unsigned_16 (Value));
   end Header_Set_Options;


   function Header_Get_Options (I: in out Instance) return Block_Options is
   begin
      return To_Block_Options (Container.Get_Unsigned_16 (I.Container, Header.Options_Offset));
   end Header_Get_Options;


   procedure Header_Set_Byte_Count (I: in out Instance; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (I.Container, Header.Byte_Count_Offset, Value);
   end Header_Set_Byte_Count;


   function Header_Get_Byte_Count (I: in out Instance) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (I.Container, Header.Byte_Count_Offset);
   end Header_Get_Byte_Count;


   procedure Header_Set_Header_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Header_Byte_Count_Offset, Value);
   end Header_Set_Header_Byte_Count;


   function Header_Get_Header_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Header_Byte_Count_Offset);
   end Header_Get_Header_Byte_Count;


   procedure Header_Set_Encrypted_Header_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Encrypted_Header_Byte_Count_Offset, Value);
   end Header_Set_Encrypted_Header_Byte_Count;


   function Header_Get_Encrypted_Header_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Encrypted_Header_Byte_Count_Offset);
   end Header_Get_Encrypted_Header_Byte_Count;


   procedure Header_Set_Origin_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Origin_CRC16_Offset, Value);
   end Header_Set_Origin_Crc16;


   function Header_Get_Origin_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Origin_CRC16_Offset);
   end Header_Get_Origin_Crc16;


   procedure Header_Set_Identifier_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Identifier_CRC16_Offset, Value);
   end Header_Set_Identifier_Crc16;


   function Header_Get_Identifier_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Identifier_CRC16_Offset);
   end Header_Get_Identifier_Crc16;


   procedure Header_Set_Extension_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Extension_CRC16_Offset, Value);
   end Header_Set_Extension_Crc16;


   function Header_Get_Extension_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Extension_CRC16_Offset);
   end Header_Get_Extension_Crc16;


   procedure Header_Set_Path_Prefix_Crc16 (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Path_Prefix_CRC16_Offset, Value);
   end Header_Set_Path_Prefix_Crc16;


   function Header_Get_Path_Prefix_Crc16 (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Path_Prefix_CRC16_Offset);
   end Header_Get_Path_Prefix_Crc16;


   procedure Header_Set_Origin_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (I.Container, Header.Origin_Byte_Count_Offset, Value);
   end Header_Set_Origin_Byte_Count;


   function Header_Get_Origin_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (I.Container, Header.Origin_Byte_Count_Offset);
   end Header_Get_Origin_Byte_Count;


   procedure Header_Set_Identifier_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (I.Container, Header.Identifier_Byte_Count_Offset, Value);
   end Header_Set_Identifier_Byte_Count;


   function Header_Get_Identifier_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (I.Container, Header.Identifier_Byte_Count_Offset);
   end Header_Get_Identifier_Byte_Count;


   procedure Header_Set_Extension_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (I.Container, Header.Extension_Byte_Count_Offset, Value);
   end Header_Set_Extension_Byte_Count;


   function Header_Get_Extension_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (I.Container, Header.Extension_Byte_Count_Offset);
   end Header_Get_Extension_Byte_Count;


   procedure Header_Set_Path_Prefix_Byte_Count (I: in out Instance; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (I.Container, Header.Path_Prefix_Byte_Count_Offset, Value);
   end Header_Set_Path_Prefix_Byte_Count;


   function Header_Get_Path_Prefix_Byte_Count (I: in out Instance) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (I.Container, Header.Path_Prefix_Byte_Count_Offset);
   end Header_Get_Path_Prefix_Byte_Count;


   procedure Header_Set_Origin_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Origin_Offset_Offset, Value);
   end Header_Set_Origin_Offset;


   function Header_Get_Origin_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Origin_Offset_Offset);
   end Header_Get_Origin_Offset;


   procedure Header_Set_Identifier_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Identifier_Offset_Offset, Value);
   end Header_Set_Identifier_Offset;


   function Header_Get_Identifier_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Identifier_Offset_Offset);
   end Header_Get_Identifier_Offset;


   procedure Header_Set_Extension_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Extension_Offset_Offset, Value);
   end Header_Set_Extension_Offset;


   function Header_Get_Extension_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Extension_Offset_Offset);
   end Header_Get_Extension_Offset;


   procedure Header_Set_Path_Prefix_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Path_Prefix_Offset_Offset, Value);
   end Header_Set_Path_Prefix_Offset;


   function Header_Get_Path_Prefix_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Path_Prefix_Offset_Offset);
   end Header_Get_Path_Prefix_Offset;


   procedure Header_Set_Acquisition_URL_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Acquisition_URL_Byte_Count_Offset, Value);
   end Header_Set_Acquisition_URL_Byte_Count;


   function Header_Get_Acquisition_URL_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Acquisition_URL_Byte_Count_Offset);
   end Header_Get_Acquisition_URL_Byte_Count;


   procedure Header_Set_Acquisition_URL_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Acquisition_URL_Offset_Offset, Value);
   end Header_Set_Acquisition_URL_Offset;


   function Header_Get_Acquisition_URL_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Acquisition_URL_Offset_Offset);
   end Header_Get_Acquisition_URL_Offset;


   procedure Header_Set_Target_List_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Target_List_Byte_Count_Offset, Value);
   end Header_Set_Target_List_Byte_Count;


   function Header_Get_Target_List_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Target_List_Byte_Count_Offset);
   end Header_Get_Target_List_Byte_Count;


   procedure Header_Set_Target_List_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Target_List_Offset_Offset, Value);
   end Header_Set_Target_List_Offset;


   function Header_Get_Target_List_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Target_List_Offset_Offset);
   end Header_Get_Target_List_Offset;


   procedure Header_Set_Public_Key_URL_Byte_Count (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Public_Key_URL_Byte_Count_Offset, Value);
   end Header_Set_Public_Key_URL_Byte_Count;


   function Header_Get_Public_Key_URL_Byte_Count (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Public_Key_URL_Byte_Count_Offset);
   end Header_Get_Public_Key_URL_Byte_Count;


   procedure Header_Set_Public_Key_URL_Offset (I: in out Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (I.Container, Header.Public_Key_URL_Offset_Offset, Value);
   end Header_Set_Public_Key_URL_Offset;


   function Header_Get_Public_Key_URL_Offset (I: in out Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (I.Container, Header.Public_Key_URL_Offset_Offset);
   end Header_Get_Public_Key_URL_Offset;


   procedure Header_Set_Creation_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      Container.Set_Unsigned_64 (I.Container, Header.Creation_Timestamp_Offset, Value);
   end Header_Set_Creation_Timestamp;


   function Header_Get_Creation_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (I.Container, Header.Creation_Timestamp_Offset);
   end Header_Get_Creation_Timestamp;


   procedure Header_Set_Modification_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      Container.Set_Unsigned_64 (I.Container, Header.Modification_Timestamp_Offset, Value);
   end Header_Set_Modification_Timestamp;


   function Header_Get_Modification_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (I.Container, Header.Modification_Timestamp_Offset);
   end Header_Get_Modification_Timestamp;


   procedure Header_Set_Expiry_Timestamp (I: in out Instance; Value: Unsigned_64) is
   begin
      Container.Set_Unsigned_64 (I.Container, Header.Expiry_Timestamp_Offset, Value);
   end Header_Set_Expiry_Timestamp;


   function Header_Get_Expiry_Timestamp (I: in out Instance) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (I.Container, Header.Expiry_Timestamp_Offset);
   end Header_Get_Expiry_Timestamp;


   procedure Header_Set_Reserved_1a (I: in out Instance; Value: Unsigned_32) is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1a_Distance_Before_Header_End;
   begin
      Container.Set_Unsigned_32 (I.Container, Offset, Value);
   end Header_Set_Reserved_1a;


   function Header_Get_Reserved_1a (I: in out Instance) return Unsigned_32 is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1a_Distance_Before_Header_End;
   begin
      return Container.Get_Unsigned_32 (I.Container, Offset);
   end Header_Get_Reserved_1a;


   procedure Header_Set_Reserved_1b (I: in out Instance; Value: Unsigned_16) is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1b_Distance_Before_Header_End;
   begin
      Container.Set_Unsigned_16 (I.Container, Offset, Value);
   end Header_Set_Reserved_1b;


   function Header_Get_Reserved_1b (I: in out Instance) return Unsigned_16 is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Reserved_1b_Distance_Before_Header_End;
   begin
      return Container.Get_Unsigned_16 (I.Container, Offset);
   end Header_Get_Reserved_1b;


   procedure Header_Set_Header_Crc16 (I: in out Instance; Value: Unsigned_16) is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Header_CRC16_Distance_Before_Header_End;
   begin
      Container.Set_Unsigned_16 (I.Container, Offset, Value);
   end Header_Set_Header_Crc16;


   function Header_Get_Header_Crc16 (I: in out Instance) return Unsigned_16 is
      Offset: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count) - Header.Header_CRC16_Distance_Before_Header_End;
   begin
      return Container.Get_Unsigned_16 (I.Container, Offset);
   end Header_Get_Header_Crc16;


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
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new origin (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Origin;


   procedure Set_Identifier (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Identifier := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new identifier (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Identifier;


   procedure Set_Extension (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Extension := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new extension (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Extension;


   procedure Set_Path_Prefix (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Path_Prefix := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new path prefix (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Path_Prefix;


   procedure Set_Acquisition_URL (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Acquisition_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new acquisition URL (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Acqquisition_URL;


   procedure Set_Target_List (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Target_List := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new target list (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Target_List;


   procedure Set_Public_Key_URL (I: in out Instance; Value: String) is
      Field_Strings: Field_Storage_Strings := Read_Field_Storage_Strings (I);
   begin
      Field_Strings.Public_Key_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Field_Storage_Strings_Byte_Count (Field_Strings) > I.Header_Get_Header_Byte_Count then
         Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "Not enough space available in block header field storage for new public key URL (" & Value & ")");
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
         return Container.Get_String (I.Container, Offset, Byte_Count);
      end if;
   end Get_Public_Key_URL;


   --


   function Byte_Count (I: in out Instance) return Unsigned_32 is
   begin
      return I.Header_Get_Byte_Count;
   end Byte_Count;


   procedure Update_Block_CRC (I: in out Instance) is
      BC: constant Unsigned_32 := I.Header_Get_Byte_Count;
      Crc: constant Unsigned_32 := Container.Get_CRC_32_Over_Range (I.Container, Start => 0, Count => BC - 4);
   begin
      Container.Set_Unsigned_32 (I.Container, Offset => BC - 4, Value => Crc);
   end Update_Block_CRC;


   procedure Update_Header_CRC (I: in out Instance) is
      HC: Unsigned_32 := Unsigned_32 (I.Header_Get_Header_Byte_Count);
      Crc: Unsigned_16 := Container.Get_CRC_16_Over_Range (I.Container, Start => 0, Count => HC);
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
      Container.Write_To_File (I.Container, To_Path);
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

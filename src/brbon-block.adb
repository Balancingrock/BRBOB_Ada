with Interfaces; use Interfaces;

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with CRC_Package;

with BRBON; use BRBON;
with BRBON.Container;





package body BRBON.Block is


   -- ==========================================================================
   -- Internal definitions
   -- ==========================================================================

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

   function FSS_Upper_Limit (S: Store) return Unsigned_16 is
   begin
      case Get_Block_Type (S) is
         when Single_Item_Block =>
            return Get_Header_Byte_Count (S) - Block_Header_Leading_Byte_Count - Block_Header_Trailing_Byte_Count;
         when others =>
            Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Cannot calculate FSS_Upper_Limit for not-supported block types");
      end case;
   end FSS_Upper_Limit;


   -----------------------------------------------------------------------------

   function Read_Field_Storage_Strings (S: Store) return Field_Storage_Strings is
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

   procedure Write_Field_Storage_Strings (S: Store; Strings: Field_Storage_Strings) is

      HPtr: Block_Header_Leading_Ptr := Get_Block_Header_Leading_Ptr (S);
      Offset: Unsigned_16 := Block_Header_Leading_Byte_Count;
      Free_Bytes: Unsigned_16 := Get_Header_Byte_Count (S) - Offset - Block_Header_Trailing_Byte_Count;

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
         Set_Offset (S, Offset);
         Set_Byte_Count (S, Unsigned_8 (Str'Length));
         if Str'Length > 0 then
            if Str'Length > Free_Bytes then
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "String length exceeds available area");
            else
               Container.Set_String (S, Unsigned_32 (Offset), Str);
               Offset := Offset + Unsigned_16 (Str'Length);
               Free_Bytes := Free_Bytes - Unsigned_16 (Str'Length);
            end if;
            Set_CRC_16 (S, CRC_Package.Calculate_CRC_16 (Str));
         else
            Set_CRC_16 (S, 0);
            Set_Offset (S, 0);
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
         Set_Offset (S, Offset);
         Set_Byte_Count (S, Unsigned_16 (Str'Length));
         if Str'Length > 0 then
            if Str'Length > Free_Bytes then
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "String length exceeds available area");
            else
               Container.Set_String (S, Unsigned_32 (Offset), Str);
               Offset := Offset + Unsigned_16 (Str'Length);
               Free_Bytes := Free_Bytes - Unsigned_16 (Str'Length);
            end if;
         else
            Set_Offset (S, 0);
         end if;
      end Add;

   begin

      Add (Strings.Origin, Set_Origin_Offset'Access, Set_Origin_Byte_Count'Access, Set_Origin_CRC'Access);
      Add (Strings.Identifier, Set_Identifier_Offset'Access, Set_Identifier_Byte_Count'Access, Set_Identifier_CRC'Access);
      Add (Strings.Extension, Set_Extension_Offset'Access, Set_Extension_Byte_Count'Access, Set_Extension_CRC'Access);
      Add (Strings.Path_Prefix, Set_Path_Prefix_Offset'Access, Set_Path_Prefix_Byte_Count'Access, Set_Path_Prefix_CRC'Access);

      Add (Strings.Acquisition_URL, Set_Acquisition_URL_Offset'Access, Set_Acquisition_URL_Byte_Count'Access);
      Add (Strings.Target_List, Set_Target_List_Offset'Access, Set_Target_List_Byte_Count'Access);
      Add (Strings.Public_Key_URL, Set_Public_Key_URL_Offset'Access, Set_Public_Key_URL_Byte_Count'Access);

   end Write_Field_Storage_Strings;


   -----------------------------------------------------------------------------

   function Byte_Count (F: Field_Storage_Strings) return Unsigned_16 is
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
   end Byte_Count;

   -----------------------------------------------------------------------------

   function Get_Byte_Storage_Order (S: Store) return Byte_Storage_Order is
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



   -- ==========================================================================
   -- Header field access
   -- ==========================================================================

   -----------------------------------------------------------------------------

   procedure Set_Header_Synchronization_Bytes (S: Store) is
      HPtr: Block_Header_Leading_Ptr := Get_Block_Header_Leading_Ptr (S);
   begin
      HPtr.Synchronization_Byte_1 := Synch_Byte_1_Expected_Value;
      HPtr.Synchronization_Byte_2 := Synch_Byte_2_Expected_Value;
      HPtr.Synchronization_Byte_3 := Synch_Byte_3_Expected_Value;
      HPtr.Synchronization_Byte_4 := Synch_Byte_4_LUT (BRBON.Machine_Byte_Storage_Order, S.Swap);
   end Set_Header_Synchronization_Bytes;


   -----------------------------------------------------------------------------

   function Get_Block_Type (S: Store) return Block_Type is
   begin
      return To_Block_Header_Leading_Ptr (S.Data (0)'Access).Is_Type;
   end Get_Block_Type;

   procedure Set_Block_Type (S: Store; Value: Block_Type) is
   begin
      Get_Block_Header_Leading_Ptr (S).Is_Type := Value;
   end Set_Block_Type;


   -----------------------------------------------------------------------------

   procedure Set_Block_Options (S: Store; Value: BRBON.Block_Options) is
   begin
      Get_Block_Header_Leading_Ptr (S).Options := Value;
   end Set_Block_Options;

   function Get_Block_Options (S: Store) return BRBON.Block_Options is
   begin
      return Get_Block_Header_Leading_Ptr (S).Options;
   end Get_Block_Options;


   -----------------------------------------------------------------------------

   function Get_Block_Byte_Count (S: Store) return Unsigned_32 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Block_Byte_Count;
   end Get_Block_Byte_Count;

   procedure Set_Block_Byte_Count (S: Store; Value: Unsigned_32) is
   begin
      Get_Block_Header_Leading_Ptr (s).Block_Byte_Count := Value;
   end Set_Block_Byte_Count;


   -----------------------------------------------------------------------------


   function Get_Header_Byte_Count (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Header_Byte_Count;
   end Get_Header_Byte_Count;

   procedure Set_Header_Byte_Count (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Header_Byte_Count := Value;
   end Set_Header_Byte_Count;

   -----------------------------------------------------------------------------

   function Get_Encrypted_Header_Byte_Count (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Encrypted_Header_Byte_Count;
   end Get_Encrypted_Header_Byte_Count;

   procedure Set_Encrypted_Header_Byte_Count (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Encrypted_Header_Byte_Count := Value;
   end Set_Encrypted_Header_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Origin_CRC (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Origin_CRC;
   end Get_Origin_CRC;

   procedure Set_Origin_CRC (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Origin_CRC := Value;
   end Set_Origin_CRC;


   -----------------------------------------------------------------------------

   function Get_Identifier_CRC (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Identifier_CRC;
   end Get_Identifier_CRC;

   procedure Set_Identifier_CRC (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Identifier_CRC := Value;
   end Set_Identifier_CRC;


   -----------------------------------------------------------------------------

   function Get_Extension_CRC (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Extension_CRC;
   end Get_Extension_CRC;

   procedure Set_Extension_CRC (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Extension_CRC := Value;
   end Set_Extension_CRC;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix_CRC (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Path_Prefix_CRC;
   end Get_Path_Prefix_CRC;

   procedure Set_Path_Prefix_CRC (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Path_Prefix_CRC := Value;
   end Set_Path_Prefix_CRC;


   -----------------------------------------------------------------------------

   function Get_Origin_Byte_Count (S: Store) return Unsigned_8 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Origin_Byte_Count;
   end Get_Origin_Byte_Count;

   procedure Set_Origin_Byte_Count (S: Store; Value: Unsigned_8) is
   begin
      Get_Block_Header_Leading_Ptr (S).Origin_Byte_Count := Value;
   end Set_Origin_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Identifier_Byte_Count (S: Store) return Unsigned_8 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Identifier_Byte_Count;
   end Get_Identifier_Byte_Count;

   procedure Set_Identifier_Byte_Count (S: Store; Value: Unsigned_8) is
   begin
      Get_Block_Header_Leading_Ptr (S).Identifier_Byte_Count := Value;
   end Set_Identifier_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Extension_Byte_Count (S: Store) return Unsigned_8 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Extension_Byte_Count;
   end Get_Extension_Byte_Count;

   procedure Set_Extension_Byte_Count (S: Store; Value: Unsigned_8) is
   begin
      Get_Block_Header_Leading_Ptr (S).Extension_Byte_Count := Value;
   end Set_Extension_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix_Byte_Count (S: Store) return Unsigned_8 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Path_Prefix_Byte_Count;
   end Get_Path_Prefix_Byte_Count;

   procedure Set_Path_Prefix_Byte_Count (S: Store; Value: Unsigned_8) is
   begin
      Get_Block_Header_Leading_Ptr (S).Path_Prefix_Byte_Count := Value;
   end Set_Path_Prefix_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Origin_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Origin_Offset;
   end Get_Origin_Offset;

   procedure Set_Origin_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Origin_Offset := Value;
   end Set_Origin_Offset;


   -----------------------------------------------------------------------------

   function Get_Identifier_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Identifier_Offset;
   end Get_Identifier_Offset;

   procedure Set_Identifier_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Identifier_Offset := Value;
   end Set_Identifier_Offset;


   -----------------------------------------------------------------------------

   function Get_Extension_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Extension_Offset;
   end Get_Extension_Offset;

   procedure Set_Extension_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Extension_Offset := Value;
   end Set_Extension_Offset;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Path_Prefix_Offset;
   end Get_Path_Prefix_Offset;

   procedure Set_Path_Prefix_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Path_Prefix_Offset := Value;
   end Set_Path_Prefix_Offset;


   -----------------------------------------------------------------------------

   function Get_Acquisition_URL_Byte_Count (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Acquisition_URL_Byte_Count;
   end Get_Acquisition_URL_Byte_Count;

   procedure Set_Acquisition_URL_Byte_Count (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Acquisition_URL_Byte_Count := Value;
   end Set_Acquisition_URL_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Acquisition_URL_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Acquisition_URL_Offset;
   end Get_Acquisition_URL_Offset;

   procedure Set_Acquisition_URL_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Acquisition_URL_Offset := Value;
   end Set_Acquisition_URL_Offset;


   -----------------------------------------------------------------------------

   function Get_Target_List_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Target_List_Offset;
   end Get_Target_List_Offset;

   procedure Set_Target_List_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Target_List_Offset := Value;
   end Set_Target_List_Offset;


   -----------------------------------------------------------------------------

   function Get_Target_List_Byte_Count (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Target_List_Byte_Count;
   end Get_Target_List_Byte_Count;

   procedure Set_Target_List_Byte_Count (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Target_List_Byte_Count := Value;
   end Set_Target_List_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Public_Key_URL_Byte_Count (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Public_Key_URL_Byte_Count;
   end Get_Public_Key_URL_Byte_Count;

   procedure Set_Public_Key_URL_Byte_Count (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Public_Key_URL_Byte_Count := Value;
   end Set_Public_Key_URL_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Public_Key_URL_Offset (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Leading_Ptr (S).Public_Key_URL_Offset;
   end Get_Public_Key_URL_Offset;

   procedure Set_Public_Key_URL_Offset (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Leading_Ptr (S).Public_Key_URL_Offset := Value;
   end Set_Public_Key_URL_Offset;


   -----------------------------------------------------------------------------

   function Get_Creation_Timestamp (S: Store) return Timestamp is
   begin
      return Get_Block_Header_Leading_Ptr (S).Creation_Timestamp;
   end Get_Creation_Timestamp;

   procedure Set_Creation_Timestamp (S: Store; Value: Timestamp) is
   begin
      Get_Block_Header_Leading_Ptr (S).Creation_Timestamp := Value;
   end Set_Creation_Timestamp;


   -----------------------------------------------------------------------------

   function Get_Modification_Timestamp (S: Store) return Timestamp is
   begin
      return Get_Block_Header_Leading_Ptr (S).Modification_Timestamp;
   end Get_Modification_Timestamp;

   procedure Set_Modification_Timestamp (S: Store; Value: Timestamp) is
   begin
      Get_Block_Header_Leading_Ptr (S).Modification_Timestamp := Value;
   end Set_Modification_Timestamp;


   -----------------------------------------------------------------------------

   function Get_Expiry_Timestamp (S: Store) return Timestamp is
   begin
      return Get_Block_Header_Leading_Ptr (S).Expiry_Timestamp;
   end Get_Expiry_Timestamp;

   procedure Set_Expiry_Timestamp (S: Store; Value: Timestamp) is
   begin
      Get_Block_Header_Leading_Ptr (S).Expiry_Timestamp := Value;
   end Set_Expiry_Timestamp;


   -- ==========================================================================
   -- Higher level block APIs
   -- ==========================================================================


   procedure Set_Origin (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified origin string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Origin := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new origin string cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Origin;


   -----------------------------------------------------------------------------

   function Get_Origin (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Origin_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Origin_Offset (S)), Byte_Count);
      end if;
   end Get_Origin;


   -----------------------------------------------------------------------------

   procedure Set_Identifier (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified identifier string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Identifier := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new identifier string cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Identifier;


   -----------------------------------------------------------------------------

   function Get_Identifier (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Identifier_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Identifier_Offset (S)), Byte_Count);
      end if;
   end Get_Identifier;


   -----------------------------------------------------------------------------

   procedure Set_Extension (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified extension string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Extension := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension string cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Extension;


-- -----------------------------------------------------------------------------

   function Get_Extension (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Extension_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Extension_Offset (S)), Byte_Count);
      end if;
   end Get_Extension;


   -----------------------------------------------------------------------------

   procedure Set_Path_Prefix (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified path prefix string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Path_Prefix := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension path prefix cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Path_Prefix;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Path_Prefix_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Path_Prefix_Offset (S)), Byte_Count);
      end if;
   end Get_Path_Prefix;


   -----------------------------------------------------------------------------

   procedure Set_Acquisition_URL (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 2550 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified acquisition url is too long, expected < 2550, found: " & Value'Length'Image);
      end if;
      FSS.Acquisition_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension acquisition url cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Acquisition_URL;


   -----------------------------------------------------------------------------

   function Get_Acquisition_URL (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Acquisition_URL_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Acquisition_URL_Offset (S)), Byte_Count);
      end if;
   end Get_Acquisition_URL;


   -----------------------------------------------------------------------------

   procedure Set_Target_List (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 2550 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified public key url is too long, expected < 2550, found: " & Value'Length'Image);
      end if;
      FSS.Target_List := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension public key url cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Target_List;


   -----------------------------------------------------------------------------

   function Get_Target_List (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Target_List_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Target_List_Offset (S)), Byte_Count);
      end if;
   end Get_Target_List;


   -----------------------------------------------------------------------------

   procedure Set_Public_Key_URL (S: Store; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (S);
   begin
      if Value'Length > 2550 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified public key url is too long, expected < 2550, found: " & Value'Length'Image);
      end if;
      FSS.Public_Key_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > FSS_Upper_Limit (S) then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension public key url cannot be fitted into the available space");
      end if;
      Write_Field_Storage_Strings (S, FSS);
   end Set_Public_Key_URL;


   -----------------------------------------------------------------------------

   function Get_Public_Key_URL (S: Store) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (Get_Public_Key_URL_Byte_Count (S));
   begin
      if Byte_Count = 0 then
         return "";
      else
         return Container.Get_String (S, Unsigned_32 (Get_Public_Key_URL_Offset (S)), Byte_Count);
      end if;
   end Get_Public_Key_URL;



   -----------------------------------------------------------------------------

   function Get_Header_CRC (S: Store) return Unsigned_16 is
   begin
      raise Implementation;
      return 0;
   end Get_Header_CRC;


   -----------------------------------------------------------------------------

   procedure Update_Header_CRC (S: Store) is
   begin
      raise Implementation;
   end Update_Header_CRC;


   -- --------------------------------------------------------------------------

   function Verify_Header_Synchronization_Bytes (S: Store) return Boolean is
      HPtr: Block_Header_Leading_Ptr := Get_Block_Header_Leading_Ptr (S);
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




   procedure Setup (S: Store; For_Byte_Storage_Order: Byte_Storage_Order; With_Field_Storage_Byte_Count: Unsigned_16) is
   begin
      raise Implementation;
   end Setup;


   -- ========================================================
   -- Child support operations
   -- ========================================================

   procedure Create_Single_Item_Block_Header
     (
      In_Store: Store;
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
     ) is

      S: Store renames In_Store;

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

      Set_Header_Synchronization_Bytes (S);
      Set_Block_Type (S, Single_Item_Block);
      Set_Block_Options (S, No_Block_Options);

      Set_Block_Byte_Count (S, S.Data.all'Length);
      Set_Header_Byte_Count (S, Header_Byte_Count);
      Set_Encrypted_Header_Byte_Count (S, 0);

      Write_Field_Storage_Strings (S, FS);

      Set_Creation_Timestamp (S, Creation_Timestamp);
      Set_Modification_Timestamp (S, Creation_Timestamp);
      Set_Expiry_Timestamp (S, Expiry_Timestamp);

      Set_Block_Header_Trailing_Reserved_1 (S, 0);
      Set_Block_Header_Trailing_Reserved_1 (S, 0);
      Update_Header_CRC (S);

   end Create_Single_Item_Block_Header;


   function Get_Block_Header_Trailing_Reserved_1 (S: Store) return Unsigned_32 is
   begin
      return Get_Block_Header_Trailing_Ptr (S).Reserved_1;
   end Get_Block_Header_Trailing_Reserved_1;


   function Get_Block_Header_Trailing_Reserved_2 (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Trailing_Ptr (S).Reserved_2;
   end Get_Block_Header_Trailing_Reserved_2;


   function Get_Block_Header_CRC (S: Store) return Unsigned_16 is
   begin
      return Get_Block_Header_Trailing_Ptr (S).CRC;
   end Get_Block_Header_CRC;


   procedure Set_Block_Header_Trailing_Reserved_1 (S: Store; Value: Unsigned_32) is
   begin
      Get_Block_Header_Trailing_Ptr (S).Reserved_1 := Value;
   end Set_Block_Header_Trailing_Reserved_1;


   procedure Set_Block_Header_Trailing_Reserved_2 (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Trailing_Ptr (S).Reserved_2 := Value;
   end Set_Block_Header_Trailing_Reserved_2;


   procedure Set_Block_Header_CRC (S: Store; Value: Unsigned_16) is
   begin
      Get_Block_Header_Trailing_Ptr (S).CRC := Value;
   end Set_Block_Header_CRC;


   -- =====================================================
   -- Block API
   -- ======================================================


   function Test_Support_Serializer (S: Store) return Serializable.Instance is
   begin
      return Serializable.Create_Without_Copy (Use_In_Place => S.Data,
                                               First        => S.Data'First,
                                               Last         => S.Data'Last);
   end Test_Support_Serializer;

--   procedure Ensure_Block_Consistency (I: in out Instance) is
--   begin
--         I.Update_Header_CRC;
--         I.Update_Block_CRC;
--   end Ensure_Block_Consistency;


end BRBON.Block;

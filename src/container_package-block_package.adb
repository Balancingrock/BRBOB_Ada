with Interfaces; use Interfaces;

with Ada.Exceptions;
with Ada.Strings.Unbounded;

with CRC_Package;

with BRBON; use BRBON;
with Container_Package; use Container_Package;


package body Container_Package.Block_Package is


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

   function FSS_Upper_Limit (B: Block) return Unsigned_16 is
   begin
      case B.Get_Block_Type is
         when Single_Item_Block =>
            return B.Get_Header_Byte_Count - Block_Header_Leading_Byte_Count - Block_Header_Trailing_Byte_Count;
         when others =>
            Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Cannot calculate FSS_Upper_Limit for not-supported block types");
      end case;
   end FSS_Upper_Limit;


   -----------------------------------------------------------------------------

   function Read_Field_Storage_Strings (B: Block) return Field_Storage_Strings is
   begin
      return
        (
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Origin),
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Identifier),
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Extension),
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Path_Prefix),
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Acquisition_URL),
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Target_List),
         Ada.Strings.Unbounded.To_Unbounded_String (B.Get_Public_Key_Url)
        );
   end Read_Field_Storage_Strings;


   -----------------------------------------------------------------------------

   procedure Write_Field_Storage_Strings (B: Block; Strings: Field_Storage_Strings) is

      HPtr: Block_Header_Leading_Ptr := B.Get_Block_Header_Leading_Ptr;
      Offset: Unsigned_16 := Block_Header_Leading_Byte_Count;
      Free_Bytes: Unsigned_16 := B.Get_Header_Byte_Count - Offset - Block_Header_Trailing_Byte_Count;

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
         Set_Offset (B, Offset);
         Set_Byte_Count (B, Unsigned_8 (Str'Length));
         if Str'Length > 0 then
            if Str'Length > Free_Bytes then
               Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "String length exceeds available area");
            else
               B.Set_String (Unsigned_32 (Offset), Str);
               Offset := Offset + Unsigned_16 (Str'Length);
               Free_Bytes := Free_Bytes - Unsigned_16 (Str'Length);
            end if;
            Set_CRC_16 (B, CRC_Package.Calculate_CRC_16 (Str));
         else
            Set_CRC_16 (B, 0);
            Set_Offset (B, 0);
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
         Set_Offset (B, Offset);
         Set_Byte_Count (B, Unsigned_16 (Str'Length));
         if Str'Length > 0 then
            if Str'Length > Free_Bytes then
               Ada.Exceptions.Raise_Exception (BRBON.Storage_Warning'Identity, "String length exceeds available area");
            else
               B.Set_String (Unsigned_32 (Offset), Str);
               Offset := Offset + Unsigned_16 (Str'Length);
               Free_Bytes := Free_Bytes - Unsigned_16 (Str'Length);
            end if;
         else
            Set_Offset (B, 0);
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

   function Factory (Memory_Ptr: Unsigned_8_Array_Ptr; Byte_Order: Byte_Storage_Order) return Block is
      B: Block;
   begin
      Container (B) := Container_Package.Factory (Memory_Ptr, Byte_Order);
      --Container (B).Setup (Memory_Ptr, Byte_Order, False);
      return B;
   end Factory;


   -----------------------------------------------------------------------------

   function Factory (Byte_Count: Unsigned_32; Byte_Order: Byte_Storage_Order) return Block is
      B: Block;
   begin
      Container (B) := Container_Package.Factory (Byte_Count, Byte_Order);
      return B;
   end Factory;


   -----------------------------------------------------------------------------

   function Factory (Filepath: String) return Block is
      B: Block;
   begin
      Container (B) := Container_Package.Factory (Filepath);
      B.Swap := B.Get_Byte_Storage_Order /= Machine_Byte_Storage_Order;
      Ada.Exceptions.Raise_Exception (Implementation'Identity, "Still need to set the other block parameters");
      return B;
   end Factory;


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

   function Get_Byte_Storage_Order (B: Block) return Byte_Storage_Order is
   begin
      if B.Swap then
         if Machine_Byte_Storage_Order = MSB_First then
            return LSB_First;
         else
            return MSB_First;
         end if;
      else
         return Machine_Byte_Storage_Order;
      end if;
   end Get_Byte_Storage_Order;



   -- ==========================================================================
   -- Header field access
   -- ==========================================================================

   -----------------------------------------------------------------------------

   procedure Set_Header_Synchronization_Bytes (B: Block) is
      HPtr: Block_Header_Leading_Ptr := B.Get_Block_Header_Leading_Ptr;
   begin
      HPtr.Synchronization_Byte_1 := Synch_Byte_1_Expected_Value;
      HPtr.Synchronization_Byte_2 := Synch_Byte_2_Expected_Value;
      HPtr.Synchronization_Byte_3 := Synch_Byte_3_Expected_Value;
      HPtr.Synchronization_Byte_4 := Synch_Byte_4_LUT (Machine_Byte_Storage_Order, B.Swap);
   end Set_Header_Synchronization_Bytes;


   -----------------------------------------------------------------------------

   function Get_Block_Type (B: Block) return Block_Type is
   begin
      return B.Get_Block_Header_Leading_Ptr.Is_Type;
   end Get_Block_Type;

   procedure Set_Block_Type (B: Block; Value: Block_Type) is
   begin
      B.Get_Block_Header_Leading_Ptr.Is_Type := Value;
   end Set_Block_Type;


   -----------------------------------------------------------------------------

   procedure Set_Block_Options (B: Block; Value: BRBON.Block_Options) is
   begin
      B.Get_Block_Header_Leading_Ptr.Options := Value;
   end Set_Block_Options;

   function Get_Block_Options (B: Block) return BRBON.Block_Options is
   begin
      return B.Get_Block_Header_Leading_Ptr.Options;
   end Get_Block_Options;


   -----------------------------------------------------------------------------

   function Get_Block_Byte_Count (B: Block) return Unsigned_32 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Block_Byte_Count;
   end Get_Block_Byte_Count;

   procedure Set_Block_Byte_Count (B: Block; Value: Unsigned_32) is
   begin
      B.Get_Block_Header_Leading_Ptr.Block_Byte_Count := Value;
   end Set_Block_Byte_Count;


   -----------------------------------------------------------------------------


   function Get_Header_Byte_Count (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Header_Byte_Count;
   end Get_Header_Byte_Count;

   procedure Set_Header_Byte_Count (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Header_Byte_Count := Value;
   end Set_Header_Byte_Count;

   -----------------------------------------------------------------------------

   function Get_Encrypted_Header_Byte_Count (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Encrypted_Header_Byte_Count;
   end Get_Encrypted_Header_Byte_Count;

   procedure Set_Encrypted_Header_Byte_Count (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Encrypted_Header_Byte_Count := Value;
   end Set_Encrypted_Header_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Origin_CRC (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Origin_CRC;
   end Get_Origin_CRC;

   procedure Set_Origin_CRC (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Origin_CRC := Value;
   end Set_Origin_CRC;


   -----------------------------------------------------------------------------

   function Get_Identifier_CRC (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Identifier_CRC;
   end Get_Identifier_CRC;

   procedure Set_Identifier_CRC (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Identifier_CRC := Value;
   end Set_Identifier_CRC;


   -----------------------------------------------------------------------------

   function Get_Extension_CRC (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Extension_CRC;
   end Get_Extension_CRC;

   procedure Set_Extension_CRC (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Extension_CRC := Value;
   end Set_Extension_CRC;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix_CRC (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Path_Prefix_CRC;
   end Get_Path_Prefix_CRC;

   procedure Set_Path_Prefix_CRC (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Path_Prefix_CRC := Value;
   end Set_Path_Prefix_CRC;


   -----------------------------------------------------------------------------

   function Get_Origin_Byte_Count (B: Block) return Unsigned_8 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Origin_Byte_Count;
   end Get_Origin_Byte_Count;

   procedure Set_Origin_Byte_Count (B: Block; Value: Unsigned_8) is
   begin
      B.Get_Block_Header_Leading_Ptr.Origin_Byte_Count := Value;
   end Set_Origin_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Identifier_Byte_Count (B: Block) return Unsigned_8 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Identifier_Byte_Count;
   end Get_Identifier_Byte_Count;

   procedure Set_Identifier_Byte_Count (B: Block; Value: Unsigned_8) is
   begin
      B.Get_Block_Header_Leading_Ptr.Identifier_Byte_Count := Value;
   end Set_Identifier_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Extension_Byte_Count (B: Block) return Unsigned_8 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Extension_Byte_Count;
   end Get_Extension_Byte_Count;

   procedure Set_Extension_Byte_Count (B: Block; Value: Unsigned_8) is
   begin
      B.Get_Block_Header_Leading_Ptr.Extension_Byte_Count := Value;
   end Set_Extension_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix_Byte_Count (B: Block) return Unsigned_8 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Path_Prefix_Byte_Count;
   end Get_Path_Prefix_Byte_Count;

   procedure Set_Path_Prefix_Byte_Count (B: Block; Value: Unsigned_8) is
   begin
      B.Get_Block_Header_Leading_Ptr.Path_Prefix_Byte_Count := Value;
   end Set_Path_Prefix_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Origin_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Origin_Offset;
   end Get_Origin_Offset;

   procedure Set_Origin_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Origin_Offset := Value;
   end Set_Origin_Offset;


   -----------------------------------------------------------------------------

   function Get_Identifier_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Identifier_Offset;
   end Get_Identifier_Offset;

   procedure Set_Identifier_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Identifier_Offset := Value;
   end Set_Identifier_Offset;


   -----------------------------------------------------------------------------

   function Get_Extension_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Extension_Offset;
   end Get_Extension_Offset;

   procedure Set_Extension_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Extension_Offset := Value;
   end Set_Extension_Offset;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Path_Prefix_Offset;
   end Get_Path_Prefix_Offset;

   procedure Set_Path_Prefix_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Path_Prefix_Offset := Value;
   end Set_Path_Prefix_Offset;


   -----------------------------------------------------------------------------

   function Get_Acquisition_URL_Byte_Count (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Acquisition_URL_Byte_Count;
   end Get_Acquisition_URL_Byte_Count;

   procedure Set_Acquisition_URL_Byte_Count (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Acquisition_URL_Byte_Count := Value;
   end Set_Acquisition_URL_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Acquisition_URL_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Acquisition_URL_Offset;
   end Get_Acquisition_URL_Offset;

   procedure Set_Acquisition_URL_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Acquisition_URL_Offset := Value;
   end Set_Acquisition_URL_Offset;


   -----------------------------------------------------------------------------

   function Get_Target_List_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Target_List_Offset;
   end Get_Target_List_Offset;

   procedure Set_Target_List_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Target_List_Offset := Value;
   end Set_Target_List_Offset;


   -----------------------------------------------------------------------------

   function Get_Target_List_Byte_Count (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Target_List_Byte_Count;
   end Get_Target_List_Byte_Count;

   procedure Set_Target_List_Byte_Count (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Target_List_Byte_Count := Value;
   end Set_Target_List_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Public_Key_URL_Byte_Count (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Public_Key_URL_Byte_Count;
   end Get_Public_Key_URL_Byte_Count;

   procedure Set_Public_Key_URL_Byte_Count (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Public_Key_URL_Byte_Count := Value;
   end Set_Public_Key_URL_Byte_Count;


   -----------------------------------------------------------------------------

   function Get_Public_Key_URL_Offset (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Leading_Ptr.Public_Key_URL_Offset;
   end Get_Public_Key_URL_Offset;

   procedure Set_Public_Key_URL_Offset (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Leading_Ptr.Public_Key_URL_Offset := Value;
   end Set_Public_Key_URL_Offset;


   -----------------------------------------------------------------------------

   function Get_Creation_Timestamp (B: Block) return Timestamp is
   begin
      return B.Get_Block_Header_Leading_Ptr.Creation_Timestamp;
   end Get_Creation_Timestamp;

   procedure Set_Creation_Timestamp (B: Block; Value: Timestamp) is
   begin
      B.Get_Block_Header_Leading_Ptr.Creation_Timestamp := Value;
   end Set_Creation_Timestamp;


   -----------------------------------------------------------------------------

   function Get_Modification_Timestamp (B: Block) return Timestamp is
   begin
      return B.Get_Block_Header_Leading_Ptr.Modification_Timestamp;
   end Get_Modification_Timestamp;

   procedure Set_Modification_Timestamp (B: Block; Value: Timestamp) is
   begin
      B.Get_Block_Header_Leading_Ptr.Modification_Timestamp := Value;
   end Set_Modification_Timestamp;


   -----------------------------------------------------------------------------

   function Get_Expiry_Timestamp (B: Block) return Timestamp is
   begin
      return B.Get_Block_Header_Leading_Ptr.Expiry_Timestamp;
   end Get_Expiry_Timestamp;

   procedure Set_Expiry_Timestamp (B: Block; Value: Timestamp) is
   begin
      B.Get_Block_Header_Leading_Ptr.Expiry_Timestamp := Value;
   end Set_Expiry_Timestamp;


   -- ==========================================================================
   -- Higher level block APIs
   -- ==========================================================================


   procedure Set_Origin (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified origin string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Origin := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new origin string cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Origin;


   -----------------------------------------------------------------------------

   function Get_Origin (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Origin_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Origin_Offset), Byte_Count);
      end if;
   end Get_Origin;


   -----------------------------------------------------------------------------

   procedure Set_Identifier (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified identifier string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Identifier := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new identifier string cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Identifier;


   -----------------------------------------------------------------------------

   function Get_Identifier (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Identifier_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Identifier_Offset), Byte_Count);
      end if;
   end Get_Identifier;


   -----------------------------------------------------------------------------

   procedure Set_Extension (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified extension string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Extension := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension string cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Extension;


-- -----------------------------------------------------------------------------

   function Get_Extension (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Extension_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Extension_Offset), Byte_Count);
      end if;
   end Get_Extension;


   -----------------------------------------------------------------------------

   procedure Set_Path_Prefix (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 255 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified path prefix string is too long, expected < 255, found: " & Value'Length'Image);
      end if;
      FSS.Path_Prefix := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension path prefix cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Path_Prefix;


   -----------------------------------------------------------------------------

   function Get_Path_Prefix (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Path_Prefix_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Path_Prefix_Offset), Byte_Count);
      end if;
   end Get_Path_Prefix;


   -----------------------------------------------------------------------------

   procedure Set_Acquisition_URL (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 2550 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified acquisition url is too long, expected < 2550, found: " & Value'Length'Image);
      end if;
      FSS.Acquisition_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension acquisition url cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Acquisition_URL;


   -----------------------------------------------------------------------------

   function Get_Acquisition_URL (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Acquisition_URL_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Acquisition_URL_Offset), Byte_Count);
      end if;
   end Get_Acquisition_URL;


   -----------------------------------------------------------------------------

   procedure Set_Target_List (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 2550 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified public key url is too long, expected < 2550, found: " & Value'Length'Image);
      end if;
      FSS.Target_List := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension public key url cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Target_List;


   -----------------------------------------------------------------------------

   function Get_Target_List (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Target_List_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Target_List_Offset), Byte_Count);
      end if;
   end Get_Target_List;


   -----------------------------------------------------------------------------

   procedure Set_Public_Key_URL (B: Block; Value: String) is
      FSS: Field_Storage_Strings := B.Read_Field_Storage_Strings;
   begin
      if Value'Length > 2550 then
         Ada.Exceptions.Raise_Exception (String_Too_Long'Identity, "The specified public key url is too long, expected < 2550, found: " & Value'Length'Image);
      end if;
      FSS.Public_Key_URL := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      if Byte_Count (FSS) > B.FSS_Upper_Limit then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "The new extension public key url cannot be fitted into the available space");
      end if;
      B.Write_Field_Storage_Strings (FSS);
   end Set_Public_Key_URL;


   -----------------------------------------------------------------------------

   function Get_Public_Key_URL (B: Block) return String is
      Byte_Count: Unsigned_32 := Unsigned_32 (B.Get_Public_Key_URL_Byte_Count);
   begin
      if Byte_Count = 0 then
         return "";
      else
         return B.Get_String (Unsigned_32 (B.Get_Public_Key_URL_Offset), Byte_Count);
      end if;
   end Get_Public_Key_URL;



   -----------------------------------------------------------------------------

   function Get_Header_CRC (B: Block) return Unsigned_16 is
   begin
      raise Implementation;
      return 0;
   end Get_Header_CRC;


   -----------------------------------------------------------------------------

   procedure Update_Header_CRC (B: Block) is
   begin
      raise Implementation;
   end Update_Header_CRC;


   -- --------------------------------------------------------------------------

   function Verify_Header_Synchronization_Bytes (B: Block) return Boolean is
      HPtr: Block_Header_Leading_Ptr := B.Get_Block_Header_Leading_Ptr;
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




   procedure Setup (B: Block; For_Byte_Storage_Order: Byte_Storage_Order; With_Field_Storage_Byte_Count: Unsigned_16) is
   begin
      raise Implementation;
   end Setup;


   -- ========================================================
   -- Child support operations
   -- ========================================================

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
     ) is

      B: Block renames In_Block;

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

      B.Set_Header_Synchronization_Bytes;
      B.Set_Block_Type (Single_Item_Block);
      B.Set_Block_Options (No_Block_Options);

      B.Set_Block_Byte_Count (B.MPtr.all'Length);
      B.Set_Header_Byte_Count (Header_Byte_Count);
      B.Set_Encrypted_Header_Byte_Count (0);

      B.Write_Field_Storage_Strings (FS);

      B.Set_Creation_Timestamp (Creation_Timestamp);
      B.Set_Modification_Timestamp (Creation_Timestamp);
      B.Set_Expiry_Timestamp (Expiry_Timestamp);

      B.Set_Block_Header_Trailing_Reserved_1 (0);
      B.Set_Block_Header_Trailing_Reserved_1 (0);
      B.Update_Header_CRC;

   end Create_Single_Item_Block_Header;


   function Get_Block_Header_Trailing_Reserved_1 (B: Block) return Unsigned_32 is
   begin
      return B.Get_Block_Header_Trailing_Ptr.Reserved_1;
   end Get_Block_Header_Trailing_Reserved_1;


   function Get_Block_Header_Trailing_Reserved_2 (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Trailing_Ptr.Reserved_2;
   end Get_Block_Header_Trailing_Reserved_2;


   function Get_Block_Header_CRC (B: Block) return Unsigned_16 is
   begin
      return B.Get_Block_Header_Trailing_Ptr.CRC;
   end Get_Block_Header_CRC;


   procedure Set_Block_Header_Trailing_Reserved_1 (B: Block; Value: Unsigned_32) is
   begin
      B.Get_Block_Header_Trailing_Ptr.Reserved_1 := Value;
   end Set_Block_Header_Trailing_Reserved_1;


   procedure Set_Block_Header_Trailing_Reserved_2 (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Trailing_Ptr.Reserved_2 := Value;
   end Set_Block_Header_Trailing_Reserved_2;


   procedure Set_Block_Header_CRC (B: Block; Value: Unsigned_16) is
   begin
      B.Get_Block_Header_Trailing_Ptr.CRC := Value;
   end Set_Block_Header_CRC;


   -- footer

   function Get_Block_Footer_Reserved (B: Block) return Unsigned_32 is
   begin
      return B.Get_Block_Footer_Ptr.Reserved;
   end Get_Block_Footer_Reserved;


   function Get_Block_Footer_CRC (B: Block) return Unsigned_32 is
   begin
      return B.Get_Block_Footer_Ptr.CRC;
   end Get_Block_Footer_CRC;


   procedure Set_Block_Footer_Reserved (B: Block; Value: Unsigned_32) is
   begin
      B.Get_Block_Footer_Ptr.Reserved := Value;
   end Set_Block_Footer_Reserved;


   procedure Set_Block_Footer_CRC (B: Block; Value: Unsigned_32) is
   begin
      B.Get_Block_Footer_Ptr.CRC := Value;
   end Set_Block_Footer_CRC;


   -- ==========================================================================
   -- Test Support
   -- ==========================================================================


   function Test_Support_Serializer (B: Block) return Serializable.Instance is
   begin
      return Serializable.Create_Without_Copy (Use_In_Place => B.MPtr,
                                               First        => B.MPtr'First,
                                               Last         => B.MPtr'Last);
   end Test_Support_Serializer;


end Container_Package.Block_Package;


package body BRBON.Block.Header is


   function Get_Byte_Storage_Order (B: BRBON.Block) return BRBON.Byte_Storage_Order is
   begin
      if B.Swap then
         if BRBON.Machine_Byte_Order = BRBON.MSB_First then
            return BRBON.LSB_First;
         else
            return BRBON.MSB_First;
         end if;
      else
         return BRBON.Machine_Order;
      end if;
   end Get_Byte_Storage_Order;

   
-- -----------------------------------------------------------------------------

   function Get_Type (B: BRBON.Block) return BRBON.Block.Block_Type is
   begin
      return To_Header_Leading_Ptr (B.Data (0)'Access).Is_Type;
   end Get_Type;

   
-- -----------------------------------------------------------------------------

   procedure Set_Options (B: BRBON.Block; Value: BRBON.Block.Block_Options) is
   begin
      To_Header_Leading_Ptr (B.Data (0)'Access).Options := Value;
   end Set_Options;

   
-- -----------------------------------------------------------------------------

   function Get_Options (B: BRBON.Block) return BRBON.Block.Block_Options is
   begin
      return To_Header_Leading_Ptr (B.Data (0)'Access).Options;
   end Get_Options;

   
-- -----------------------------------------------------------------------------

   function Get_Byte_Count (B: BRBON.Block) return Unsigned_32 is
   begin
      return To_Header_Leading_Ptr (B.Data (0)'Access).Block_Byte_Count;
   end Get_Byte_Count;

   
-- -----------------------------------------------------------------------------

   function Get_Header_Byte_Count (B: BRBON.Block) return Unsigned_16 is
   begin
      return To_Header_Leading_Ptr (B.Data (0)'Access).Header_Byte_Count;
   end Get_Header_Byte_Count;

   
-- -----------------------------------------------------------------------------

   function Get_Encrypted_Header_Byte_Count (B: BRBON.Block) return Unsigned_16 is
   begin
      return To_Header_Leading_Ptr (B.Data (0)'Access).Encrypted_Header_Byte_Count;
   end Get_Encrypted_Header_Byte_Count;
   
   
-- -----------------------------------------------------------------------------

   procedure Set_Origin (B: BRBON.Block; Value: String) is
      FSS: Field_Storage_Strings := Read_Field_Storage_Strings (B);
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
   
   
-- -----------------------------------------------------------------------------

   function Get_Origin (B: BRBON.Block) return String;
   
   
-- -----------------------------------------------------------------------------

   function Get_Origin_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   function Get_Origin_Byte_Count (B: BRBON.Block) return Unsigned_8;

   
-- -----------------------------------------------------------------------------

   procedure Set_Identifier (B: BRBON.Block; Value: String);
   
   
-- -----------------------------------------------------------------------------

   function Get_Identifier (B: BRBON.Block) return String;
   
   
-- -----------------------------------------------------------------------------

   function Get_Identifier_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   function Get_Identifier_Byte_Count (B: BRBON.Block) return Unsigned_8;

   
-- -----------------------------------------------------------------------------

   procedure Set_Extension (B: BRBON.Block; Value: String);
   
   
-- -----------------------------------------------------------------------------

   function Get_Extension (B: BRBON.Block) return String;
   
   
-- -----------------------------------------------------------------------------

   function Get_Extension_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   function Get_Extension_Byte_Count (B: BRBON.Block) return Unsigned_8;

   
-- -----------------------------------------------------------------------------

   procedure Set_Path_Prefix (B: BRBON.Block; Value: String);
   
   
-- -----------------------------------------------------------------------------

   function Get_Path_Prefix (B: BRBON.Block) return String;
   
   
-- -----------------------------------------------------------------------------

   function Get_Path_Prefix_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   function Get_Path_Prefix_Byte_Count (B: BRBON.Block) return Unsigned_8;

   
-- -----------------------------------------------------------------------------

   procedure Set_Acquisition_URL (B: BRBON.Block; Value: String);
   
   
-- -----------------------------------------------------------------------------

   function Get_Acquisition_URL (B: BRBON.Block) return String;
   
   
-- -----------------------------------------------------------------------------

   function Get_Acquisition_URL_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   function Get_Acquisition_URL_Byte_Count (B: BRBON.Block) return Unsigned_8;

   
-- -----------------------------------------------------------------------------

   procedure Set_Public_Key_URL (B: BRBON.Block; Value: String);
   
   
-- -----------------------------------------------------------------------------

   function Get_Public_Key_URL (B: BRBON.Block) return String;
   
   
-- -----------------------------------------------------------------------------

   function Get_Public_Key_URL_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   function Get_Public_Key_URL_Byte_Count (B: BRBON.Block) return Unsigned_8;

   
-- -----------------------------------------------------------------------------

   procedure Set_Creation_Timestamp (B: BRBON.Block; Value: Unsigned_64);

   
-- -----------------------------------------------------------------------------

   function Get_Creation_Timestamp (B: BRBON.Block) return Unsigned_64;

   
-- -----------------------------------------------------------------------------

   procedure Set_Modification_Timestamp (B: BRBON.Block; Value: Unsigned_64);

   
-- -----------------------------------------------------------------------------

   function Get_Modification_Timestamp (B: BRBON.Block) return Unsigned_64;

   
-- -----------------------------------------------------------------------------

   procedure Set_Expiry_Timestamp (B: BRBON.Block; Value: Unsigned_64);

   
-- -----------------------------------------------------------------------------

   function Get_Expiry_Timestamp (B: BRBON.Block) return Unsigned_64;

   
-- -----------------------------------------------------------------------------

   function Get_Header_CRC (B: BRBON.Block) return Unsigned_16;

   
-- -----------------------------------------------------------------------------

   procedure Update_Header_CRC (B: BRBON.Block);
   
   
-- -----------------------------------------------------------------------------
   
   function Byte_Count (FFS: Field_Storage_Strings) return Unsigned_16 is
      BC: Unsigned_16 := 0;
   begin
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Origin));
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Identifier));
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Extension));
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Path_Prefix));
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Acquisition_URL));
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Public_Key_URL));
      BC := BC + Unsigned_16 (Ada.Strings.Unbounded.Length (FSS.Target_List));
      return BC;
   end Byte_Count;

end BRBON.Header_Package;

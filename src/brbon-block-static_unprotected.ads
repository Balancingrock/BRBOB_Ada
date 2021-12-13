with Interfaces; use Interfaces;

with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Item;
with BRBON.Utils;
with BRBON.Configure;
with BRBON.Container; use BRBON.Container;
with BRBON.Block;
with BRBON.Portal;

with Serializable;
with UUID_Package;
with Color_Package;


package BRBON.Block.Static_Unprotected is


   -- The store that contains a static BRBON hierarchy on which unprotected access is possible.
   -- This is the fasted possible acces to items in a BRBON store.
   --
   type Instance is new Block.Instance with null record;


   -- Creates a new instance with a header of the requested type.
   -- @param Block_Type The type of the block to be created.
   --   Exception Illegal_Buffer_Type is raised if this is not a Single_Item_File (The only type supported at this time)
   -- @param Minimum_Byte_Count The minimum number of bytes to allocate.
   --   The actual number of bytes allocated will be higher than this number, modified for alignement and overhead.
   --   Depending on the block type, the actual count can be about 100~150 bytes higher than requested.
   --   Use the operation Byte_Count to determine the actual byte count of the buffer.
   -- @param Header_Field_Storage_Byte_Count Sets the minimum size for this area in the header. Note that if the strings
   --   in the other parameters need more space, more space will be assigned. Leave the default value in to automatically
   --   assign the needed space. This space will always be a multiple of 8 bytes. The necessary length can be calculated
   --   by summing the length of each string (in the parameters) and rounding up to a multiple of 8 bytes.
   -- @param Using_Endianness The endianness to be used for multi-byte items.
   -- @returns A Static_Unprotected instance.
   --
   function Factory
      (
       Type_Of_Block: Block_Type;
       Minimum_Byte_Count: Unsigned_32;
       Header_Field_Storage_Byte_Count: Unsigned_16 := 1;
       Options: Block_Options := No_Block_Options;
       Using_Endianness: Endianness := Configure.Machine_Endianness;
       Origin: String := "";
       Identifier: String := "";
       Extension: String := "";
       Path_Prefix: String := "";
       Acquisition_URL: String := "";
       Target_List: String := "";
       Public_Key_URL: String := "";
       Creation_Timestamp: Unsigned_64 := BRBON.Utils.Milli_Sec_Since_Jan_1_1970;
       Expiry_Timestamp: Unsigned_64 := 16#7FFF_FFFF_FFFF_FFFF#
      ) return Instance;


   function Free_Area_Byte_Count (I: in out Instance) return Unsigned_32;

   procedure Add_Root_Item (I: in out Instance; Of_Type: Types.Item_Type; With_Byte_Count: Unsigned_32; With_Name: String);


   -- Item Access
   --
   function Get_Bool (I: Portal.Instance) return Boolean;
   pragma Inline (Get_Bool);
   --
   procedure Set_Bool (P: Portal.Instance; Value: Boolean);
   pragma Inline (Set_Bool);
   --
   function Get_Int_8 (P: Portal.Instance) return Integer_8;
   pragma Inline (Get_Int_8);
   --
   procedure Set_Int_8 (P: Portal.Instance; Value: Integer_8);
   pragma Inline (Set_Int_8);
   --
   function Get_Int_16 (P: Portal.Instance) return Integer_16;
   pragma Inline (Get_Int_16);
   --
   procedure Set_Int_16 (P: Portal.Instance; Value: Integer_16);
   pragma Inline (Set_Int_16);
   --
   function Get_Int_32 (P: Portal.Instance) return Integer_32;
   pragma Inline (Get_Int_32);
   --
   procedure Set_Int_32 (P: Portal.Instance; Value: Integer_32);
   pragma Inline (Set_Int_32);
   --
   function Get_Int_64 (P: Portal.Instance) return Integer_64;
   pragma Inline (Get_Int_64);
   --
   procedure Set_Int_64 (P: Portal.Instance; Value: Integer_64);
   pragma Inline (Set_Int_64);
   --
   function Get_UInt_8 (P: Portal.Instance) return Unsigned_8;
   pragma Inline (Get_UInt_8);
   --
   procedure Set_UInt_8 (P: Portal.Instance; Value: Unsigned_8);
   pragma Inline (Set_UInt_8);
   --
   function Get_UInt_16 (P: Portal.Instance) return Unsigned_16;
   pragma Inline (Get_UInt_16);
   --
   procedure Set_UInt_16 (P: Portal.Instance; Value: Unsigned_16);
   pragma Inline (Set_UInt_16);
   --
   function Get_UInt_32 (P: Portal.Instance) return Unsigned_32;
   pragma Inline (Get_UInt_32);
   --
   procedure Set_UInt_32 (P: Portal.Instance; Value: Unsigned_32);
   pragma Inline (Set_UInt_32);
   --
   function Get_UInt_64 (P: Portal.Instance) return Unsigned_64;
   pragma Inline (Get_UInt_64);
   --
   procedure Set_UInt_64 (P: Portal.Instance; Value: Unsigned_64);
   pragma Inline (Set_UInt_64);
   --
   function Get_String (P: Portal.Instance) return String;
   pragma Inline (Get_String);
   --
   procedure Set_String (P: Portal.Instance; Value: String);
   pragma Inline (Set_String);
   --
   function Get_CRC_String (P: Portal.Instance) return String;
   pragma Inline (Get_CRC_String);
   --
   procedure Set_CRC_String (P: Portal.Instance; Value: String);
   pragma Inline (Set_CRC_String);
   --
   function Get_Binary (P: Portal.Instance) return Array_Of_Unsigned_8;
   pragma Inline (Get_Binary);
   --
   procedure Set_Binary (P: Portal.Instance; Value: Array_Of_Unsigned_8);
   pragma Inline (Set_Binary);
   --
   function Get_CRC_Binary (P: Portal.Instance) return Array_Of_Unsigned_8;
   pragma Inline (Get_CRC_Binary);
   --
   procedure Set_CRC_Binary (P: Portal.Instance; Value: Array_Of_Unsigned_8);
   pragma Inline (Set_CRC_Binary);
   --
   function Get_UUID (P:Portal.Instance) return UUID_Package.UUID;
   pragma Inline (Get_UUID);
   --
   procedure Set_UUID (P: Portal.Instance; Value: UUID_Package.UUID);
   pragma Inline (Set_UUID);
   --
   function Get_Color (P:Portal.Instance) return Color_Package.Color;
   pragma Inline (Get_Color);
   --
   procedure Set_Color (P: Portal.Instance; Value: Color_Package.Color);
   pragma Inline (Set_Color);


end BRBON.Block.Static_Unprotected;

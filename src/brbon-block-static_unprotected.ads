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


   -- Return the byte count of the unused area in this block.
   --
   function Free_Area_Byte_Count (I: in out Instance) return Unsigned_32;


   -- Add the requested root item if the root item is not a container.
   --
   function Add_Root_Item (I: in out Instance; Of_Type: Types.Item_Type; With_Byte_Count: Unsigned_32; With_Name: String) return Portal.Instance;


   -- Add an Array_Type container item at root level.
   --
   function Add_Root_Item_Array_Type (I: in out Instance; With_Name: String; Element_Type: Types.Item_Type; Element_Byte_Count: Unsigned_32; Max_Element_Count: Unsigned_32) return Portal.Instance;


   -- Return a portal referencing the root item.
   --
   function Get_Root_Item (I: in out Instance) return Portal.Instance;




   -- Item Structure Access
   --
   function Get_Type (P: Portal.Instance) return Types.Item_Type;
   pragma Inline (Get_Type);
   --
   function Get_Options (P: Portal.Instance) return Types.Item_Options;
   pragma Inline (Get_Options);
   --
   function Get_Flags (P: Portal.Instance) return Types.Item_Flags;
   pragma Inline (Get_Flags);
   --
   function Get_Name (P: Portal.Instance) return String;
   pragma Inline (Get_Name);
   --
   function Get_Byte_Count (P: Portal.Instance) return Unsigned_32;
   pragma Inline (Get_Byte_Count);
   --
   function Get_Parent_Offset (P: Portal.Instance) return Unsigned_32;
   pragma Inline (Get_Parent_Offset);
   --
   function Get_Value_Area_Byte_Count (P: Portal.Instance) return Unsigned_32;
   pragma Inline (Get_Value_Area_Byte_Count);


   -- Value Access

   -- Return the bool value if the portal refers to a Bool_Type.
   -- Note: It is assumed that the portal refers to the start of an item of the expected type.
   -- If not, no error will be signalled, but the returned result will be unreliable.
   --
   function Get_Bool (P: Portal.Instance) return Boolean;
   pragma Inline (Get_Bool);

   -- Sets the bool value if the portal refers to a Bool_Type.
   -- Note: It is assumed that the portal refers to the start of an item of the expected type.
   -- If not, no error will be signalled, but the block/item structure or content may be corrupted in unpredictable ways.
   --
   procedure Set_Bool (P: Portal.Instance; Value: Boolean);
   pragma Inline (Set_Bool);


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
   function Get_Float_32 (P: Portal.Instance) return IEEE_Float_32;
   pragma Inline (Get_Float_32);
   --
   procedure Set_Float_32 (P: Portal.Instance; Value: IEEE_Float_32);
   pragma Inline (Set_Float_32);
   --
   function Get_Float_64 (P: Portal.Instance) return IEEE_Float_64;
   pragma Inline (Get_Float_64);
   --
   procedure Set_Float_64 (P: Portal.Instance; Value: IEEE_Float_64);
   pragma Inline (Set_Float_64);
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

   -- Returns the CRC value for the stored string.
   -- Returns zero for empty strings.
   --
   function Get_CRC_String_CRC (P: Portal.Instance) return Unsigned_32;
   pragma Inline (Get_CRC_String);

   -- Returns the 32 bit CRC and the byte count in a single value.
   -- Note that this value is endianess dependent.
   --
   function Get_CRC_String_Quick_Compare (P: Portal.Instance) return Unsigned_64;
   pragma Inline (Get_CRC_String_Quick_Compare);

   function Get_Binary (P: Portal.Instance) return Array_Of_Unsigned_8;
   pragma Inline (Get_Binary);

   procedure Set_Binary (P: Portal.Instance; Value: Array_Of_Unsigned_8);
   pragma Inline (Set_Binary);

   function Get_CRC_Binary (P: Portal.Instance) return Array_Of_Unsigned_8;
   pragma Inline (Get_CRC_Binary);

   procedure Set_CRC_Binary (P: Portal.Instance; Value: Array_Of_Unsigned_8);
   pragma Inline (Set_CRC_Binary);

   -- Returns the CRC value for the stored binary.
   -- Returns zero for empty binaries.
   --
   function Get_CRC_Binary_CRC (P: Portal.Instance) return Unsigned_32;
   pragma Inline (Get_CRC_Binary_CRC);

   -- Returns the 32 bit CRC and the byte count in a single value.
   -- Note that this value is endianess dependent.
   --
   function Get_CRC_Binary_Quick_Compare (P: Portal.Instance) return Unsigned_64;
   pragma Inline (Get_CRC_String_Quick_Compare);


   function Get_UUID (P:Portal.Instance) return UUID_Package.UUID;
   pragma Inline (Get_UUID);

   procedure Set_UUID (P: Portal.Instance; Value: UUID_Package.UUID);
   pragma Inline (Set_UUID);


   function Get_Color (P:Portal.Instance) return Color_Package.Color;
   pragma Inline (Get_Color);

   procedure Set_Color (P: Portal.Instance; Value: Color_Package.Color);
   pragma Inline (Set_Color);


   -- Return the last used index in the table.
   --
   function Get_Last_Index (P: Portal.Instance) return Unsigned_32;


   -- Return the byte count of each element in the table.
   --
   function Get_Element_Byte_Count (P: Portal.Instance) return Unsigned_32;


   -- Retrieving table elements
   --
   function Get_Element (P: Portal.Instance; Index: Unsigned_32) return Portal.Instance;


   -- Add an element to the table
   --
   function Add_Element (P: Portal.Instance) return Portal.Instance;



end BRBON.Block.Static_Unprotected;

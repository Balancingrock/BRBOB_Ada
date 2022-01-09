with Interfaces; use Interfaces;

with Ada.Finalization;


with BRBON.Utils;
with BRBON.Container; use BRBON.Container;
with BRBON.Block;


with Serializable;
with UUID_Package;
with Color_Package;


package BRBON.Block.Static_Unprotected is


   -- The store that contains a static BRBON hierarchy on which unprotected access is possible.
   -- This is the fasted possible acces to items in a BRBON store.
   --
   type Static_Unprotected_Record is new Block_Record with null record;


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
       Using_Endianness: Byte_Storage_Order := Machine_Byte_Storage_Order;
       Origin: String := "";
       Identifier: String := "";
       Extension: String := "";
       Path_Prefix: String := "";
       Acquisition_URL: String := "";
       Target_List: String := "";
       Public_Key_URL: String := "";
       Creation_Timestamp: Unsigned_64 := BRBON.Utils.Milli_Sec_Since_Jan_1_1970;
       Expiry_Timestamp: Unsigned_64 := Unsigned_64'Last
      ) return Static_Unprotected_Record;


   -- Return the byte count of the unused area in this block.
   --
   function Free_Area_Byte_Count (S: Static_Unprotected_Record) return Unsigned_32;


   -- Add a new root item and returns a portal for it.
   -- Use one of the specific container functions to create a fully functional container item.
   --
   function Add_Root_Item (S: Static_Unprotected_Record; Of_Type: BRBON.Item_Type; With_Byte_Count: Unsigned_32; With_Name: BRBON.Item_Name) return BRBON.Portal_Record;


   -- Add an Array_Type container item at root level and return the portal for it.
   --
   function Add_Root_Item_Array_Type (S: Static_Unprotected_Record; With_Name: BRBON.Item_Name; Element_Type: BRBON.Item_Type; Element_Byte_Count: Unsigned_32; Max_Element_Count: Unsigned_32) return BRBON.Portal_Record;


   -- Return a portal referencing the root item.
   --
   function Get_Root_Item (S: Static_Unprotected_Record) return Portal_Record;


   -- Item Structure Access
   --
   function Get_Type (P: Portal_Record) return Item_Type;
   pragma Inline (Get_Type);
   --
   function Get_Options (P: Portal_Record) return Item_Options;
   pragma Inline (Get_Options);
   --
   function Get_Flags (P: Portal_Record) return Item_Flags;
   pragma Inline (Get_Flags);
   --
   function Get_Name (P: Portal_Record) return String;
   pragma Inline (Get_Name);
   --
   function Get_Byte_Count (P: Portal_Record) return Unsigned_32;
   pragma Inline (Get_Byte_Count);
   --
   function Get_Parent_Offset (P: Portal_Record) return Unsigned_32;
   pragma Inline (Get_Parent_Offset);
   --
   function Get_Value_Area_Byte_Count (P: Portal_Record) return Unsigned_32;
   pragma Inline (Get_Value_Area_Byte_Count);


   -- Value Access

   -- Return the bool value if the portal refers to a Bool_Type.
   -- Note: It is assumed that the portal refers to the start of an item of the expected type.
   -- If not, no error will be signalled, but the returned result will be unreliable.
   --
   function Get_Bool (P: Portal_Record) return Boolean;
   pragma Inline (Get_Bool);

   -- Sets the bool value if the portal refers to a Bool_Type.
   -- Note: It is assumed that the portal refers to the start of an item of the expected type.
   -- If not, no error will be signalled, but the block/item structure or content may be corrupted in unpredictable ways.
   --
   procedure Set_Bool (P: Portal_Record; Value: Boolean);
   pragma Inline (Set_Bool);


   function Get_Int_8 (P: Portal_Record) return Integer_8;
   pragma Inline (Get_Int_8);
   --
   procedure Set_Int_8 (P: Portal_Record; Value: Integer_8);
   pragma Inline (Set_Int_8);
   --
   function Get_Int_16 (P: Portal_Record) return Integer_16;
   pragma Inline (Get_Int_16);
   --
   procedure Set_Int_16 (P: Portal_Record; Value: Integer_16);
   pragma Inline (Set_Int_16);
   --
   function Get_Int_32 (P: Portal_Record) return Integer_32;
   pragma Inline (Get_Int_32);
   --
   procedure Set_Int_32 (P: Portal_Record; Value: Integer_32);
   pragma Inline (Set_Int_32);
   --
   function Get_Int_64 (P: Portal_Record) return Integer_64;
   pragma Inline (Get_Int_64);
   --
   procedure Set_Int_64 (P: Portal_Record; Value: Integer_64);
   pragma Inline (Set_Int_64);
   --
   function Get_UInt_8 (P: Portal_Record) return Unsigned_8;
   pragma Inline (Get_UInt_8);
   --
   procedure Set_UInt_8 (P: Portal_Record; Value: Unsigned_8);
   pragma Inline (Set_UInt_8);
   --
   function Get_UInt_16 (P: Portal_Record) return Unsigned_16;
   pragma Inline (Get_UInt_16);
   --
   procedure Set_UInt_16 (P: Portal_Record; Value: Unsigned_16);
   pragma Inline (Set_UInt_16);
   --
   function Get_UInt_32 (P: Portal_Record) return Unsigned_32;
   pragma Inline (Get_UInt_32);
   --
   procedure Set_UInt_32 (P: Portal_Record; Value: Unsigned_32);
   pragma Inline (Set_UInt_32);
   --
   function Get_UInt_64 (P: Portal_Record) return Unsigned_64;
   pragma Inline (Get_UInt_64);
   --
   procedure Set_UInt_64 (P: Portal_Record; Value: Unsigned_64);
   pragma Inline (Set_UInt_64);
   --
   function Get_Float_32 (P: Portal_Record) return IEEE_Float_32;
   pragma Inline (Get_Float_32);
   --
   procedure Set_Float_32 (P: Portal_Record; Value: IEEE_Float_32);
   pragma Inline (Set_Float_32);
   --
   function Get_Float_64 (P: Portal_Record) return IEEE_Float_64;
   pragma Inline (Get_Float_64);
   --
   procedure Set_Float_64 (P: Portal_Record; Value: IEEE_Float_64);
   pragma Inline (Set_Float_64);
   --
   function Get_String (P: Portal_Record) return String;
   pragma Inline (Get_String);
   --
   procedure Set_String (P: Portal_Record; Value: String);
   pragma Inline (Set_String);
   --
   function Get_CRC_String (P: Portal_Record) return String;
   pragma Inline (Get_CRC_String);
   --
   procedure Set_CRC_String (P: Portal_Record; Value: String);
   pragma Inline (Set_CRC_String);

   -- Returns the CRC value for the stored string.
   -- Returns zero for empty strings.
   --
   function Get_CRC_String_CRC (P: Portal_Record) return Unsigned_32;
   pragma Inline (Get_CRC_String);

   -- Returns the 32 bit CRC and the byte count in a single value.
   -- Note that this value is endianess dependent.
   --
   function Get_CRC_String_Quick_Compare (P: Portal_Record) return Unsigned_64;
   pragma Inline (Get_CRC_String_Quick_Compare);

   function Get_Binary (P: Portal_Record) return Unsigned_8_Array;
   pragma Inline (Get_Binary);

   procedure Set_Binary (P: Portal_Record; Value: Unsigned_8_Array);
   pragma Inline (Set_Binary);

   function Get_CRC_Binary (P: Portal_Record) return Unsigned_8_Array;
   pragma Inline (Get_CRC_Binary);

   procedure Set_CRC_Binary (P: Portal_Record; Value: Unsigned_8_Array);
   pragma Inline (Set_CRC_Binary);

   -- Returns the CRC value for the stored binary.
   -- Returns zero for empty binaries.
   --
   function Get_CRC_Binary_CRC (P: Portal_Record) return Unsigned_32;
   pragma Inline (Get_CRC_Binary_CRC);

   -- Returns the 32 bit CRC and the byte count in a single value.
   -- Note that this value is endianess dependent.
   --
   function Get_CRC_Binary_Quick_Compare (P: Portal_Record) return Unsigned_64;
   pragma Inline (Get_CRC_String_Quick_Compare);


   function Get_UUID (P:Portal_Record) return UUID_Package.UUID;
   pragma Inline (Get_UUID);

   procedure Set_UUID (P: Portal_Record; Value: UUID_Package.UUID);
   pragma Inline (Set_UUID);


   function Get_Color (P:Portal_Record) return Color_Package.Color;
   pragma Inline (Get_Color);

   procedure Set_Color (P: Portal_Record; Value: Color_Package.Color);
   pragma Inline (Set_Color);


   -- ==========================================================================
   -- Array_Type operations
   -- ==========================================================================

   -- Return the number of elements in the array.
   --
   function Get_Element_Count (P: Portal_Record) return Unsigned_32;


   -- Return the byte count of each element in the array.
   --
   function Get_Element_Byte_Count (P: Portal_Record) return Unsigned_32;


   -- Retrieve an array elements
   --
   function Get_Element (P: Portal_Record; From_Index: Unsigned_32) return Portal_Record;


   -- Add an element to the end of the array
   --
   function Add_Element (P: Portal_Record) return Portal_Record;


   -- Remove an element from the array
   --
   procedure Remove_Last_Element (P: Portal_Record);


end BRBON.Block.Static_Unprotected;

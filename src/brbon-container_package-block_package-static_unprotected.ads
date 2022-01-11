with Interfaces; use Interfaces;

with Ada.Finalization;


with BRBON.Utils;
with BRBON.Container_Package.Block_Package; use BRBON.Container_Package.Block_Package;


with Serializable;
with UUID_Package;
with Color_Package;


with BRBON.Portal_Package.Static_Unprotected; use BRBON.Portal_Package.Static_Unprotected;


package BRBON.Container_Package.Block_Package.Static_Unprotected is


   -- The store that contains a static BRBON hierarchy on which unprotected access is possible.
   -- This is the fasted possible acces to items in a BRBON store.
   --
   type Static_Unprotected_Block is new Block with null record;


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
      ) return Static_Unprotected_Block;


   -- Return the byte count of the unused area in this block.
   --
   function Free_Area_Byte_Count (S: Static_Unprotected_Block) return Unsigned_32;


   -- Add a new root item and returns a portal for it.
   -- Use one of the specific container functions to create a fully functional container item.
   --
   function Add_Root_Item (S: Static_Unprotected_Block; Of_Type: BRBON.Item_Type; With_Byte_Count: Unsigned_32; With_Name: BRBON.Item_Name) return Static_Unprotected_Portal;


   -- Add an Array_Type container item at root level and return the portal for it.
   --
   function Add_Root_Item_Array_Type (S: Static_Unprotected_Block; With_Name: BRBON.Item_Name; Element_Type: BRBON.Item_Type; Element_Byte_Count: Unsigned_32; Max_Element_Count: Unsigned_32) return Static_Unprotected_Portal;


   -- Return a portal referencing the root item.
   --
   function Get_Root_Item (S: Static_Unprotected_Block) return Static_Unprotected_Portal;


end BRBON.Container_Package.Block_Package.Static_Unprotected;

with Interfaces; use Interfaces;

with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Item;
with BRBON.Utils;
with BRBON.Configure;
with BRBON.Container; use BRBON.Container;
with BRBON.Block;
with Serializable;


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

   function Get_Bool (P: Portal) return Boolean;
   procedure Set_Bool (P: Portal; Value: Boolean);
   function Get_Int_8 (P: Portal) return Integer_8);
   procedure Set_Int_8 (P: Portal; Value: Integer_8);


end BRBON.Block.Static_Unprotected;

with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;
with BRBON.Header;
with BRBON.Footer;

with Serializable;


package BRBON.Block is


   type Instance is abstract new Ada.Finalization.Controlled with private;
      
   
   -- The type of block
   --
   function Type_Of_Block (I: in out Instance) return Block_Type;
   
   
   -- The total number of bytes that will be used by the block if it is saved or transferred.
   --
   function Byte_Count (I: in out Instance) return Unsigned_32 is abstract;

   
   -- The byte count of the area that can be used for item storage.
   --
   function Free_Area_Byte_Count (I: in out Instance) return Unsigned_32 is abstract;
   
   
   -- Writes the block to file.
   -- First the header and footer will be updated to create a valid block.
   --
   procedure Write_To_File (I: in out Instance; To_Path: String);
      
   
   -- =================================================================
   -- | The following operations are for test-use only. Do not use.   |
   -- | (they are not stable and may have unanticipated side effects) |
   -- =================================================================
   
   -- Undocumented, for testing only, do not use!
   --
   function Test_Serializer (I: in out Instance) return Serializable.Instance;
   
   
private
   

   type Instance is abstract new Ada.Finalization.Controlled with record
      Container: BRBON.Container.Instance;
      Memory_Ptr: Array_Of_Unsigned_8_Ptr; -- The Container does not export its pointer, a copy must be kept.
      Start_Of_Header_Field_Storage: Unsigned_16;
      First_Free_Byte_In_Header_Field_Storage: Unsigned_16; -- range self.Start_Of_Header_Field_Storage .. self.Last_Free_Byte_In_header_Field_Storage + 1
      Last_Free_Byte_In_header_Field_Storage: Unsigned_16; -- quasi constant
      Start_Of_Payload: Unsigned_32; -- quasi constant
      First_Free_Byte_In_Payload: Unsigned_32; -- range self.Start_Of_Payload .. self.Last_Free_Byte_In_Payload + 1
      Last_Free_Byte_In_Payload: Unsigned_32; -- will never decrease, may increase for some child classes
   end record;

   
   -- Ensures that all fields in the block and header structure are consistent with the content and size of the block.
   -- This affects things like byte-count(s), CRC-values, footer content and maybe more.
   --
   procedure Ensure_Block_Consistency (I: in out Instance);
   
   
   -- Adds the given string to the header field storage area and returns the offset of the start location.
   -- Returns zero if there is insufficient space available.
   --
   function Add_To_Header_Field (I: in out Instance; Value: String) return Unsigned_16;
   
      
end BRBON.Block;

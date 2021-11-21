with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;
with BRBON.Header;

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
      Header: BRBON.Header.Instance;
      First_Free_Byte_In_Content_Area: Unsigned_32;
   end record;

   
   -- Ensures that all fields in the block and header structure are consistent with the content and size of the block.
   -- This affects things like byte-count(s), CRC-values, footer content and maybe more.
   --
   procedure Ensure_Block_Consistency (I: in out Instance);
   
end BRBON.Block;

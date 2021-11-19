with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;

with Serializable;


package BRBON.Block is


   type Instance is new Ada.Finalization.Controlled with private;
   
   
   -- The types of blocks available
   --
   type Instance_Type is
     (
      Illegal,
      Single_Item_File
     );
   for Instance_Type'Size use 16;
   for Instance_Type use
     (
      Illegal => 0,
      Single_Item_File => 1
     );

   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Instance_Type, Unsigned_16);
   function To_Block_Instance_Type is new Ada.Unchecked_Conversion (Unsigned_16, Instance_Type);


   -- The options for a Block
   --
   type Options is (No_Options, Reacquisition_Possible);
   for Options'Size use 16;
   for Options use
      (
         No_Options             => 0,
         Reacquisition_Possible => 16#01#
      );
   
   function To_Unsigned_16 is new Ada.Unchecked_Conversion (Options, Unsigned_16);
   function To_Options is new Ada.Unchecked_Conversion (Unsigned_16, Options);
   

   -- The type of block
   --
   function Type_Of_Block (I: in out Instance'Class) return Instance_Type;
   
   
   -- The total number of bytes that will be used by the block if it is saved or transferred.
   --
   function Byte_Count (I: in out Instance'Class) return Unsigned_32 is abstract;

   
   -- Writes the block to file.
   -- First the header and footer will be updated to create a valid block.
   --
   procedure Write_To_File (I: in out Instance'Class; To_Path: String);
   
   
   -- Read a block from file.
   -- Also verifies that the block header structure is valid. If this check fails, an exception will be raised.
   --
   function Read_From_File (At_Path: String) return Instance;
   
   
   -- =================================================================
   -- | The following operations are for test-use only. Do not use.   |
   -- | (they are not stable and may have unanticipated side effects) |
   -- =================================================================
   
   -- Undocumented, for testing only, do not use!
   --
   function Test_Serializer (I: in out Instance'Class) return Serializable.Instance;
   
   
private
   

   type Instance is new Ada.Finalization.Controlled with record
      Container: BRBON.Container.Instance;
      Memory_Ptr: Array_Of_Unsigned_8_Ptr; -- The Container does not export its pointer, a copy must be kept.
      First_Free_Byte_In_Header_Field: Unsigned_16;
      First_Free_Byte_In_Content_Area: Unsigned_32;
   end record;

   
   -- Ensures that all fields in the block and header structure are consistent with the content and size of the block.
   -- This affects things like byte-count(s), CRC-values, footer content and maybe more.
   --
   procedure Ensure_Block_Consistency (I: in out Instance'Class);
   
end BRBON.Block;

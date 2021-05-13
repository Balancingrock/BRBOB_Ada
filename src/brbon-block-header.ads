with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; -- use BRBON.Container;


package BRBON.Block.Header is

   Block_Header_Type_1_Byte_Count: Unsigned_32 := 70;

   -- A block header
   --
   type Block_Header is tagged private;

   -- A block header pointer
   --
   type Block_Header_Ptr is access all Block_Header;


   -- Create a type 1 block header in the given memory area with the given endianness.
   -- @param Memory_Area_Ptr The memory area where the block header should be constrcuted & maintained.
   -- @param Using_Endianness The endianness to be used in the header.
   -- @return A block header.
   --
   function Block_Header_Type_1_Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header;


private

   type Block_Header is tagged
      record
         Store: BRBON.Container.Store;
         Endianness: BRBON.Types.Endianness; -- Set after verification/assignment of synch byte 4
      end record;

end BRBON.Block.Header;

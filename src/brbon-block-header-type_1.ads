with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container;


package BRBON.Block.Header.Type_1 is

   Block_Header_Type_1_Byte_Count: Unsigned_32 := 70;


   -- Create a type 1 block header in the given memory area with the given endianness.
   -- @param Memory_Area_Ptr The memory area where the block header should be constrcuted & maintained.
   -- @param Using_Endianness The endianness to be used in the header.
   -- @return A block header.
   --
   function Block_Header_Type_1_Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header;

   --
   function Factory (Memory_Area_Ptr: Array_Of_Unsigned_8_Ptr; Using_Endianness: Endianness) return Block_Header renames Block_Header_Type_1_Factory;

end BRBON.Block.Header.Type_1;

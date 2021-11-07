with Interfaces; use Interfaces;

with Ada.Finalization;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;
with BRBON.Block;
with Serializable;


package BRBON.Static_Unprotected is

   -- The store that contains a static BRBON hierarchy on which unprotected access is possible.
   -- This is the fasted possible acces to items in a BRBON store.
   --
   type Instance is new Ada.Finalization.Controlled with private;


   -- Access type for the static unprotected store.
   --
   type Instance_Ptr is access all Instance;

   -- Creates a new instance with a header of the requested type.
   -- @param Block_Type The type of the block to be created.
   --   Exception Illegal_Buffer_Type is raised if this is not a Single_Item_File (The only type supported at this time)
   -- @param Minimum_Byte_Count The minimum number of bytes to allocate.
   --   The actual number of bytes allocated will be higher than this number, modified for alignement and overhead.
   --   Depending on the block type, the actual count can be about 100~150 bytes higher than requested.
   --   Use the operation Byte_Count to determine the actual byte count of the buffer.
   -- @param Using_Endianness The endianness to be used for multi-byte items.
   -- @returns A Static_Unprotected instance.
   --
   function Factory (Block_Type: Block.Instance_Type; Minimum_Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Instance;


   -- The byte count of the buffer, including overhead.
   -- Use the operation "Free_Byte_Count" to determine the remaining usable space.
   --
   function Byte_Count (I: in out Instance'Class) return Unsigned_32;
   pragma Inline (Byte_Count);


   -- Returns a serializer that returns the bytes without copying the block.
   --
   function Create_In_Place_Serializable_Instance (I: in out Instance'Class) return Serializable.Instance;

private

   type Instance is new Ada.Finalization.Controlled with
      record
         Memory_Ptr: Array_Of_Unsigned_8_Ptr; -- The Container does not export its pointer, a copy must be kept.
         Container: BRBON.Container.Instance;
      end record;

end BRBON.Static_Unprotected;

with Interfaces; use Interfaces;
with BRBON.Types; use BRBON.Types;


-- Contains methods to convert types into a stream of bytes.
--
package Serializable is


   -- A serializable returns bytes and True as long as a valid byte is supplied.
   -- When no valid byte can be supplied it returns False.
   -- Once False is returned it will not be called again.
   --
   type Instance is tagged private;


   -- Creates a new Serializable.Instanceby copying all the bytes from the given string.
   -- @parameter Copy_Bytes_From The string from which the bytes will be copied.
   -- @returns The new instance.
   --
   function New_Instance (Copy_Bytes_From: String) return Instance;


   -- Creates a new Serializable.Instanceby copying all the bytes from the given array.
   -- @parameter Copy_Bytes_From The array from which the bytes will be copied.
   -- @returns The new instance.
   --
   function New_Instance (Copy_Bytes_From: Array_Of_Unsigned_8) return Instance;


   -- Creates a new Serializable.Instance by referring to the bytes at the given location and length.
   --
   -- Note: The callee must guarantee that the bytes are available during the existence of the instance.
   --
   -- @parameter Use_In_Place A pointer to the array from which to return a series of bytes.
   -- @parameter First The index of the first byte to be returned. First must be <= Last.
   -- @parameter Last The index of the last byte to be returned. Last must be >= First.
   -- @returns The new instance
   --
   function New_Instance (Use_In_Place: Array_Of_Unsigned_8_Ptr; First: Unsigned_32; Last: Unsigned_32) return Instance;


   -- Copies the next byte from this instance into the out parameter.
   -- Returns true if a copy was made, false if not.
   -- Note that after returning false once this instance is no longer usable.
   -- @parameter Source The Serializable.Instance
   -- @parameter Byte The location where the next byte should be copied to.
   -- @retuns True if a byte was copied, false if not (and the instance is exhausted).
   --
   function Copy_Next_Byte (Source: in out Instance; Byte: out Unsigned_8) return Boolean;


   -- Returns true if the instance is empty.
   --
   function Is_Empty (Source: in out Instance) return Boolean;
   pragma Inline (Is_Empty);


   -- Returns the number of bytes left in the instance.
   --
   function Remaining_Bytes (Source: in out Instance) return Integer;
   pragma Inline (Remaining_Bytes);


   -- Compares two serializables.
   -- Both serializables will be updated for the number of examined bytes.
   -- If the operation returns False and the Remaining_Bytes is the same for both Instances
   -- then the Remaining_Bytes may be used to calculate which byte caused the fail using:
   --    (Index_Of_Failed_Byte := Source'Last - Source.Remaining_Bytes)
   -- If the operation returns True then both the serializables have zero Remaining_Bytes.
   --
   function Compare (Source: in out Instance; Expected_Values: in out Instance) return Boolean;


   -- Undocumented, used for test purposes only.
   --
   procedure Hex_Dump_With_Cursor (Source: in out Instance);


   -- Undocumented, used for test purposes only.
   --
   procedure Put_All (Source: in out Instance);

private


   type Instance is tagged
      record
         Base_Ptr: Array_Of_Unsigned_8_Ptr;
         First: Unsigned_32;
         Cursor: Unsigned_32;
         Last: Unsigned_32;
         Must_Deallocate: Boolean;
      end record;


end Serializable;

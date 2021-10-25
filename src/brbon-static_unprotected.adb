
-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ada.Unchecked_Conversion;

with BRBON.Utils;
with BRBON.Block; use BRBON.Block;
with BRBON.Block.Header;
with BRBON.Block.Header.Single_Item_File;


package body BRBON.Static_Unprotected is


   function Factory (Block_Type: Block.Instance_Type; Minimum_Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Instance is

      Byte_Count: Unsigned_32 := 0;
      I: Instance;

   begin

      -- Check if block type is supported
      if Block_Type /= BRBON.Block.Single_Item_File then raise BRBON.Illegal_Block_Type; end if;


      -- Check byte count
      case Block_Type is
         when BRBON.Block.Illegal => raise BRBON.Buffer_Error;
         when BRBON.Block.Single_Item_File =>
            Byte_Count := BRBON.Utils.Round_Up_To_Nearest_Multiple_of_8 (Minimum_Byte_Count + Unsigned_32 (BRBON.Block.Header.Minimum_Byte_Count (BRBON.block.Single_Item_File)) + 4);
      end case;


      -- Allocate memory area
      I.Memory_Ptr := new BRBON.Types.Array_Of_Unsigned_8 (1 .. Byte_Count);
      I.Container := BRBON.Container.Factory (Buffer_Ptr       => I.Memory_Ptr,
                                                        Using_Endianness => Using_Endianness);

      -- Create the block header
      BRBON.Block.Header.Single_Item_File.Create (In_Container => I.Container);

      return I;

   end Factory;


   procedure Finalization (I: in out Instance'Class) is
   begin
      Deallocate_Array_Of_Unsigned_8 (I.Memory_Ptr);
   end Finalization;


   -- Operational Interface

   function Byte_Count (I: in out Instance'Class) return Unsigned_32 is
   begin
      return I.Container.Byte_Count;
   end Byte_Count;

end BRBON.Static_Unprotected;

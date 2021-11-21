-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ada.Unchecked_Conversion;

with BRBON.Utils;
with BRBON.Block; use BRBON.Block;
with BRBON.Footer;
with BRBON.Header;
with BRBON.Header.Single_Item_File;


package body BRBON.Block.Static_Unprotected is

   function Factory
      (
         Type_Of_Block: Block_Type;
         Minimum_Byte_Count: Unsigned_32;
         Options: Block_Options := No_Block_Options;
         Using_Endianness: Endianness := BRBON.Configure.Machine_Endianness;
         Origin: String := "";
         Identifier: String := "";
         Extension: String := "";
         Path_Prefix: String := "";
         Acquisition_URL: String := "";
         Target_List: String := "";
         Public_Key_URL: String := "";
         Creation_Timestamp: Unsigned_64 := BRBON.Utils.Milli_Sec_Since_Jan_1_1970;
         Expiry_Timestamp: Unsigned_64 := 16#7FFF_FFFF_FFFF_FFFF#
      ) return Instance is

      Header_Storage_Field_Byte_Count: Unsigned_16;
      Header_Type_Dependent_Byte_count: Unsigned_16;
      Header_Byte_Count: Unsigned_16;

      Content_Byte_Count: Unsigned_32;
      Block_Byte_Count: Unsigned_32;

      Block: Instance;

   begin

      -- Check if block type is supported
      --
      if Type_Of_Block /= Single_Item_File then raise BRBON.Illegal_Block_Type; end if;


      -- Calculate the size of the storage field in de header
      --
      Header_Storage_Field_Byte_Count := Unsigned_16 (Origin'Length + Identifier'Length + Extension'Length + Path_Prefix'Length + Acquisition_URL'Length + Target_List'Length + Public_Key_URL'Length);

      -- Get the type dependent size
      --
      case Type_Of_Block is
         when Illegal => raise BRBON.Buffer_Error;
         when Single_Item_File => Header_Type_Dependent_Byte_Count := 0;
      end case;

      -- Calculate the header size
      --
      Header_Byte_Count := BRBON.Header.Fixed_Part_Byte_Count + Header_Type_Dependent_Byte_Count + Header_Storage_Field_Byte_Count + BRBON.Header.Past_Storage_Field_Byte_Count;

      -- Calculate the size of the block content field
      --
      Content_Byte_Count := BRBON.Utils.Round_Up_To_Nearest_Multiple_of_8 (Minimum_Byte_Count);

      -- Calculate the size of the block
      --
      Block_Byte_Count := Unsigned_32 (Unsigned_32 (Header_Byte_Count) + Content_Byte_Count + BRBON.Footer.Footer_Byte_Count (Single_Item_File));

      -- Allocate memory area for the container that will enclose the block
      --
      Block.Memory_Ptr := new BRBON.Types.Array_Of_Unsigned_8 (0 .. Block_Byte_Count - 1);

      -- Create the container for the block
      --
      Block.Container := BRBON.Container.Factory (Buffer_Ptr => Block.Memory_Ptr, Using_Endianness => Using_Endianness);

      -- Create the block header
      --
      BRBON.Header.Single_Item_File.Create
        (
         In_Container => Block.Container,
         Header_Byte_Count => Header_Byte_Count,
         Options => Options,
         Origin => Origin,
         Identifier => Identifier,
         Extension => Extension,
         Path_Prefix => Path_Prefix,
         Acquisition_URL => Acquisition_URL,
         Target_List => Target_List,
         Public_Key_URL => Public_Key_URL,
         Creation_Timestamp => Creation_Timestamp,
         Expiry_Timestamp => Expiry_Timestamp
        );

--      Block.Header.First_Free_Byte_In_Field_Area := BRBON.Header.Fixed_Part_Byte_Count;
      Block.First_Free_Byte_In_Content_Area := Unsigned_32 (Header_Byte_Count);

      return Block;

   end Factory;


   procedure Finalization (I: in out Instance) is
   begin
      Deallocate_Array_Of_Unsigned_8 (I.Memory_Ptr);
   end Finalization;


   -- Operational Interface

   function Byte_Count (I: in out Instance) return Unsigned_32 is
   begin
      return I.Container.Byte_Count;
   end Byte_Count;

   function Free_Area_Byte_Count (I: in out Instance) return Unsigned_32 is
      B: constant Unsigned_32 := I.Byte_Count;
      F: constant Unsigned_32 := I.First_Free_Byte_In_Content_Area;
      V: constant Unsigned_32 := F + BRBON.Footer.Footer_Byte_Count (Single_Item_File);
   begin
      if V > B then
         return 0; -- cannot return negative
      else
         return B - V;
      end if;
   end Free_Area_Byte_Count;


end BRBON.Block.Static_Unprotected;

-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;

with BRBON.Utils;
with BRBON.Block; use BRBON.Block;
with BRBON.Footer;
with BRBON.Header;


package body BRBON.Block.Static_Unprotected is

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
       Creation_Timestamp: Unsigned_64 := Utils.Milli_Sec_Since_Jan_1_1970;
       Expiry_Timestamp: Unsigned_64 := 16#7FFF_FFFF_FFFF_FFFF#
      ) return Instance is

      Field_Storage_Byte_Count: Unsigned_16;
      Header_Type_Dependent_Byte_count: Unsigned_16;
      Header_Byte_Count: Unsigned_16;

      Content_Byte_Count: Unsigned_32;
      Block_Byte_Count: Unsigned_32;

      New_Block: Instance;

   begin

      -- Check if block type is supported
      --
      if Type_Of_Block /= Single_Item then raise BRBON.Illegal_Block_Type; end if;


      -- Calculate the size of the storage field in de header
      --
      Field_Storage_Byte_Count :=
        Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Origin'Length))
        + Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Identifier'Length))
        + Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Extension'Length))
        + Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Path_Prefix'Length))
        + Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Acquisition_URL'Length))
        + Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Target_List'Length))
        + Utils.Round_Up_To_Nearest_Multiple_of_8 (Unsigned_16 (Public_Key_URL'Length));


      New_Line (2); Put_Line ("Field Storage Byte Count = " & Field_Storage_Byte_Count'Image);


      -- Get the type dependent size
      --
      case Type_Of_Block is
         when Illegal => raise BRBON.Buffer_Error;
         when Single_Item => Header_Type_Dependent_Byte_Count := 0;
      end case;

      -- Calculate the header size
      --
      Header_Byte_Count := Header.Fixed_Part_Byte_Count + Header_Type_Dependent_Byte_Count + Field_Storage_Byte_Count + Header.Past_Field_Storage_Byte_Count;

      New_Line (2); Put_Line ("Header Byte Count = " & Header_Byte_Count'Image);


      -- Calculate the size of the block content field
      --
      Content_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Minimum_Byte_Count);

      New_Line (2); Put_Line ("Content Byte Count = " & Content_Byte_Count'Image);


      -- Calculate the size of the block
      --
      Block_Byte_Count := Unsigned_32 (Unsigned_32 (Header_Byte_Count) + Content_Byte_Count + Footer.Footer_Byte_Count (Single_Item));


      New_Line (2); Put_Line ("Block Byte Count = " & Block_Byte_Count'Image);


      -- Allocate memory area for the container that will enclose the block
      --
      New_Block.Memory_Ptr := new Types.Array_Of_Unsigned_8 (0 .. Block_Byte_Count - 1);

      -- Create the container for the block
      --
      New_Block.Container := Container.Factory (Buffer_Ptr => New_Block.Memory_Ptr, Using_Endianness => Using_Endianness);

      -- Create the block header
      --
      Block.Create_Single_Item_Block_Header
        (
         In_Block           => New_Block,
         Field_Storage_Byte_Count => Field_Storage_Byte_Count,
         Header_Byte_Count  => Header_Byte_Count,
         Options            => Options,
         Origin             => Origin,
         Identifier         => Identifier,
         Extension          => Extension,
         Path_Prefix        => Path_Prefix,
         Acquisition_URL    => Acquisition_URL,
         Target_List        => Target_List,
         Public_Key_URL     => Public_Key_URL,
         Creation_Timestamp => Creation_Timestamp,
         Expiry_Timestamp   => Expiry_Timestamp
        );


      New_Block.First_Free_Byte_In_Header_Field_Storage := Header.Fixed_Part_Byte_Count;
      New_Block.First_Free_Byte_In_Payload := Unsigned_32 (Header_Byte_Count);
      New_Block.Last_Free_Byte_In_Payload := New_Block.Container.Byte_Count - Footer.Footer_Byte_Count (Types.Single_Item) - 1;

      return New_Block;

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
      F: constant Unsigned_32 := I.First_Free_Byte_In_Payload;
      V: constant Unsigned_32 := F + Footer.Footer_Byte_Count (Types.Single_Item);
   begin
      if V > B then
         return 0; -- cannot return negative
      else
         return B - V;
      end if;
   end Free_Area_Byte_Count;

   procedure Header_Update_Header_Crc16 (I: in out Instance) is
   begin
      null;
   end Header_Update_Header_Crc16;


end BRBON.Block.Static_Unprotected;

-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;

with BRBON.Utils;
with BRBON.Block; use BRBON.Block;
with BRBON.Footer;
with BRBON.Header;
with Ada.Exceptions;


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
      if Type_Of_Block /= Single_Item then
         Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Block type not (yet) supported (" & Type_Of_Block'Image & ")");
      end if;


      -- Calculate the size of the storage field in de header
      --
      Field_Storage_Byte_Count :=
        Utils.Round_Up_To_Nearest_Multiple_of_8
          (
           Unsigned_16
             (
              Origin'Length
              + Identifier'Length
              + Extension'Length
              + Path_Prefix'Length
              + Acquisition_URL'Length
              + Target_List'Length
              + Public_Key_URL'Length
             )
          );


      -- Get the type dependent size
      --
      case Type_Of_Block is
         when Illegal => raise BRBON.Buffer_Error;
         when Single_Item => Header_Type_Dependent_Byte_Count := 0;
      end case;


      -- Calculate the header size
      --
      Header_Byte_Count := Header.Fixed_Part_Byte_Count + Header_Type_Dependent_Byte_Count + Field_Storage_Byte_Count + Header.Past_Field_Storage_Byte_Count;


      -- Calculate the size of the block content field
      --
      Content_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Minimum_Byte_Count);


      -- Calculate the size of the block
      --
      Block_Byte_Count := Unsigned_32 (Unsigned_32 (Header_Byte_Count) + Content_Byte_Count + Footer.Footer_Byte_Count (Single_Item));


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


      New_Block.First_Free_Byte_In_Payload := Unsigned_32 (Header_Byte_Count);
      New_Block.Last_Free_Byte_In_Payload := New_Block.Container.Byte_Count - Footer.Footer_Byte_Count (Types.Single_Item) - 1;

      return New_Block;

   end Factory;


   procedure Finalization (I: in out Instance) is
   begin
      Deallocate_Array_Of_Unsigned_8 (I.Memory_Ptr);
   end Finalization;


   -- Operational Interface

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


   procedure Create_Root_Item (I: in out Instance; Of_Type: Item.Item_Type; With_Byte_Count: Unsigned_32) is
   begin

      if With_Byte_Count > I.Free_Area_Byte_Count then
         Ada.Exceptions.Raise_Exception (Storage_Error'Identity, "Block storage insufficient for requested byte count");
      end if;

      case Of_Type is
         when Item.Illegal =>
            Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create top level item 'illegal'");
         when Item.Null_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Null_Type");
         when Item.Bool_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Bool_Type");
         when Item.Int_8_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Int_8_Type");
         when Item.Int_16_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Int_16_Type");
         when Item.Int_32_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Int_32_Type");
         when Item.Int_64_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Int_64_Type");
         when Item.UInt_8_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for UInt_8_Type");
         when Item.UInt_16_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for UInt_16_Type");
         when Item.UInt_32_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for UInt_32_Type");
         when Item.UInt_64_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for UInt_64_Type");
         when Item.Float_32_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Float_32_Type");
         when Item.Float_64_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Float_64_Type");
         when Item.String_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for String_Type");
         when Item.Crc_String_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Crc_String_Type");
         when Item.Binary_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Binary_Type");
         when Item.Crc_Binary_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Crc_Binary_Type");
         when Item.Array_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Array_Type");
         when Item.Dictionary_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Dictionary_Type");
         when Item.Sequence_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Sequence_Type");
         when Item.Table_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Table_Type");
         when Item.UUID_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for UUID_Type");
         when Item.RGBA_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for RGBA_Type");
         when Item.Font_Type =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Block.Static_Unprotected.Create_Root_Item not yet implemented for Font_Type");
      end case;

   end Create_Root_Item;

end BRBON.Block.Static_Unprotected;

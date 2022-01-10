-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with BRBON.Utils;
with BRBON.Block; use BRBON.Block;
private with BRBON.Portal;

with UUID_Package;
with CRC_Package;



package body BRBON.Block.Static_Unprotected is


   -- Body internals


   -- Implement API

   function Factory
      (
       Type_Of_Block: Block_Type;
       Minimum_Byte_Count: Unsigned_32;
       Header_Field_Storage_Byte_Count: Unsigned_16 := 1;
       Options: Block_Options := No_Block_Options;
       Using_Endianness: Byte_Storage_Order := Machine_Byte_Storage_Order;
       Origin: String := "";
       Identifier: String := "";
       Extension: String := "";
       Path_Prefix: String := "";
       Acquisition_URL: String := "";
       Target_List: String := "";
       Public_Key_URL: String := "";
       Creation_Timestamp: Unsigned_64 := Utils.Milli_Sec_Since_Jan_1_1970;
       Expiry_Timestamp: Unsigned_64 := Unsigned_64'Last
      ) return Static_Unprotected_Block is


      Field_Storage_Byte_Count: Unsigned_16;
      Header_Type_Dependent_Byte_count: Unsigned_16;
      Header_Byte_Count: Unsigned_16;

      Content_Byte_Count: Unsigned_32;
      Block_Byte_Count: Unsigned_32;

      New_Block: Static_Unprotected_Block;

   begin


      -- Check if block type is supported
      --
      if Type_Of_Block /= Single_Item_Block then
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
         when Illegal => Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Impossible to create an illegal-type block");
         when Single_Item_Block => Header_Type_Dependent_Byte_Count := 0;
      end case;


      -- Calculate the header size
      --
      Header_Byte_Count := Block_Header_Leading_Byte_Count + Header_Type_Dependent_Byte_Count + Field_Storage_Byte_Count + Block_Header_Trailing_Byte_Count;


      -- Calculate the size of the block content field
      --
      Content_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Minimum_Byte_Count);


      -- Calculate the size of the block
      --
      Block_Byte_Count := Unsigned_32 (Unsigned_32 (Header_Byte_Count) + Content_Byte_Count + Block_Footer_Byte_Count (Single_Item_Block));


      -- Allocate memory area for the container that will enclose the block
      --
      declare
         Memory_Ptr: Unsigned_8_Array_Ptr := new Unsigned_8_Array (0 .. Block_Byte_Count - 1);
      begin
         Setup (B                             => Memory_Ptr,
                For_Byte_Storage_Order        => Using_Endianness,
                With_Field_Storage_Byte_Count => )
      Container.Setup (New_Block, new Unsigned_8_Array (0 .. Block_Byte_Count - 1), Using_Endianness);


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
      New_Block.Last_Free_Byte_In_Payload := Container.Byte_Count (New_Block.Container) - Footer.Footer_Byte_Count (Types.Single_Item) - 1;

      return New_Block;

   end Factory;


   procedure Finalization (I: Static_Unprotected_Block) is
   begin
      Deallocate_Array_Of_Unsigned_8 (I.Memory_Ptr);
   end Finalization;


   -- Operational Interface

   function Free_Area_Byte_Count (S: Static_Unprotected_Block) return Unsigned_32 is
      B: constant Unsigned_32 := S.Byte_Count;
      F: constant Unsigned_32 := S.First_Free_Byte_In_Payload;
      V: constant Unsigned_32 := F + Footer.Footer_Byte_Count (Single_Item);
   begin
      if V > B then
         return 0; -- cannot return negative
      else
         return B - V;
      end if;
   end Free_Area_Byte_Count;


   function Add_Root_Item (S: Static_Unprotected_Block; Of_Type: Item_Type; With_Byte_Count: Unsigned_32; With_Name: String) return Static_Unprotected_Portal is

      Name_Assistent: Name_Field_Assistent.Instance := Name_Field_Assistent.Create_Name_Field_Assistent (With_Name);
      Item_Byte_Count: Unsigned_32;

   begin

      -- Don't create illegal types
      --
      if Of_Type = Types.Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create top level item 'illegal'");
      end if;

      -- Don't accept names that are too long
      --
      if With_Name'Length > Types.Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Types.Max_Name_Length'Image & ")");
      end if;

      -- Determine the byte count of the new item
      --
      Item_Byte_Count :=
        Name_Field_Assistent.Get_Minimum_Item_Byte_Count (Name_Assistent)
        + Utils.Round_Up_To_Nearest_Multiple_of_8
        (
         With_Byte_Count
         + Types.Item_Overhead_Byte_Count (Of_Type)
        );

      -- Ensure the type fits in the available area
      --
      if Item_Byte_Count > I.Free_Area_Byte_Count then
         Ada.Exceptions.Raise_Exception (Storage_Warning'Identity, "Block storage insufficient for requested byte count");
      end if;

      -- Create the item structure
      --
      Item.Create_Layout (Of_Type          => Of_Type,
                          In_Container     => I.Container,
                          At_Offset        => I.First_Free_Byte_In_Payload,
                          With_Name        => Name_Assistent,
                          Using_Byte_Count => Item_Byte_Count,
                          Parent_Offset    => 0);

      -- Set the free byte pointer
      --
      I.First_Free_Byte_In_Payload := I.First_Free_Byte_In_Payload + Item.Get_Byte_Count (I.Container, I.First_Free_Byte_In_Payload);


      return Static_Unprotected_Portal.Factory (I.Container, Unsigned_32 (I.Header_Get_Header_Byte_Count));

   end Add_Root_Item;


   function Add_Root_Item_Array_Type (I: in out Static_Unprotected_Block; With_Name: String; Element_Type: Types.Item_Type; Element_Byte_Count: Unsigned_32; Max_Element_Count: Unsigned_32) return Static_Unprotected_Portal is

      Name_Assistent: Name_Field_Assistent.Instance := Name_Field_Assistent.Create_Name_Field_Assistent (With_Name);
      P: Static_Unprotected_Portal;

   begin

      P := Item.Create_Array_Layout (In_Container             => I.Container,
                                     At_Offset                => 0,
                                     With_Name                => Name_Assistent,
                                     For_Element_Type         => Element_Type,
                                     Using_Element_Byte_Count => Element_Byte_Count,
                                     Max_Element_Count        => Max_Element_Count);

      -- Set the free byte pointer
      --
      I.First_Free_Byte_In_Payload := I.First_Free_Byte_In_Payload + Item.Get_Byte_Count (I.Container, Unsigned_32 (I.Header_Get_Header_Byte_Count));


      return P;

   end Add_Root_Item_Array_Type;


   function Get_Root_Item (I: in out Static_Unprotected_Block) return Static_Unprotected_Portal is
   begin
      return Static_Unprotected_Portal.Factory (I.Container, Unsigned_32 (I.Header_Get_Header_Byte_Count));
   end Get_Root_Item;


end BRBON.Block.Static_Unprotected;

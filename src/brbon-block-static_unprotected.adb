-- with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with BRBON.Utils;
with BRBON.Block; use BRBON.Block;
with BRBON.Footer;
with BRBON.Header;
with BRBON.Name_Field_Assistent;

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
         when Illegal => Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Impossible to create an illegal-type block");
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
      New_Block.Last_Free_Byte_In_Payload := Container.Byte_Count (New_Block.Container) - Footer.Footer_Byte_Count (Types.Single_Item) - 1;

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


   function Add_Root_Item (I: in out Instance; Of_Type: Types.Item_Type; With_Byte_Count: Unsigned_32; With_Name: String) return Portal.Instance is

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


      return Portal.Factory (I.Container, Unsigned_32 (I.Header_Get_Header_Byte_Count));

   end Add_Root_Item;


   function Add_Root_Item_Array_Type (I: in out Instance; With_Name: String; Element_Type: Types.Item_Type; Element_Byte_Count: Unsigned_32; Max_Element_Count: Unsigned_32) return Portal.Instance is

      Name_Assistent: Name_Field_Assistent.Instance := Name_Field_Assistent.Create_Name_Field_Assistent (With_Name);
      P: Portal.Instance;

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


   function Get_Root_Item (I: in out Instance) return Portal.Instance is
   begin
      return Portal.Factory (I.Container, Unsigned_32 (I.Header_Get_Header_Byte_Count));
   end Get_Root_Item;


   -- Structure Access

   function Get_Type (P: Portal.Instance) return Types.Item_Type is
   begin
      return Item.Get_Type (P.Container, P.Item_Offset);
   end Get_Type;


   function Get_Options (P: Portal.Instance) return Types.Item_Options is
   begin
      return Item.Get_Options (P.Container, P.Item_Offset);
   end Get_Options;


   function Get_Flags (P: Portal.Instance) return Types.Item_Flags is
   begin
      return Item.Get_Flags (P.Container, P.Item_Offset);
   end Get_Flags;


   function Get_Name (P: Portal.Instance) return String is
   begin
      return Item.Get_Name_String (P.Container, P.Item_Offset);
   end Get_Name;


   function Get_Byte_Count (P: Portal.Instance) return Unsigned_32 is
   begin
      return Item.Get_Byte_Count (P.Container, P.Item_Offset);
   end Get_Byte_Count;


   function Get_Parent_Offset (P: Portal.Instance) return Unsigned_32 is
   begin
      return Item.Get_Parent_Offset (P.Container, P.Item_Offset);
   end Get_Parent_Offset;


   function Get_Value_Area_Byte_Count (P: Portal.Instance) return Unsigned_32 is
   begin
      return Get_Byte_Count (P) - Minimum_Item_Byte_Count - Unsigned_32 (Item.Get_Name_Field_Byte_Count (P.Container, P.Item_Offset));
   end Get_Value_Area_Byte_Count;


   -- Value access

   function Get_Bool (P: Portal.Instance) return Boolean is
   begin
      return Container.Get_Bool (P.Container, Portal.Small_Value_Offset (P));
   end Get_Bool;


   procedure Set_Bool (P: Portal.Instance; Value: Boolean) is
   begin
      Container.Set_Bool (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_Bool;


   function Get_Int_8 (P: Portal.Instance) return Integer_8 is
   begin
      return Container.Get_Integer_8 (P.Container, Portal.Small_Value_Offset (P));
   end Get_Int_8;


   procedure Set_Int_8 (P: Portal.Instance; Value: Integer_8) is
   begin
      Container.Set_Integer_8 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_Int_8;


   function Get_Int_16 (P: Portal.Instance) return Integer_16 is
   begin
      return Container.Get_Integer_16 (P.Container, Portal.Small_Value_Offset (P));
   end Get_Int_16;


   procedure Set_Int_16 (P: Portal.Instance; Value: Integer_16) is
   begin
      Container.Set_Integer_16 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_Int_16;


   function Get_Int_32 (P: Portal.Instance) return Integer_32 is
   begin
      return Container.Get_Integer_32 (P.Container, Portal.Small_Value_Offset (P));
   end Get_Int_32;


   procedure Set_Int_32 (P: Portal.Instance; Value: Integer_32) is
   begin
      Container.Set_Integer_32 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_Int_32;


   function Get_Int_64 (P: Portal.Instance) return Integer_64 is
   begin
      return Container.Get_Integer_64 (P.Container, Portal.Value_Offset (P));
   end Get_Int_64;


   procedure Set_Int_64 (P: Portal.Instance; Value: Integer_64) is
   begin
      Container.Set_Integer_64 (P.Container, Portal.Value_Offset (P), Value);
   end Set_Int_64;


   function Get_UInt_8 (P: Portal.Instance) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (P.Container, Portal.Small_Value_Offset (P));
   end Get_UInt_8;


   procedure Set_UInt_8 (P: Portal.Instance; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_UInt_8;


   function Get_UInt_16 (P: Portal.Instance) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (P.Container, Portal.Small_Value_Offset (P));
   end Get_UInt_16;


   procedure Set_UInt_16 (P: Portal.Instance; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_UInt_16;


   function Get_UInt_32 (P: Portal.Instance) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (P.Container, Portal.Small_Value_Offset (P));
   end Get_UInt_32;


   procedure Set_UInt_32 (P: Portal.Instance; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_UInt_32;


   function Get_UInt_64 (P: Portal.Instance) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (P.Container, Portal.Value_Offset (P));
   end Get_UInt_64;


   procedure Set_UInt_64 (P: Portal.Instance; Value: Unsigned_64) is
   begin
      Container.Set_Unsigned_64 (P.Container, Portal.Value_Offset (P), Value);
   end Set_UInt_64;


   function Get_Float_32 (P: Portal.Instance) return IEEE_Float_32 is
   begin
      return Container.Get_Float_32 (P.Container, Portal.Small_Value_Offset (P));
   end Get_Float_32;


   procedure Set_Float_32 (P: Portal.Instance; Value: IEEE_Float_32) is
   begin
      Container.Set_Float_32 (P.Container, Portal.Small_Value_Offset (P), Value);
   end Set_Float_32;


   function Get_Float_64 (P: Portal.Instance) return IEEE_Float_64 is
   begin
      return Container.Get_Float_64 (P.Container, Portal.Value_Offset (P));
   end Get_Float_64;


   procedure Set_Float_64 (P: Portal.Instance; Value: IEEE_Float_64) is
   begin
      Container.Set_Float_64 (P.Container, Portal.Value_Offset (P), Value);
   end Set_Float_64;


   function Get_String (P: Portal.Instance) return String is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
      Byte_Count: Unsigned_32 := Container.Get_Unsigned_32 (P.Container, Value_Offset + Item.String_Byte_Count_Offset);
   begin
      return Container.Get_String (P.Container, Value_Offset + Item.String_Byte_Code_Offset, Byte_Count);
   end Get_String;


   procedure Set_String (P: Portal.Instance; Value: String) is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
   begin
      Container.Set_String (P.Container, Value_Offset + Item.String_Byte_Code_Offset, Value);
      Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.String_Byte_Count_Offset, Value'Length);
   end Set_String;


   function Get_CRC_String (P: Portal.Instance) return String is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
      Byte_Count: Unsigned_32 := Container.Get_Unsigned_32 (P.Container, Value_Offset + Item.CRC_String_Byte_Count_Offset);
   begin
      return Container.Get_String (P.Container, Value_Offset + Item.CRC_String_Byte_Code_Offset, Byte_Count);
   end Get_CRC_String;


   procedure Set_CRC_String (P: Portal.Instance; Value: String) is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
   begin
      if Value'Length > 0 then
         Container.Set_String (P.Container, Value_Offset + Item.CRC_String_Byte_Code_Offset, Value);
         Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.CRC_String_CRC_Offset, CRC_Package.Calculate_CRC_32 (Value));
      else
         Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.CRC_String_CRC_Offset, 0);
      end if;
      Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.CRC_String_Byte_Count_Offset, Value'Length);
   end Set_CRC_String;


   function Get_CRC_String_CRC (P: Portal.Instance) return Unsigned_32 is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
   begin
      return Container.Get_Unsigned_32 (P.Container, Value_Offset + Item.CRC_String_CRC_Offset);
   end Get_CRC_String_CRC;


   function Get_CRC_String_Quick_Compare (P: Portal.Instance) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (P.Container, Portal.Value_Offset (P));
   end Get_CRC_String_Quick_Compare;


   function Get_Binary (P: Portal.Instance) return Array_Of_Unsigned_8 is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
      Byte_Count: Unsigned_32 := Container.Get_Unsigned_32 (P.Container, Value_Offset + Item.Binary_Byte_Count_Offset);
   begin
      return Container.Get_Unsigned_8_Array (P.Container, Value_Offset + Item.Binary_Byte_Code_Offset, Byte_Count);
   end Get_Binary;


   procedure Set_Binary (P: Portal.Instance; Value: Array_Of_Unsigned_8) is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
   begin
      Container.Set_Unsigned_8_Array (P.Container, Value_Offset + Item.Binary_Byte_Code_Offset, Value);
      Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.Binary_Byte_Count_Offset, Value'Length);
   end Set_Binary;


   function Get_CRC_Binary (P: Portal.Instance) return Array_Of_Unsigned_8 is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
      Byte_Count: Unsigned_32 := Container.Get_Unsigned_32 (P.Container, Value_Offset + Item.CRC_Binary_Byte_Count_Offset);
   begin
      return Container.Get_Unsigned_8_Array (P.Container, Value_Offset + Item.CRC_Binary_Byte_Code_Offset, Byte_Count);
   end Get_CRC_Binary;


   procedure Set_CRC_Binary (P: Portal.Instance; Value: Array_Of_Unsigned_8) is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
   begin
      if Value'Length > 0 then
         Container.Set_Unsigned_8_Array (P.Container, Value_Offset + Item.CRC_Binary_Byte_Code_Offset, Value);
         Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.CRC_Binary_CRC_Offset, CRC_Package.Calculate_CRC_32 (Value));
      else
         Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.CRC_Binary_CRC_Offset, 0);
      end if;
      Container.Set_Unsigned_32 (P.Container, Value_Offset + Item.CRC_Binary_Byte_Count_Offset, Value'Length);
   end Set_CRC_Binary;


   function Get_CRC_Binary_CRC (P: Portal.Instance) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (P.Container, Portal.Value_Offset (P) + Item.CRC_Binary_CRC_Offset);
   end Get_CRC_Binary_CRC;


   function Get_CRC_Binary_Quick_Compare (P: Portal.Instance) return Unsigned_64 is
   begin
      return Container.Get_Unsigned_64 (P.Container, Portal.Value_Offset (P));
   end;


   function Get_UUID (P:Portal.Instance) return UUID_Package.UUID is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
      Bytes: Array_Of_Unsigned_8 := Container.Get_Unsigned_8_Array (P.Container, Value_Offset, 16);
   begin
      return UUID_Package.Factory (Bytes);
   end Get_UUID;


   procedure Set_UUID (P: Portal.Instance; Value: UUID_Package.UUID) is
      Value_Offset: Unsigned_32 := Portal.Value_Offset (P);
   begin
      Container.Set_Unsigned_8_Array (P.Container, Value_Offset, UUID_Package.Get_Bytes(Value));
   end Set_UUID;


   function Get_Color (P:Portal.Instance) return Color_Package.Color is
      Small_Value_Offset: Unsigned_32 := Portal.Small_Value_Offset (P);
      R, G, B, A: Unsigned_8;
   begin
      R := Container.Get_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Red_Offset);
      G := Container.Get_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Green_Offset);
      B := Container.Get_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Blue_Offset);
      A := Container.Get_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Alpha_Offset);
      return Color_Package.Factory (R, G, B, A);
   end Get_Color;


   procedure Set_Color (P: Portal.Instance; Value: Color_Package.Color) is
      Small_Value_Offset: Unsigned_32 := Portal.Small_Value_Offset (P);
   begin
      Container.Set_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Red_Offset, Color_Package.Get_Red_Component (Value));
      Container.Set_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Green_Offset, Color_Package.Get_Green_Component (Value));
      Container.Set_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Blue_Offset, Color_Package.Get_Blue_Component (Value));
      Container.Set_Unsigned_8 (P.Container, Small_Value_Offset + Item.Color_Alpha_Offset, Color_Package.Get_Alpha_Component (Value));
   end Set_Color;


   function Get_Element_Count (P: Portal.Instance) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (P.Container, Portal.Value_Offset (P) + Item.Array_Element_Count_Offset);
   end Get_Element_Count;


   procedure Decrement_Element_Count (P: Portal.Instance) is
      Element_Count: Unsigned_32 := Get_Element_Count (P);
   begin
      if Element_Count > 0 then
         Element_Count := Element_Count - 1;
      end if;
      Container.Set_Unsigned_32 (P.Container, Portal.Value_Offset (P) + Item.Array_Element_Count_Offset, Element_Count);
   end Decrement_Element_Count;


   procedure Increment_Element_Count (P: Portal.Instance) is
      Element_Count: Unsigned_32 := Get_Element_Count (P);
   begin
      Element_Count := Element_Count + 1;
      Container.Set_Unsigned_32 (P.Container, Portal.Value_Offset (P) + Item.Array_Element_Count_Offset, Element_Count);
   end Increment_Element_Count;


   function Get_Element_Byte_Count (P: Portal.Instance) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (P.Container, Portal.Value_Offset (P) + Item.Array_Element_Byte_Count_Offset);
   end Get_Element_Byte_Count;


   function Get_Element (P: Portal.Instance; From_Index: Unsigned_32) return Portal.Instance is
   begin
      return Portal.Factory (P.Container, Portal.Value_Offset (P) + Item.Array_Element_Start_Offset + From_Index * Get_Element_Byte_Count (P), Portal.Element, From_Index, 0);
   end Get_Element;


   function Add_Element (P: Portal.Instance) return Portal.Instance is
      New_Index: Unsigned_32 := Portal.Value_Offset (P) + Item.Array_Element_Start_Offset + (Get_Element_Count (P) + 1) * Get_Element_Byte_Count (P);
   begin
      Increment_Element_Count (P);
      return Portal.Factory (P.Container, New_Index, Portal.Element, Get_Element_Count (P));
   end Add_Element;


   procedure Remove_Last_Element (P: Portal.Instance) is
      Element_Offset: Unsigned_32 := Get_Element_Byte_Count (P) * (Get_Element_Count (P) - 1);
   begin
      Decrement_Element_Count (P);
      declare
         Arr: Array_Of_Unsigned_8 (1 .. Get_Element_Byte_Count (P)) := (others => 0);
      begin
         Container.Set_Unsigned_8_Array (P.Container, Portal.Value_Offset (P) + Item.Array_Element_Start_Offset + Element_Offset, Arr);
      end;
   end Remove_Last_Element;


end BRBON.Block.Static_Unprotected;

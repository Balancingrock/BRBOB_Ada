with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with GNAT.Byte_Swapping;

with Item_Static_Unprotected; use Item_Static_Unprotected;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;
with CRC_Package; use CRC_Package;
with BRBON.Block.Header; use BRBON.Block.Header;


package body BRBON.Static_Unprotected is


   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);

   procedure Create_Block_Single_Item_File (S: out Static_Unprotected_Store'Class; With_Item_Type: BR_Item_Type := BR_Dictionary) is

      A: Item_Access;

   begin

      -- Create the block header including the type-1 part
      --
      S.Create_Block_Type_1_Single_Item_File;

      -- Set the offset for the first item
      --
--      S.First_Item_Offset := Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset;

      -- Create a shortcut (Item_Access) for the first item
      --
      A := (S.Store_Ptr, S.First_Item_Offset);

      -- Create the initial type
      --
      case With_Item_Type is
         when BR_Dictionary => A.Create_Dictionary_Item (Parent_Offset => A.Offset); -- Parent references itself
         when others => raise Incomplete_Code;
      end case;

      -- Initialize the remainder of the block info
      --
      S.Block_Type := Single_Item_File;
      S.Free_Area_Offset := 0;
--      S.Update_Free_Area_Offset (Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset + A.Item_Byte_Count); -- Will also set the block header


   end Create_Block_Single_Item_File;

--   function Get_Block_Info (Path: String) return Block_Info is

--      subtype Header_Type is Array_Of_Unsigned_8 (0 .. Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset);
--      Header_Buffer: Header_Type := (others => 0);

--      File: File_Type;
--      In_Stream: Stream_Access;

--      Using_Endianness: Endianness;
      --Block_Ptr: BR_Block_Ptr;

--      Block_Length: Unsigned_32;

--   begin

      -- Open the file and read the header
      -- ---------------------------------

--      Open (File => File, Mode => In_File, Name => Path);

--      if Unsigned_64 (Size (File)) < Unsigned_64 (Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset) then raise File_IO_Error with "File too small"; end if;

--      In_Stream := Stream (File);

--      Header_Type'Read (In_Stream, Header_Buffer);


      -- Verify that the header is ok.
      -- -----------------------------

      -- Marker bytes
--      if Header_Buffer (Block_Header_Start_Marker_Byte_1_Offset) /= 16#A5# then raise Invalid_Block with "First header marker not found"; end if;
--      if Header_Buffer (Block_Header_Start_Marker_Byte_2_Offset) /= 16#7E# then raise Invalid_Block with "Second header marker not found"; end if;
--      if Header_Buffer (Block_Header_Start_Marker_Byte_3_Offset) /= 16#81# then raise Invalid_Block with "Third header marker not found"; end if;
--      if Header_Buffer (Block_Header_Start_Marker_Byte_4_Offset) /= 16#5A# then raise Invalid_Block with "Fourth header marker not found"; end if;

      -- Block type
--      if Header_Buffer (Block_Header_Type_Offset) /= To_Unsigned_8 (Single_Item_File) then raise Invalid_Block with "Expected block type 1"; end if;

      -- Block options
--      declare
--         Options: BR_Block_Options := To_BR_Block_Options (Header_Buffer (Block_Header_Options_Offset));
--      begin
--         if Options.Bit_6 or Options.Bit_5 or Options.Bit_4 or Options.Bit_3 or Options.Bit_2 or Options.Bit_1 or Options.Bit_0 then raise Invalid_Block with "Unexpected options"; end if;
--         Using_Endianness := (if Options.Endianness then Big else Little);
--      end;

      -- Header size
--      declare
--         Header_Length: Unsigned_16 := To_Unsigned_16 (Header_Buffer (Block_Header_Header_Byte_Count_Offset .. Block_Header_Header_Byte_Count_Offset + 1));
--      begin
--         if Using_Endianness /= Machine_Endianness then Header_Length := Swap_Unsigned_16 (Header_Length); end if;
--         if Unsigned_32 (Header_Length) /= Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset then raise Invalid_Block with "Block header byte count error"; end if;
--      end;

      -- Block size (is not necessarily the same as the file size)
--      Block_Length := To_Unsigned_32 (Header_Buffer (Block_Type_Dependent_Header_Offset + Type_1_Block_Byte_Count_Offset .. Block_Type_Dependent_Header_Offset + Type_1_Block_Byte_Count_Offset + 3));
--      if Using_Endianness /= Machine_Endianness then Block_Length := Swap_Unsigned_32 (Block_Length); end if;

      -- Item count
--      declare
--         Item_Count: Unsigned_16 := To_Unsigned_16 (Header_Buffer (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_Item_Count_Offset .. Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_Item_Count_Offset + 1));
--      begin
--         if Using_Endianness /= Machine_Endianness then Item_Count := Swap_Unsigned_16 (Item_Count); end if;
--         if Item_Count /= 1 then raise Invalid_Block with "Only 1 item expected"; end if;
--      end;

      -- CRC check
--      declare
--         Expected_CRC: Unsigned_16 := To_Unsigned_16 (Header_Buffer (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset .. Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset + 1));
--         Actual_CRC: Unsigned_16 := Calculate_CRC_16 (Header_Buffer (0 .. Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset - 1));
--      begin
--         if Using_Endianness /= Machine_Endianness then Expected_CRC := Swap_Unsigned_16 (Expected_CRC); end if;
--         if Expected_CRC /= Actual_CRC then raise Invalid_Block with "CRC check failed"; end if;
--      end;


      -- Header check completed with success

--      Close (File);


      -- Return the new block

--      return (Block_Length, Using_Endianness);

--   end Get_Block_Info;



--   procedure Create_Block_Header (S: in out Static_Unprotected_Store'Class) is

--      Block_Options: BR_Block_Options :=
--        (
--         Endianness => S.Store_Ptr.Uses_Endianness = Big,
--         Bit_6 => False,
--         Bit_5 => False,
--         Bit_4 => False,
--         Bit_3 => False,
--         Bit_2 => False,
--         Bit_1 => False,
--         Bit_0 => False
--        );

--   begin

--      S.Store_Ptr.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_1_Offset, 16#A5#);
--      S.Store_Ptr.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_2_Offset, 16#7E#);
--      S.Store_Ptr.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_3_Offset, 16#81#);
--      S.Store_Ptr.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_4_Offset, 16#5A#);
--      S.Store_Ptr.Set_Unsigned_8 (Block_Header_Type_Offset, To_Unsigned_8 (Single_Item_File));
--      S.Store_Ptr.Set_Unsigned_8 (Block_Header_Options_Offset, To_Unsigned_8 (Block_Options));

--      null;
--   end;


   procedure Create_Block_Type_1_Single_Item_File (S: in out Static_Unprotected_Store'Class) is
   begin
--      S.Create_Block_Header;
--      S.Store_Ptr.Set_Unsigned_32 (Block_Type_Dependent_Header_Offset + Type_1_Block_Byte_Count_Offset, 0);        -- Is updated continously
--      S.Store_Ptr.Set_Unsigned_16 (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_Item_Count_Offset, 1);  -- Should always be 1 for a type 1 block
--      S.Store_Ptr.Set_Unsigned_16 (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset, 0);      -- Will need to be filled in before saving to file
      null;
   end Create_Block_Type_1_Single_Item_File;


--   procedure Write_To_File (B: in out Static_Unprotected_Store'Class; Filepath: String) is
--   begin
--      B.Update_Header_Crc;
--      B.Store.Write_to_File (Filepath);
--   end Write_To_File;


--   function Get_Number_Of_Items (B: in out Static_Unprotected_Store'Class) return Unsigned_32 is
--   begin
--      case B.Block_Type is
--         when Single_Item_File => return 1;
--         when others => raise Illegal_Block_Type;
--      end case;
--   end Get_Number_Of_Items;


--   function Get_Item (B: in out Static_Unprotected_Store'Class; Index: Unsigned_32 := 1) return BR_Static_Unprotected_Item is
--   begin
--      case B.Block_Type is
--         when Single_Item_File =>
--            if Index /= 1 then raise Index_Out_Of_Range; end if;
--            return (B.Store, B.First_Item_Offset);
--         when others => raise Illegal_Block_Type;
--      end case;
--   end Get_Item;


   procedure Update_Free_Area_Offset (S: in out Static_Unprotected_Store'Class; Increment: Unsigned_32) is
   begin
--      S.Free_Area_Offset := S.Free_Area_Offset + Increment;
--      case S.Block_Type is
--         when Single_Item_File => S.Store_Ptr.Set_Unsigned_32 (Block_Type_Dependent_Header_Offset + Type_1_Block_Byte_Count_Offset, S.Free_Area_Offset);
--         when others => raise Incomplete_Code;
--      end case;
      null;
   end Update_Free_Area_Offset;


--   procedure Test_Support_Update_Header_CRC (B: in out Static_Unprotected_Store'Class) is
--   begin
--      B.Update_Header_Crc;
--   end Test_Support_Update_Header_CRC;


--   procedure Update_Header_Crc (B: in out Static_Unprotected_Store'Class) is
--   begin
--      case B.Block_Type is
--         when Single_Item_File =>
--            declare
--               Crc: Unsigned_16 := Calculate_CRC_16 (B.Store.Get_Unsigned_8_Array (0, Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset));
--            begin
--               B.Store.Set_Unsigned_16 (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset, Crc);
--            end;
--         when others => raise Incomplete_Code;
--      end case;
--   end Update_Header_Crc;


end BRBON.Static_Unprotected;

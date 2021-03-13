with Item_Static_Unprotected; use Item_Static_Unprotected;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;
with CRC_Package; use CRC_Package;


package body BRBON.Static_Unprotected is


   -- First 32 bits
   Block_Header_Start_Marker_Byte_1_Offset: Unsigned_32 := 0; -- 1 byte
   Block_Header_Start_Marker_Byte_2_Offset: Unsigned_32 := 1; -- 1 byte
   Block_Header_Start_Marker_Byte_3_Offset: Unsigned_32 := 2; -- 1 byte
   Block_Header_Start_Marker_Byte_4_Offset: Unsigned_32 := 3; -- 1 byte

   -- Second 32 bits
   Block_Header_Type_Offset: Unsigned_32 := 4;                -- 1 byte
   Block_Header_Options_Offset: Unsigned_32 := 5;             -- 1 byte
   Block_Header_Header_Byte_Count_Offset: Unsigned_32 := 6;   -- 2 bytes

   Block_Type_Dependent_Header_Offset: Unsigned_32 := 8;


   -- Type 1: Single file

   Type_1_Block_Byte_Count_Offset: Unsigned_32 := 0;          -- 4 bytes

   Type_1_Header_Tail_Item_Count_Offset: Unsigned_32 := 4;    -- 2 bytes
   Type_1_Header_Tail_CRC_16_Offset: Unsigned_32 := 6;        -- 2 bytes

   Type_1_Payload_Offset: Unsigned_32 := 8;


   function Block_Factory_Create_Single_Item_File
     (
      With_Item_Type: BR_Item_Type := BR_Dictionary;
      In_Byte_Store: Byte_Store_Ptr
     )
      return BR_Block is

      B: BR_Block := (In_Byte_Store, Illegal, 0, 0);
      A: Item_Access;

   begin

      -- Create the block header including the type-1 part
      --
      B.Create_Block_Type_1_Single_Item_File;

      -- Set the offset for the first item
      --
      B.First_Item_Offset := Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset;

      -- Create a shortcut (Item_Access) for the first item
      --
      A := (B.Store, B.First_Item_Offset);

      -- Create the initial type
      --
      case With_Item_Type is
         when BR_Dictionary => A.Create_Dictionary_Item (Parent_Offset => A.Offset); -- Parent references itself
         when others => raise Incomplete_Code;
      end case;

      -- Initialize the remainder of the block info
      --
      B.Block_Type := Single_Item_File;
      B.Free_Area_Offset := 0;
      B.Update_Free_Area_Offset (Block_Type_Dependent_Header_Offset + Type_1_Payload_Offset + A.Item_Byte_Count); -- Will also set the block header

      return B;

   end Block_Factory_Create_Single_Item_File;

   function Block_Factory_Open_Single_Item_File (B: Byte_Store_Ptr) return Boolean is --BR_Block is
   begin

      -- Verify that the header is ok.

      if B.Get_Unsigned_8 (Block_Header_Start_Marker_Byte_1_Offset) /= 16#A5# then raise Invalid_Block_Header; end if;
      if B.Get_Unsigned_8 (Block_Header_Start_Marker_Byte_2_Offset) /= 16#7E# then raise Invalid_Block_Header; end if;
      if B.Get_Unsigned_8 (Block_Header_Start_Marker_Byte_3_Offset) /= 16#81# then raise Invalid_Block_Header; end if;
      if B.Get_Unsigned_8 (Block_Header_Start_Marker_Byte_4_Offset) /= 16#5A# then raise Invalid_Block_Header; end if;

      if B.Get_Unsigned_8 (Block_Header_Type_Offset) /= To_Unsigned_8 (Single_Item_File) then raise Illegal_Block_Type; end if;

      declare
         Options: BR_Block_Options := To_BR_Block_Options (B.Get_Unsigned_8 (Block_Header_Options_Offset));
      begin
         if Options.Bit_6 or Options.Bit_5 or Options.Bit_4 or Options.Bit_3 or Options.Bit_2 or Options.Bit_1 or Options.Bit_0 then raise Illegal_Block_Options; end if;
         if Options.Endianness = True and B.Uses_Endianness = Little then raise Endianness_Mismatch; end if;
         if Options.Endianness = False and B.Uses_Endianness = Big then raise Endianness_Mismatch; end if;
      end;

      raise Incomplete_Code;

      return True;

   end Block_Factory_Open_Single_Item_File;

   function Get_File_Info (Filepath: String) return Block_File_info is
   begin
      raise Incomplete_Code;
      return (OK, 100, Big);
   end Get_File_Info;

   procedure Create_Block_Header (B: in out BR_Block'Class) is

      Block_Options: BR_Block_Options :=
        (
         Endianness => B.Store.Uses_Endianness = Big,
         Bit_6 => False,
         Bit_5 => False,
         Bit_4 => False,
         Bit_3 => False,
         Bit_2 => False,
         Bit_1 => False,
         Bit_0 => False
        );

   begin

      B.Store.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_1_Offset, 16#A5#);
      B.Store.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_2_Offset, 16#7E#);
      B.Store.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_3_Offset, 16#81#);
      B.Store.Set_Unsigned_8 (Block_Header_Start_Marker_Byte_4_Offset, 16#5A#);
      B.Store.Set_Unsigned_8 (Block_Header_Type_Offset, To_Unsigned_8 (Single_Item_File));
      B.Store.Set_Unsigned_8 (Block_Header_Options_Offset, To_Unsigned_8 (Block_Options));

   end;


   procedure Create_Block_Type_1_Single_Item_File (B: in out BR_Block'Class) is
   begin
      B.Create_Block_Header;
      B.Store.Set_Unsigned_32 (Block_Type_Dependent_Header_Offset + Type_1_Block_Byte_Count_Offset, 0);        -- Is updated continously
      B.Store.Set_Unsigned_16 (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_Item_Count_Offset, 1);  -- Should always be 1 for a type 1 block
      B.Store.Set_Unsigned_16 (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset, 0);      -- Will need to be filled in before saving to file
   end Create_Block_Type_1_Single_Item_File;


   procedure Write_To_File (B: in out BR_Block'Class; Filepath: String) is
   begin
      B.Update_Header_Crc;
      B.Store.Write_to_File (Filepath);
   end Write_To_File;


   function Get_Number_Of_Items (B: in out BR_Block'Class) return Unsigned_32 is
   begin
      case B.Block_Type is
         when Single_Item_File => return 1;
         when others => raise Illegal_Block_Type;
      end case;
   end Get_Number_Of_Items;


   function Get_Item (B: in out BR_Block'Class; Index: Unsigned_32 := 1) return BR_Static_Unprotected_Item is
   begin
      case B.Block_Type is
         when Single_Item_File =>
            if Index /= 1 then raise Index_Out_Of_Range; end if;
            return (B.Store, B.First_Item_Offset);
         when others => raise Illegal_Block_Type;
      end case;
   end Get_Item;


   procedure Update_Free_Area_Offset (B: in out BR_Block'Class; Increment: Unsigned_32) is
   begin
      B.Free_Area_Offset := B.Free_Area_Offset + Increment;
      case B.Block_Type is
         when Single_Item_File => B.Store.Set_Unsigned_32 (Block_Type_Dependent_Header_Offset + Type_1_Block_Byte_Count_Offset, B.Free_Area_Offset);
         when others => raise Incomplete_Code;
      end case;
   end Update_Free_Area_Offset;


   procedure Test_Support_Update_Header_CRC (B: in out BR_Block'Class) is
   begin
      B.Update_Header_Crc;
   end Test_Support_Update_Header_CRC;


   procedure Update_Header_Crc (B: in out BR_Block'Class) is
   begin
      case B.Block_Type is
         when Single_Item_File =>
            declare
               Crc: Unsigned_16 := Calculate_CRC_16 (B.Store.Get_Unsigned_8_Array (0, Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset));
            begin
               B.Store.Set_Unsigned_16 (Block_Type_Dependent_Header_Offset + Type_1_Header_Tail_CRC_16_Offset, Crc);
            end;
         when others => raise Incomplete_Code;
      end case;
   end Update_Header_Crc;


end BRBON.Static_Unprotected;

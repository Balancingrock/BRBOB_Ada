with Item_Static_Unprotected; use Item_Static_Unprotected;
with BRBON.Container; use BRBON.Container;
with BRBON.Configure; use BRBON.Configure;


package body BRBON.Static_Unprotected is


   Block_Header_Start_Marker_Byte_1_Offset: Unsigned_32 := 0;
   Block_Header_Start_Marker_Byte_2_Offset: Unsigned_32 := 1;
   Block_Header_Start_Marker_Byte_3_Offset: Unsigned_32 := 2;
   Block_Header_Start_Marker_Byte_4_Offset: Unsigned_32 := 3;
   Block_Header_Type_Offset: Unsigned_32 := 4;
   Block_Header_Options_Offset: Unsigned_32 := 5;
   Block_Header_Header_Byte_Count_Offset: Unsigned_32 := 6;

   Block_Type_1_Block_Byte_Count_Offset: Unsigned_32 := 8;
   Block_Type_1_Reserved_Offset: Unsigned_32 := 12;
   Block_Type_1_First_Item_Offset: Unsigned_32 := 16;


   function Block_Factory_Create_Single_Item_File
     (
      With_Item_Type: BR_Item_Type := BR_Dictionary;
      In_Byte_Store: Byte_Store_Ptr
     )
      return BR_Block is

      B: BR_Block := (In_Byte_Store, Illegal, 0, 0);
      A: Item_Access;

   begin

      B.Create_Block_Type_1_Single_Item_File;
      B.First_Item_Offset := Block_Type_1_First_Item_Offset;

      A := (B.Store, Block_Type_1_First_Item_Offset);

      A.Create_Dictionary_Item (Parent_Offset => Block_Type_1_First_Item_Offset);

      B.Free_Area_Offset := Block_Type_1_First_Item_Offset + A.Item_Byte_Count;

      B.Block_Type := Single_Item_File;

      return B;

   end Block_Factory;

   function Block_Factory_Open_Single_Item_File (B: Byte_Store_Ptr) return BR_Block is
   begin
      -- Verify that the header is ok.
   end Block_Factory_Open_Single_Item_File;


   procedure Create_Block_Header (B: in out BR_Block) is

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


   procedure Create_Block_Type_1_Single_Item_File (B: in out BR_Block) is
   begin
      B.Create_Block_Header;
      B.Store.Set_Unsigned_32 (Block_Type_1_Block_Byte_Count_Offset, 0); -- Will need to be filled in before saving to file
      B.Store.Set_Unsigned_32 (Block_Type_1_Reserved_Offset, 0);
   end Create_Block_Type_1_Single_Item_File;


   procedure Write_To_File (B: in out BR_Block'Class; Filepath: String) is
   begin
      B.Store.Write_to_File (Filepath);
   end Write_To_File;


   function Get_Number_Of_Items (B: in out BR_Block'Class) return Unsigned_32 is
   begin
      case B.Block_Type is
         when Single_Item_File => return 1;
         when others => raise Illegal_Block_Type;
      end case;
   end Get_Number_Of_Items;


   function Get_Item (B: BR_Block'Class; Index: Unsigned_32 := 1) return BR_Static_Unprotected_Item is
   begin
      case B.Block_Type is
         when Single_Item_File =>
            if Index /= 1 then raise Index_Out_Of_Range; end if;
            return (B.Store, B.First_Item_Offset);
         when others => raise Illegal_Block_Type;
      end case;
   end Get_Item;

   procedure Update_Free_Area_Offset (B: in out BR_Block; Increment: Unsigned_32) is
   begin
      B.Free_Area_Offset := B.Free_Area_Offset + Increment;
      case B.Block_Type is
         when Single_Item_File => B.Store.Set_Unsigned_32 (Block_Type_1_Block_Byte_Count_Offset, B.Free_Area_Offset);
         when others => raise Illegal_Block_Type;
      end case;
   end Update_Free_Area_Offset;

end BRBON.Static_Unprotected;

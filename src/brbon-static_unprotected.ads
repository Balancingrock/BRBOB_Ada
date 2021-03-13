with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;


package BRBON.Static_Unprotected is

   -- Controls access to a block.
   --
   type BR_Block is tagged private;

   -- Controls access to the static unprotected items in the block.
   --
   type BR_Static_Unprotected_Item is tagged private;

   -- The possible status of a file info request.
   --
   type File_Status is (Not_Found, Cannot_Read, OK);

   -- Contains information about a file that can be used to create the necessary data structures to open a block file with allocating dynamic memory.
   --
   type Block_File_info is record
      Status: File_Status;
      Byte_Count: Unsigned_32; -- The byte count necessary to read a file, note that this may be smaller than the file size itself.
      Using_Endianness: Endianness;  -- The endianess for the buffer to read the file into.
   end record;

   -- Callback for an operation that allocates a block of memory to read in a file
   --
   type Byte_Store_Allocator is access function (Info: Block_File_info) return Byte_Store;

   -- Create a new single-item-file type block.
   -- You will need to provide a byte-store. This can be created by the Byte_Store_Factory in BRBON.Container.
   -- @param With_Item_Type The type of item to create in the block.
   -- @param In_Byte_Store The byte store to use, note that any data in it will be overwritten.
   -- @return The new BR-Block.
   --
   function Block_Factory_Create_Single_Item_File (With_Item_Type: BR_Item_Type := BR_Dictionary; In_Byte_Store: Byte_Store_Ptr) return BR_Block;

   -- Returns information about the file necessary to set up a byte store that can contain the file.
   -- @param Filepath The path to the file to be read.
   --
   function Get_File_Info (Filepath: String) return Block_File_info;

   -- Open an existing byte store as a block.
   -- @param B A pointer to the byte store to be used.
   -- @return Theblock as it was openend.
   -- @exception Invalid_Block_Structure.
   --
   function Block_Factory_Open_Single_Item_File (B: Byte_Store_Ptr) return Boolean; -- BR_Block;

   -- Return the number of items in the block.
   -- @param B The block from which to get the number of item.
   -- @return The number of items in the block.
   --
   function Get_Number_Of_Items (B: in out BR_Block'Class) return Unsigned_32;

   -- Return the N-th item in the block
   -- @param B The block from which to extract the item
   -- @param Index The index of the item to return, starts at 1.
   -- @return The requested item.
   -- @exception Index_Out_Of_Range if the requested item does not exist.
   --
   function Get_Item (B: in out BR_Block'Class; Index: Unsigned_32 := 1) return BR_Static_Unprotected_Item;

   -- Write the block to permanent storage.
   --
   procedure Write_To_File (B: in out BR_Block'Class; Filepath: String);

   -- For testing
   --
   procedure Test_Support_Update_Header_CRC (B: in out BR_Block'Class);

private

   type BR_Block is tagged
      record
         Store: Byte_Store_Ptr;
         Block_Type: BR_Block_Type;
         First_Item_Offset: Unsigned_32;
         Free_Area_Offset: Unsigned_32;
      end record;

   type BR_Static_Unprotected_Item is tagged
      record
         Store: Byte_Store_Ptr;
         Offset: Unsigned_32;
      end record;

   procedure Create_Block_Header (B: in out BR_Block'Class);

   procedure Create_Block_Type_1_Single_Item_File (B: in out BR_Block'Class);

   procedure Update_Free_Area_Offset (B: in out BR_Block'Class; Increment: Unsigned_32);

   procedure Update_Header_Crc (B: in out BR_Block'Class);

end BRBON.Static_Unprotected;

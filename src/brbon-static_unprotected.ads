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


   -- Create a new single-item-file type block.
   -- You will need to provide a byte-store. This can be created by the Byte_Store_Factory in BRBON.Container.
   -- @param With_Item_Type The type of item to create in the block.
   -- @param In_Byte_Store The byte store to use, note that any data in it will be overwritten.
   -- @return The new BR-Block.
   --
   function Block_Factory_Create_Single_Item_File (With_Item_Type: BR_Item_Type := BR_Dictionary; In_Byte_Store: Byte_Store_Ptr) return BR_Block;

   -- Open an existing byte store as a block.
   -- @param B A pointer to the byte store to be used.
   -- @return Theblock as it was openend.
   -- @exception Invalid_Block_Structure.
   --
   function Block_Factory_Open_Single_Item_File (B: Byte_Store_Ptr) return BR_Block;

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

   procedure Create_Block_Header (B: in out BR_Block);

   procedure Create_Block_Type_1_Single_Item_File (B: in out BR_Block);

   procedure Update_Free_Area_Offset (B: in out BR_Block; Increment: Unsigned_32);

end BRBON.Static_Unprotected;

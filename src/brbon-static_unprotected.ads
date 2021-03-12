with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;


package BRBON.Static_Unprotected is

   type BR_Block is tagged private;

   type BR_Static_Unprotected_Item is tagged private;

   function Block_Factory
     (
      Of_Type: BR_Block_Type := Single_Item_File;
      With_Item_Type: BR_Item_Type := BR_Dictionary;
      In_Byte_Store: Byte_Store_Ptr
     )
      return BR_Block;

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
   function Get_Item (B: BR_Block'Class; Index: Unsigned_32 := 1) return BR_Static_Unprotected_Item;


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

end BRBON.Static_Unprotected;

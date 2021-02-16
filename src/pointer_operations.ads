with Interfaces; use Interfaces;
with Ada.Containers.Bounded_Vectors;

with BRBON; use BRBON;
with BRBON_Basic_Types; use BRBON_Basic_Types;

with Pointer_Basics; use Pointer_Basics;


package Pointer_Operations is


   -- Item header access

   procedure Set_Item_Type is new Set_Unsigned_8_At_Ptr_With_Offset (0);
   function Get_Item_Type is new Get_Unsigned_8_From_Ptr_With_Offset (0);

   procedure Set_Item_Options is new Set_Unsigned_8_At_Ptr_With_Offset (1);
   function Get_Item_Options is new Get_Unsigned_8_From_Ptr_With_Offset (1);

   procedure Set_Item_Flags is new Set_Unsigned_8_At_Ptr_With_Offset (2);
   function Get_Item_Flags is new Get_Unsigned_8_From_Ptr_With_Offset (2);

   procedure Set_Item_Name_Field_Byte_Count is new Set_Unsigned_8_At_Ptr_With_Offset (3);
   function Get_Item_Name_Field_Byte_Count is new Get_Unsigned_8_From_Ptr_With_Offset (3);

   procedure Set_Item_Byte_Count is new Set_Unsigned_32_At_Ptr_With_Offset (4);
   function Get_Item_Byte_Count is new Get_Unsigned_32_From_Ptr_With_Offset (4);

   procedure Set_Item_Parent_Offset is new Set_Unsigned_32_At_Ptr_With_Offset (8);
   function Get_Item_Parent_Offset is new Get_Unsigned_32_From_Ptr_With_Offset (8);

   procedure Set_Item_Small_Value is new Set_Unsigned_8_At_Ptr_With_Offset (12);
   function Get_Item_Small_Value is new Get_Unsigned_8_From_Ptr_With_Offset (12);

   procedure Set_Item_Small_Value is new Set_Unsigned_16_At_Ptr_With_Offset (12);
   function Get_Item_Small_Value is new Get_Unsigned_16_From_Ptr_With_Offset (12);

   procedure Set_Item_Small_Value is new Set_Unsigned_32_At_Ptr_With_Offset (12);
   function Get_Item_Small_Value is new Get_Unsigned_32_From_Ptr_With_Offset (12);

   procedure Set_Item_Small_Value is new Set_Float_32_At_Ptr_With_Offset (12);
   function Get_Item_Small_Value is new Get_Float_32_From_Ptr_With_Offset (12);


   -- Item Name Access

   procedure Set_Item_Name_Crc is new Set_Unsigned_16_At_Ptr_With_Offset (16);
   function Get_Item_Name_Crc is new Get_Unsigned_16_From_Ptr_With_Offset (16);

   procedure Set_Item_Name_Byte_Count is new Set_Unsigned_8_At_Ptr_With_Offset (18);
   function Get_Item_Name_Byte_Count is new Get_Unsigned_8_From_Ptr_With_Offset (18);

   function Get_Item_Name_Quick_Check is new Get_Unsigned_32_From_Ptr_With_Offset (16);


   -- Value access support

   procedure Set_Value_Unsigned_16 is new Set_Unsigned_16_At_Ptr_With_Offset (0);
   function Get_Value_Unsigned_16 is new Get_Unsigned_16_From_Ptr_With_Offset (0);

   procedure Set_Value_Unsigned_32 is new Set_Unsigned_32_At_Ptr_With_Offset (0);
   function Get_Value_Unsigned_32 is new Get_Unsigned_32_From_Ptr_With_Offset (0);

   procedure Set_Value_Unsigned_64 is new Set_Unsigned_64_At_Ptr_With_Offset (0);
   function Get_Value_Unsigned_64 is new Get_Unsigned_64_From_Ptr_With_Offset (0);

   procedure Set_Value_Integer_16 is new Set_Integer_16_At_Ptr_With_Offset (0);
   function Get_Value_Integer_16 is new Get_Integer_16_From_Ptr_With_Offset (0);

   procedure Set_Value_Integer_32 is new Set_Integer_32_At_Ptr_With_Offset (0);
   function Get_Value_Integer_32 is new Get_Integer_32_From_Ptr_With_Offset (0);

   procedure Set_Value_Integer_64 is new Set_Integer_64_At_Ptr_With_Offset (0);
   function Get_Value_Integer_64 is new Get_Integer_64_From_Ptr_With_Offset (0);

   procedure Set_Value_Float_32 is new Set_Float_32_At_Ptr_With_Offset (0);
   function Get_Value_Float_32 is new Get_Float_32_From_Ptr_With_Offset (0);

   procedure Set_Value_Float_64 is new Set_Float_64_At_Ptr_With_Offset (0);
   function Get_Value_Float_64 is new Get_Float_64_From_Ptr_With_Offset (0);

end Pointer_Operations;

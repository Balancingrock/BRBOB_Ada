with Ada.Unchecked_Conversion;

with System;
with Interfaces; use Interfaces;

with BRBON.Types;
with BRBON.Container;
with BRBON.Name_Field_Assistent;


package BRBON.Item is


   -- Creates the layout for the requested type in the container at the requested offset.
   --
   procedure Create_Layout
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     Of_Type: Types.Item_Type;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );

   -- Returns the offset of the item value. This is either the small-value or the payload.
   --
   function Get_Value_Offset (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Value_Offset);


   -- Fixed layout

   function Get_Type (C: Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Type;
   pragma Inline (Get_Type);

   function Get_Options (C: Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Options;
   pragma Inline (Get_Options);

   function Get_Flags (C: Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Flags;
   pragma Inline (Get_Flags);

   function Get_Name_Field_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Name_Field_Byte_Count);

   function Get_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Byte_Count);

   function Get_Small_Value (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Small_Value);

   function Get_Parent_Offset (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Parent_Offset);

   procedure Set_Type (C: Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Type);
   pragma Inline (Set_Type);

   procedure Set_Options (C: Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Options);
   pragma Inline (Set_Options);

   procedure Set_Flags (C: Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Flags);
   pragma Inline (Set_Flags);

   procedure Set_Name_Field_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Name_Field_Byte_Count);

   procedure Set_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Byte_Count);

   procedure Set_Small_Value (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Small_Value);

   procedure Set_Parent_Offset (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Parent_Offset);


   -- Name Field access

   procedure Set_Name (C: Container.Instance; Item_Offset: Unsigned_32; Value: Name_Field_Assistent.Instance);

   function Get_Name_Quick_Check_Value (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Name_Quick_Check_Value);

   function Get_Name_CRC (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_Name_CRC);

   function Get_Name_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Name_Byte_Count);

   function Get_Name_String (C: Container.Instance; Item_Offset: Unsigned_32) return String;
   pragma Inline (Get_Name_String);

   procedure Set_Name_CRC (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Name_CRC);

   procedure Set_Name_Byte_Count (C: Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Name_Byte_Count);

   procedure Set_Name_String (C: Container.Instance; Item_Offset: Unsigned_32; Value: String);
   pragma Inline (Set_Name_String);


end BRBON.Item;

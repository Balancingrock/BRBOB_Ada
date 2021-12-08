with Ada.Unchecked_Conversion;

with System;
with Interfaces; use Interfaces;

with BRBON.Types;
with BRBON.Container;


package BRBON.Item is


   type Name_Field_Assistent is private;

   function Create_Name_Field_Assistent (S: String) return Name_Field_Assistent;

   function Get_Quick_Check_Value (NFA: Name_Field_Assistent) return Unsigned_32;

   function Get_Minimum_Item_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_32;


   -- Creates the layout for the requested type in the container at the requested offset.
   --
   procedure Create_Item
    (
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     Of_Type: Types.Item_Type;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );


   function Get_Item_Type (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Type;
   pragma Inline (Get_Item_Type);

   function Get_Item_Options (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Options;
   pragma Inline (Get_Item_Options);

   function Get_Item_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Flags;
   pragma Inline (Get_Item_Flags);

   function Get_Item_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Item_Name_Field_Byte_Count);

   function Get_Item_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Item_Byte_Count);

   function Get_Item_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Item_Small_Value);

   function Get_Item_Parent_Offset (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Item_Parent_Offset);

   function Get_Item_Name_CRC (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_16;
   pragma Inline (Get_Item_Name_CRC);

   function Get_Item_Name_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Item_Name_Byte_Count);

   function Get_Item_Name_String (C: in out Container.Instance; Item_Offset: Unsigned_32) return String;
   pragma Inline (Get_Item_Name_String);

   function Get_Item_Name_Quick_Check_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Item_Name_Quick_Check_Value);

   procedure Set_Item_Type (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Type);
   pragma Inline (Set_Item_Type);

   procedure Set_Item_Options (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Options);
   pragma Inline (Set_Item_Options);

   procedure Set_Item_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Types.Item_Flags);
   pragma Inline (Set_Item_Flags);

   procedure Set_Item_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Item_Name_Field_Byte_Count);

   procedure Set_Item_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Item_Byte_Count);

   procedure Set_Item_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Item_Small_Value);

   procedure Set_Item_Parent_Offset (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Item_Parent_Offset);

   procedure Set_Item_Name (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Name_Field_Assistent);

   procedure Set_Item_Name_CRC (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Item_Name_CRC);

   procedure Set_Item_Name_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Item_Name_Byte_Count);

   procedure Set_Item_Name_String (C: in out Container.Instance; Item_Offset: Unsigned_32; Value: String);
   pragma Inline (Set_Item_Name_String);

private

   Tag_Length: constant := Standard'Address_Size / System.Storage_Unit;

   type Name_Field_Assistent is
      record
         CRC: Unsigned_16;
         Name_Field_Byte_Count: Unsigned_8;
         Ascii_Code: Types.Array_Of_Unsigned_8 (1 .. 245);
         Ascii_Byte_Count: Unsigned_8;
      end record;

   for Name_Field_Assistent'Size use Standard'Address_Size + 16 + 8 + 245*8 + 8;

   for Name_Field_Assistent use
      record
         CRC                   at Tag_Length + 0 range 0..15;
         Name_Field_Byte_Count at Tag_Length + 2 range 0..7;
         Ascii_Code            at Tag_Length + 3 range 0..(245*8-1);
         Ascii_Byte_Count      at Tag_Length + 248 range 0..7;
      end record;

end BRBON.Item;

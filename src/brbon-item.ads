with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

with BRBON.Types;
with BRBON.Container;


package BRBON.Item is


   type Name_Field_Assistent (String_Byte_Count: Unsigned_32) is private;

   function Get_Quick_Check_Value (NFA: in out Name_Field_Assistent) return Unsigned_32;

   function Create_Name_Field_Assistent (S: String) return Name_Field_Assistent;


   -- Returns the byte count of an item if it will have the smallest size payload possible.
   -- For composite/container types this means 1 (smallest possible) child item, element or field.
   -- Note that for composite/container types this will error on the low side, actual byte counts
   -- will usually need to be much higher.
   --
   function Item_Byte_Count_For_Minimum_Length_Payload (T: Types.Item_Type; N: Name_Field_Assistent) return Unsigned_32;


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
   function Get_Item_Options (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Options;
   function Get_Item_Flags (C: in out Container.Instance; Item_Offset: Unsigned_32) return Types.Item_Flags;
   function Get_Item_Name_Field_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   function Get_Item_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   function Get_Item_Small_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   function Get_Item_Parent_Offset (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;
   function Get_Item_Name_CRC (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_16;
   function Get_Item_Name_Byte_Count (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_8;
   function Get_Item_Name_String (C: in out Container.Instance; Item_Offset: Unsigned_32) return String;
   function Get_Item_Name_Quick_Check_Value (C: in out Container.Instance; Item_Offset: Unsigned_32) return Unsigned_32;


private

   type Name_Field_Assistent (String_Byte_Count: Unsigned_32) is tagged
      record
         CRC: Unsigned_16;
         Name_Field_Byte_Count: Unsigned_8;
         Ascii_Code: Types.Array_Of_Unsigned_8 (1 .. String_Byte_Count);
      end record;

end BRBON.Item;

with Ada.Unchecked_Conversion;

with Interfaces; use Interfaces;

with BRBON.Types;
with BRBON.Container;


package BRBON.Item is


   type Name_Field_Assistent (String_Byte_Count: Unsigned_32) is private;

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
     Of_Type: Types.Item_Type;
     In_Container: in out Container.Instance;
     At_Offset: Unsigned_32;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    );


private

   type Name_Field_Assistent (String_Byte_Count: Unsigned_32) is tagged
      record
         CRC: Unsigned_16;
         Name_Field_Byte_Count: Unsigned_8;
         Ascii_Code: Types.Array_Of_Unsigned_8 (1 .. String_Byte_Count);
      end record;

end BRBON.Item;

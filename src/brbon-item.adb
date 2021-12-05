with Ada.Exceptions;


package body BRBON.Item is

   -- Item offsets
   --
   Type_Offset: constant Unsigned_32 := 0;
   Options_Offset: constant Unsigned_32 := 1;
   Flags_Offset: constant Unsigned_32 := 2;
   Name_Field_Byte_Count_Offset: constant Unsigned_32 := 3;
   Byte_Count_Offset: constant Unsigned_32 := 4;
   Parent_Offset_Offset: constant Unsigned_32 := 8;
   Small_Value_Offset: constant Unsigned_32 := 12;
   Payload_Offset: constant Unsigned_32 := 16;



   -- Internal specifications

   procedure Create_Null_Type (C: Container.Instance; L: Unsigned_32);


   -- API

   procedure Create_Item (Of_Type: Types.Item_Type; In_Container: Container.Instance; At_Location: Unsigned_32) is

      T: Item_Type renames Of_Type;
      C: Container.Instance renames In_Container;
      L: Unsigned_32 renames At_Location;

   begin

      case T is
         when Illegal =>
            Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "");
         when Null_Type =>
            Create_Null_Type (C, L);
         when others =>
            Ada.Exceptions.Raise_Exception (Implementation'Identity, "Not implemented yet");
      end case;

   end Create_Item;


   -- Internal bodies

   procedure Create_Null_Type (C: Container.Instance; L: Unsigned_32) is
   begin
      C.Set_Item_Type (L, To_Unsigned_8 (Null_Type));

   end Create_Null_Type;

end BRBON.Item;

with Ada.Exceptions;


package body BRBON.Item is

   -- Item offsets
   --
   Type_Offset: constant Unsigned_32 := 0;                          -- 1 byte
   Options_Offset: constant Unsigned_32 := 1;                       -- 1 byte
   Flags_Offset: constant Unsigned_32 := 2;                         -- 1 byte
   Name_Field_Byte_Count_Offset: constant Unsigned_32 := 3;         -- 1 byte
   Byte_Count_Offset: constant Unsigned_32 := 4;                    -- 4 bytes
   Parent_Offset_Offset: constant Unsigned_32 := 8;                 -- 4 bytes
   Small_Value_Offset: constant Unsigned_32 := 12;                  -- 4 bytes
   --
   Name_Field_CRC_Offset: constant Unsigned_32 := 16;
   Name_Field_ASCII_Byte_Count_Offset: constant Unsigned_32 := 18;
   Name_Field_ASCII_Code_Offset: constant Unsigned_32 := 19;        -- Up to 248 bytes


   -- Internal specifications

   procedure Create_Null_Type
    (
     C: in out Container.Instance;
     L: Unsigned_32;
     A: Name_Field_Assistent;
     B: Unsigned_32;
     P: Unsigned_32
    );


   -- API

   procedure Create_Item
    (
     Of_Type: Types.Item_Type;
     In_Container: in out Container.Instance;
     At_Location: Unsigned_32,
     With_Name: String := 0,
     Using_Byte_Count: Unsigned_32 := 0,
     Parent_Offset: Unsigned_32 := 0
    ) is

      T: Item_Type renames Of_Type;
      C: Container.Instance renames In_Container;
      L: Unsigned_32 renames At_Location;
      
      NFA: Name_Field_Assistent := Create_Name_Field_Assistent (With_Name);

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

   procedure Create_Null_Type
    (
     C: in out Container.Instance;
     L: Unsigned_32;
     A: Name_Field_Assistent;
     B: Unsigned_32;
     P: Unsigned_32
    ) is
    
   begin
      C.Set_Item_Type (L + Type_Offset, To_Unsigned_8 (Null_Type));
      C.Set_Item_Options (L + Options_Offset, To_Unsigned_8 (No_Item_Options));
      C.Set_Item_Flags (L + Flags_Offset, To_Unsigned_8 (No_Item_Flags));
      C.Set_Item_Name_Field_Byte_Count (L + Name_Field_Byte_Count_Offset, A.Byte_Count);
      C.Set_Unsigned_32 (L + Byte_Count_Offset, B);
      C.Set_Unsigned_32 (L + Parent_Offset_Offset, P);
      C.Set_Unsigned_32 (L + Small_Value_Offset, 0);
   end Create_Null_Type;

end BRBON.Item;

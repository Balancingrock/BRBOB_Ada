package body BRBON.Item is


   function Get_Type (I: in out Instance'Class) return Item_Type is
   begin
      return Null_Type;
   end Get_Type;

   procedure Set_Type (I: in out Instance'Class; T: Item_Type) is
   begin
      null;
   end Set_Type;


   function Get_Options (I: in out Instance'Class) return Item_Options is
   begin
      return Item_Options (0);
   end Get_Options;

   procedure Set_Options (I: in out Instance'Class; O: Item_Options) is
   begin
      null;
   end Set_Options;


   function Get_Flags (I: in out Instance'Class) return Item_Flags is
   begin
      return Item_Flags (0);
   end Get_Flags;

   procedure Set_Flags (I: in out Instance'Class; F: Item_Flags) is
   begin
      null;
   end Set_Flags;


   function Get_Name_Field_Byte_Count (I: in out Instance'Class) return Unsigned_8 is
   begin
      return 0;
   end Get_Name_Field_Byte_Count;

   procedure Set_Name_Field_Byte_Count (I: in out Instance'Class; Byte_Count: Unsigned_8) is
   begin
      null;
   end Set_Name_Field_Byte_Count;


   function Get_Byte_Count (I: in out Instance'Class) return Unsigned_32 is
   begin
      return 0;
   end Get_Byte_Count;

   procedure Set_Byte_Count (I: in out Instance'Class; Byte_Count: Unsigned_32) is
   begin
      null;
   end Set_Byte_Count;


   function Get_Parent_Offset (I: in out Instance'Class) return Unsigned_32 is
   begin
      return 0;
   end Get_Parent_Offset;

   procedure Set_Parent_Offset (I: in out Instance'Class; Offset: Unsigned_32) is
   begin
      null;
   end Set_Parent_Offset;


   function Get_Small_Value (I: in out Instance'Class) return Unsigned_32 is
   begin
      return 0;
   end Get_Small_Value;

   procedure Set_Small_Value (I: in out Instance'Class; Small_Value: Unsigned_32) is
   begin
      null;
   end Set_Small_Value;

end BRBON.Item;

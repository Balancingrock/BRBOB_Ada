with Interfaces.C.Pointers;

with BRBON;


package body Item is

   package Unsigned_8_Ptr_Math is new Interfaces.C.Pointers (Index              => Unsigned_32,
                                                             Element            => Unsigned_8,
                                                             Element_Array      => Array_Of_Unsigned_8,
                                                             Default_Terminator => 0);
   use Unsigned_8_Ptr_Math;

   procedure Item_Type (Ptr: Unsigned_8_Ptr; Value: BR_Item_Type) is
      IPtr: Item_Header_Ptr := To_Item_Header_Ptr (Ptr);
   begin
      IPtr.Item_Type := Value;
   end Item_Type;


   function Item_Type (Ptr: Unsigned_8_Ptr) return BR_Item_Type is
      Raw_Type: Unsigned_8 := Ptr.all;
   begin
      if Raw_Type > Unsigned_8 (BR_Item_Type'Pos (BR_Item_Type'Last_Valid)) then raise BRBON.Enum_Mapping_Failed; end if;
      return BR_Item_Type'Val (Raw_Type);
   end Item_Type;


   function Is_Valid_Item_Type (Ptr: Unsigned_8_Ptr) return Boolean is
      Raw_Type: Unsigned_8 := Ptr.all;
   begin
      return Raw_Type <= Unsigned_8 (BR_Item_Type'Pos (BR_Item_Type'Last_Valid));
   end Is_Valid_Item_Type;


   function Get_Value_Area_Ptr (Ptr: Item_Header_Ptr) return Unsigned_8_Ptr is
   begin
      return new Unsigned_8;
   end Get_Value_Area_Ptr;


   -- ==========================
   -- String Value
   -- ==========================

   procedure Set_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_String_UTF_8_Code;

   procedure Get_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_String_UTF_8_Code;


   -- ===========================
   -- CRC String Value
   -- ===========================

   procedure Set_CRC_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_CRC_String_UTF_8_Code;

   procedure Get_CRC_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_CRC_String_UTF_8_Code;


   -- ===========================
   -- Binary Value
   -- ===========================

   procedure Set_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_Binary_Data;

   procedure Get_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_Binary_Data;


   -- ===========================
   -- CRC Binary Value
   -- ===========================

   procedure Set_CRC_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Set_CRC_Binary_Data;

   procedure Get_CRC_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Array_Of_Unsigned_8) is
   begin
      raise BRBON.Incomplete_Code;
   end Get_CRC_Binary_Data;


   -- ============================
   -- Array Value
   -- ============================



end Item;

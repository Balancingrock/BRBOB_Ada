with Ada.Unchecked_Conversion;


package body Storage_Area is


   function To_Array_Of_Unsigned_8_Ptr is new Ada.Unchecked_Conversion (String_Ptr, Array_Of_Unsigned_8_Ptr);


   function Allocate_And_Create (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr is
      S: Storage_Area_Ptr;
   begin
      S := new Storage_Area;
      S.all.Uses_Endianness := Using_Endianness;
      S.all.Data := new Array_Of_Unsigned_8 (1 .. Byte_Count);
      S.all.Swap := Using_Endianness = Machine_Endianness;
      return S;
   end Allocate_And_Create;


   procedure Finalization (S: in out Storage_Area) is
   begin
      Deallocate_Array_Of_Unsigned_8 (S.Data);
   end Finalization;


   -- Operational

   procedure Set_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32; Value: BR_Item_Type) is
   begin
      S.Data.all (Offset) := BR_Item_Type'Pos (Value);
   end Set_Item_Type;

   function Valid_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32) return Boolean is
   begin
      return S.Data.all (Offset) > Unsigned_8 (BR_Item_Type'Pos (BR_Item_Type'Last));
   end Valid_Item_Type;

   function Get_Item_Type (S: Storage_Area'Class; Offset: Unsigned_32) return BR_Item_Type is
   begin
      return BR_Item_Type'Val (S.Data.all (Offset));
   end Get_Item_Type;


   procedure Set_Bool (S: Storage_Area'Class; Offset: Unsigned_32; Value: Boolean) is
   begin
      S.Data (Offset) := (if Value then 1 else 0);
   end Set_Bool;


   procedure Set_Unsigned_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      S.Data.all (Offset) := Value;
   end Set_Unsigned_8;


   procedure Set_Unsigned_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      end if;
   end Set_Unsigned_16;


   procedure Set_Unsigned_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      end if;
   end Set_Unsigned_32;


   procedure Set_Unsigned_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      end if;
   end Set_Unsigned_64;


   procedure Set_Integer_8 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_8) is
   begin
      S.Data.all (Offset) := To_Unsigned_8 (Value);
   end Set_Integer_8;


   procedure Set_Integer_16 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_16) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
      end if;
   end Set_Integer_16;


   procedure Set_Integer_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_32) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));
      end if;
   end Set_Integer_32;


   procedure Set_Integer_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: Integer_64) is
   begin
      if S.Swap then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));
      end if;
   end Set_Integer_64;


   procedure Set_Float_32 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));
      end if;
   end Set_Float_32;


   procedure Set_Float_64 (S: Storage_Area'Class; Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));
      end if;
   end Set_Float_64;


   procedure Set_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Value: Array_Of_Unsigned_8) is
   begin
      S.Data.all (Offset .. Offset + Value'Last) := Value;
   end Set_Unsigned_8_Array;


   procedure Get_Unsigned_8_Array (S: Storage_Area'Class; Offset: Unsigned_32; Value: in out Array_Of_Unsigned_8) is
   begin
      Value := S.Data.all (Offset .. Offset + Value'Last);
   end Get_Unsigned_8_Array;


   procedure Set_String (S: Storage_Area'Class; Offset: Unsigned_32; Value: String) is
      subtype Target_Array is Array_Of_Unsigned_8 (1 .. Value'Length);
      type Target_Array_Ptr is access Target_Array;
      function To_Target_Array_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Target_Array_Ptr);
      subtype String_Helper is String (1 .. Value'Length);
      function To_Target_Array is new Ada.Unchecked_Conversion (String_Helper, Target_Array);
      Target: Target_Array_Ptr := To_Target_Array_Ptr (S.Data.all (Offset)'Access);
   begin
      Target.all := To_Target_Array (Value);
   end Set_String;


   procedure Get_String (S: Storage_Area'Class; Offset: Unsigned_32; Value: out String) is
      subtype Source_Array is Array_Of_Unsigned_8 (1 .. Value'Length);
      type Source_Array_Ptr is access Source_Array;
      function To_Source_Array_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Source_Array_Ptr);
      subtype String_Helper is String (1 .. Value'Length);
      function To_String_Helper is new Ada.Unchecked_Conversion (Source_Array, String_Helper);
   begin
      Value := To_String_Helper (To_Source_Array_Ptr (S.Data.all (Offset)'Access).all);
   end Get_String;

end Storage_Area;

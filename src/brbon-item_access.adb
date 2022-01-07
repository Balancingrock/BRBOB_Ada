with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNAT.Byte_Swapping;

with CRC_Package; use CRC_Package;

with BRBON.Utils;
with BRBON.Container;


package body BRBON.Item_Access is
   
   
   function Swap_Unsigned_16 is new GNAT.Byte_Swapping.Swapped2 (Unsigned_16);
   function Swap_Unsigned_32 is new GNAT.Byte_Swapping.Swapped4 (Unsigned_32);
   function Swap_Unsigned_64 is new GNAT.Byte_Swapping.Swapped8 (Unsigned_64);
   function Swap_Integer_16 is new GNAT.Byte_Swapping.Swapped2 (Integer_16);
   function Swap_Integer_32 is new GNAT.Byte_Swapping.Swapped4 (Integer_32);
   function Swap_Integer_64 is new GNAT.Byte_Swapping.Swapped8 (Integer_64);
   function Swap_Float_32 is new GNAT.Byte_Swapping.Swapped4 (IEEE_Float_32);
   function Swap_Float_64 is new GNAT.Byte_Swapping.Swapped8 (IEEE_Float_64);


   Item_Overhead_Byte_Count: Array (Item_Type) of Unsigned_32 :=
     (
      0,  -- Illegal
      0,  -- Null
      0,  -- Bool
      0,  -- Int_8
      0,  -- Int_16
      0,  -- Int_32
      8,  -- Int_64
      0,  -- UInt_8
      0,  -- UInt_16
      0,  -- UInt_32
      8,  -- UInt_64
      0,  -- Float_32
      8,  -- Float_64
      4,  -- String
      8,  -- CRC_String
      4,  -- Binary
      8,  -- CRC_Binary
      32, -- Array
      8,  -- Dictionary
      8,  -- Sequence
      16, -- Table (no column descriptors included)
      16, -- UUID
      0,  -- RGBA
      6   -- Font
     );

   -----------------------------------------------------------------------------
   
   --Item_Header_Byte_Count: constant Unsigned_32 := 16;

   function Get_Item_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Header_Ptr is (To_Item_Header_Ptr (S.Data (Item_Offset)'Access));
   pragma Inline (Get_Item_Header_Ptr);


   -----------------------------------------------------------------------------
   
   function Get_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Type_Field;
   end Get_Type;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Type_Field := Value;
   end Set_Type;
   
   
   -----------------------------------------------------------------------------

   function Get_Options (S: Store; Item_Offset: Unsigned_32) return Item_Options is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Options_Field;
   end Get_Options;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Options (S: Store; Item_Offset: Unsigned_32; Value: Item_Options) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Options_Field := Value;
   end Set_Options;
   
   
   -----------------------------------------------------------------------------

   function Get_Flags (S: Store; Item_Offset: Unsigned_32) return Item_Flags is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Flags_Field;
   end Get_Flags;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Flags (S: Store; Item_Offset: Unsigned_32; Value: Item_Flags) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Flags_Field := Value;
   end Set_Flags;
   
   
   -----------------------------------------------------------------------------

   function Get_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      return IPtr.Name_Field_Byte_Count_Field;
   end Get_Name_Field_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Name_Field_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      IPtr.Name_Field_Byte_Count_Field := Value;
   end Set_Name_Field_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (IPtr.Byte_Count_Field);
      else
         return IPtr.Byte_Count_Field;
      end if;
   end Get_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         IPtr.Byte_Count_Field := Swap_Unsigned_32 (Value);
      else
         IPtr.Byte_Count_Field := Value;
      end if;
   end Set_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   function Get_Parent_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (IPtr.Parent_Offset_Field);
      else
         return IPtr.Parent_Offset_Field;
      end if;
   end Get_Parent_Offset;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Parent_Offset (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      IPtr: Item_Header_Ptr := Get_Item_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         IPtr.Parent_Offset_Field := Swap_Unsigned_32 (Value);
      else
         IPtr.Parent_Offset_Field := Value;
      end if;
   end Set_Parent_Offset;
   

   -----------------------------------------------------------------------------

   function Get_Bool (S: Store; Item_Offset: Unsigned_32) return Boolean is
   begin
      return Container.Get_Bool (S, Item_Offset + Small_Value_Offset);
   end Get_Bool;
   

   -----------------------------------------------------------------------------

   procedure Set_Bool (S: Store; Item_Offset: Unsigned_32; Value: Boolean) is
   begin
      Container.Set_Bool (S, Item_Offset + Small_Value_Offset, Value);
   end Set_Bool;

   
   -----------------------------------------------------------------------------
   
   function Get_Int8 (S: Store; Item_Offset: Unsigned_32) return Integer_8 is
   begin
      return Container.Get_Integer_8 (S, Item_Offset + Small_Value_Offset);
   end Get_Int8;

   
   -----------------------------------------------------------------------------
   
   procedure Set_Int8 (S: Store; Item_Offset: Unsigned_32; Value: Integer_8) is
   begin
      Container.Set_Integer_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_Int8;

   
   -----------------------------------------------------------------------------
   
   function Get_UInt8 (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset);
   end Get_UInt8;
   
   
   -----------------------------------------------------------------------------

   procedure Set_UInt8 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_UInt8;
   
   
   -----------------------------------------------------------------------------

   function Get_Int16 (S: Store; Item_Offset: Unsigned_32) return Integer_16 is
   begin
      return Container.Get_Integer_16 (S, Item_Offset + Small_Value_Offset);
   end Get_Int16;

   
   -----------------------------------------------------------------------------
   
   procedure Set_Int16 (S: Store; Item_Offset: Unsigned_32; Value: Integer_16) is
   begin
      Container.Set_Integer_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_Int16;
   
   
   -----------------------------------------------------------------------------

   function Get_UInt16 (S: Store; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (S, Item_Offset + Small_Value_Offset);
   end Get_UInt16;

   
   -----------------------------------------------------------------------------
   
   procedure Set_UInt16 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_UInt16;

   
   -----------------------------------------------------------------------------
   
   function Get_Int32 (S: Store; Item_Offset: Unsigned_32) return Integer_32 is
   begin
      return Container.Get_Integer_32 (S, Item_Offset + Small_Value_Offset);
   end Get_Int32;
   
   
   -----------------------------------------------------------------------------

   procedure Set_Int32 (S: Store; Item_Offset: Unsigned_32; Value: Integer_32) is
   begin
      Container.Set_Integer_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_Int32;

   
   -----------------------------------------------------------------------------
   
   function Get_UInt32 (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (S, Item_Offset + Small_Value_Offset);
   end Get_UInt32;
   
   
   -----------------------------------------------------------------------------

   procedure Set_UInt32 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_UInt32;

   
   -----------------------------------------------------------------------------
   
   function Get_Float32 (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32 is
   begin
      return Container.Get_Float_32 (S, Item_Offset + Small_Value_Offset);
   end Get_Float32;

   
   -----------------------------------------------------------------------------
   
   procedure Set_Float32 (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      Container.Set_Float_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Set_Float32;

   
   -----------------------------------------------------------------------------
   
--   type Item_Value_Header_RGBA is
--      record
--         Red: Unsigned_8;
--         Green: Unsigned_8;
--         Blue: Unsigned_8;
--         Alpha: Unsigned_8;
--      end record;

--   for Item_Value_Header_RGBA'Size use 32;

--   for Item_Value_Header_RGBA use
--      record
--         Red   at 0 range 0..7;
--         Green at 1 range 0..7;
--         Blue  at 2 range 0..7;
--         Alpha at 3 range 0..7;
--      end record;

--   type Item_Value_Header_RGBA_Ptr is access Item_Value_Header_RGBA;

--   function To_Item_Value_Header_RGBA_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Header_RGBA_Ptr);
   
   -----------------------------------------------------------------------------
   
   function Get_Color (S: Store; Item_Offset: Unsigned_32) return Color is
   begin
      return Color_Factory (Red   => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset),
                            Green => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1),
                            Blue  => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2),
                            Alpha => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3));
   end Get_Color;

   
   -----------------------------------------------------------------------------
   
   procedure Set_Color (S: Store; Item_Offset: Unsigned_32; Value: Color) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Get_Red_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1, Get_Green_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2, Get_Blue_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3, Get_Alpha_Component (Value));
   end Set_Color;

   
   -----------------------------------------------------------------------------
   -- Item name
   -----------------------------------------------------------------------------
   
   
   Item_Name_Offset: constant Unsigned_32 := Item_Header_Byte_count;
   Item_Name_Ascii_Code_Offset: constant Unsigned_32 := Item_Name_Offset + 3;

   type Item_Name_Field is
      record
         CRC: Unsigned_16;
         Byte_Count: Unsigned_8;
         ASCII_Code: aliased Unsigned_8; -- Followed by up to 244 characters
      end record;

   for Item_Name_Field'Size use 16 + 8 + 8;

   for Item_Name_Field use
      record
         CRC        at 0 range 0..15;
         Byte_Count at 2 range 0..7;
         ASCII_Code at 3 range 0..7;
      end record;

   type Item_Name_Field_Ptr is access Item_Name_Field;

   function To_Item_Name_Field_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Name_Field_Ptr);

   function Get_Item_Name_Field_Offset (Item_Offset: Unsigned_32) return Unsigned_32 is ( Item_Offset + Item_Name_Offset );
   pragma Inline (Get_Item_Name_Field_Offset);

   function Get_Item_Name_Field_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Name_Field_Ptr is (To_Item_Name_Field_Ptr (S.Data (Get_Item_Name_Field_Offset (Item_Offset))'Access));
   pragma Inline (Get_Item_Name_Field_Ptr);
   
   function Item_Name_Get_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Get_Item_Name_Field_Ptr (S, Item_Offset).Byte_Count;
   end Item_Name_Get_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   function Get_Name_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_16 is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return CRC_16 (Swap_Unsigned_16 (NPtr.CRC));
      else
         return CRC_16 (NPtr.CRC);
      end if;
   end Get_Name_CRC;
   

   -----------------------------------------------------------------------------
   
   procedure Set_Name_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_16) is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         NPtr.CRC := CRC_16 (Swap_Unsigned_16 (Unsigned_16 (Value)));
      else
         NPtr.CRC := Value;
      end if;
   end Set_Name_CRC;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Name_Set_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      NPtr.Byte_Count := Value;
   end Item_Name_Set_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Name_Get_Byte_Code (S: Store; Item_Offset: Unsigned_32) return String is
      Byte_Count: Unsigned_8 := Item_Name_Get_Byte_Count (S, Item_Offset);
      Offset: Unsigned_32 := Item_Offset + Item_Name_Ascii_Code_Offset;
   begin
      return Container.Get_String (S, Offset, Unsigned_32 (Byte_count));
   end Item_Name_Get_Byte_Code;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Name_Set_Byte_Code (S: Store; Item_Offset: Unsigned_32; Value: Item_Name) is
      Offset: Unsigned_32 := Item_Offset + Item_Name_Ascii_Code_Offset;
   begin
      Container.Set_String (S, Offset, Item_Name_Bounded_String_Package.To_String (Value));
   end Item_Name_Set_Byte_Code;

   
   -----------------------------------------------------------------------------
   
   function Get_Name_Quick_Check_Value (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (S, Item_Offset + Item_Header_Byte_Count);
   end Get_Name_Quick_Check_Value;
   
   
   -----------------------------------------------------------------------------
   
   function Name_Equals (S: Store; Item_Offset: Unsigned_32; Name_Assistent: Name_Field_Assistent) return Boolean is
   
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
      function To_Quick_Check_Value_Ptr is new Ada.Unchecked_Conversion (Item_Name_Field_Ptr, Quick_Check_Value_Ptr);
   
   begin
      
      if S.Swap /= Swap_Status (Name_Assistent) then
         Ada.Exceptions.Raise_Exception (Incompatible'Identity, "The Name Field Assistent is not compatible with this store");
      end if;
      
      if not Compare_Quick_Check (Name_Assistent, To_Quick_Check_Value_Ptr (NPtr)) then
         return false;
      end if;
      
      return Compare_String (Name_Assistent, NPtr.ASCII_Code'Access, NPtr.Byte_Count);

   end Name_Equals;
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Name (S: Store; Item_Offset: Unsigned_32; Value: Name_Field_Assistent) is
      NPtr: Item_Name_Field_Ptr := Get_Item_Name_Field_Ptr (S, Item_Offset);
   begin
      if Value.Field_Byte_Count > 0 then
         Set_Name_CRC (S, Item_Offset, Value.CRC);
         Item_Name_Set_Byte_Count (S, Item_Offset, Value.Name_Byte_Count);
         Item_Name_Set_Byte_Code (S, Item_Offset, Value.Name);
         Set_Name_Field_Byte_Count (S, Item_Offset, Value.Field_Byte_Count);
      end if;
   end Set_Name;
   
   
   -----------------------------------------------------------------------------
   -- Item Value: String
   -----------------------------------------------------------------------------

   
   type Item_Value_String_Header is
      record
         Count: Unsigned_32;
      end record;

   for Item_Value_String_Header'Size use 32;

   for Item_Value_String_Header use
      record
         Count     at 0 range 0..31;
      end record;

   Item_Value_String_Header_Byte_Count: constant Unsigned_32 := 4;

   type Item_Value_String_Header_Ptr is access Item_Value_String_Header;

   function To_Item_Value_String_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_String_Header_Ptr);

   function Get_Item_Value_String_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_String_Header_Ptr is (To_Item_Value_String_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      VPtr: Item_Value_String_Header_Ptr := Get_Item_Value_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (VPtr.Count);
      else
         return VPtr.Count;
      end if;
   end Item_Value_String_Get_Count;

   
   -----------------------------------------------------------------------------
   
   procedure Item_Value_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      VPtr: Item_Value_String_Header_Ptr := Get_Item_Value_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         VPtr.Count := Swap_Unsigned_32 (Value);
      else
         VPtr.Count := Value;
      end if;
   end Item_Value_String_Set_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String is
      Byte_Count: Unsigned_32 := Item_Value_String_Get_Count (S, Item_Offset);
   begin
      return Container.Get_String (S, Item_Offset + Item_Value_String_Header_Byte_Count, Byte_Count);
   end Item_Value_String_Get_ASCII_Code;
   

   -----------------------------------------------------------------------------
   
   procedure Item_Value_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String) is
      Byte_Count: Unsigned_32 := Item_Value_String_Get_Count (S, Item_Offset);
   begin
      Container.Set_String (S, Item_Offset + Item_Value_String_Header_Byte_Count, Value);
   end Item_Value_String_Set_ASCII_Code;


   -----------------------------------------------------------------------------
   -- Item Value: CRC String
   -----------------------------------------------------------------------------

   
   type Item_Value_CRC_String_Header is
      record
         CRC: CRC_32;
         Count: Unsigned_32;
      end record;

   for Item_Value_CRC_String_Header'Size use 32 + 32;

   for Item_Value_CRC_String_Header use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
      end record;

   Item_Value_CRC_String_Header_Byte_count: constant Unsigned_32 := Unsigned_32 (Item_Value_CRC_String_Header'Size / 8);

   type Item_Value_CRC_String_Header_Ptr is access Item_Value_CRC_String_Header;

   function To_Item_Value_CRC_String_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_CRC_String_Header_Ptr);

   function Get_Item_Value_CRC_String_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_CRC_String_Header_Ptr is (To_Item_Value_CRC_String_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_String_Get_CRC (S: Store; Item_Offset: Unsigned_32) return CRC_32 is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return CRC_32 (Swap_Unsigned_32 (Unsigned_32 (VPtr.CRC)));
      else
         return Vptr.CRC;
      end if;
   end Item_Value_CRC_String_Get_CRC;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_String_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: CRC_32) is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         VPtr.CRC := CRC_32 (Swap_Unsigned_32 (Unsigned_32 (Value)));
      else
         VPtr.CRC := Value;
      end if;
   end Item_Value_CRC_String_Set_CRC;

   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_String_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (VPtr.Count);
      else
         return Vptr.Count;
      end if;
   end Item_Value_CRC_String_Get_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_String_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      VPtr: Item_Value_CRC_String_Header_Ptr := Get_Item_Value_CRC_String_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         VPtr.CRC := Swap_Unsigned_32 (Value);
      else
         VPtr.CRC := Value;
      end if;
   end Item_Value_CRC_String_Set_Count;

   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_String_Get_ASCII_Code (S: Store; Item_Offset: Unsigned_32) return String is
      Byte_Count: Unsigned_32 := Item_Value_CRC_String_Get_Count (S, Item_Offset);
      Offset: Unsigned_32 := Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_CRC_String_Header_Byte_count;
   begin
      return Container.Get_String (S, Offset, Byte_Count);
   end Item_Value_CRC_String_Get_ASCII_Code;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_String_Set_ASCII_Code (S: Store; Item_Offset: Unsigned_32; Value: String) is
      Offset: Unsigned_32 := Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_CRC_String_Header_Byte_count;
   begin
      Container.Set_String (S, Offset, Value);
   end Item_Value_CRC_String_Set_ASCII_Code;


   -----------------------------------------------------------------------------
   -- Item Value: Binary
   -----------------------------------------------------------------------------
   
   
   type Item_Value_Binary_Header is
      record
         Count: Unsigned_32;
      end record;

   for Item_Value_Binary_Header'Size use 32;

   for Item_Value_Binary_Header use
      record
         Count at 0 range 0..31;
      end record;

   Item_Value_Binary_Header_Byte_count: constant Unsigned_32 := Unsigned_32 (Item_Value_Binary_Header'Size / 8);

   type Item_Value_Binary_Header_Ptr is access Item_Value_Binary_Header;

   function To_Item_Value_Binary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Binary_Header_Ptr);

   function Get_Item_Value_Binary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Binary_Header_Ptr is (To_Item_Value_Binary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      BPtr: Item_Value_Binary_Header_Ptr := Get_Item_Value_Binary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (BPtr.Count);
      else
         return BPtr.Count;
      end if;
   end Item_Value_Binary_Get_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      BPtr: Item_Value_Binary_Header_Ptr := Get_Item_Value_Binary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         BPtr.Count := Swap_Unsigned_32 (Value);
      else
         BPtr.Count := Value;
      end if;
   end Item_Value_Binary_Set_Count;

   
   -----------------------------------------------------------------------------
   
   function Item_Value_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array is
      Byte_Count: Unsigned_32 := Item_Value_Binary_Get_Count (S, Item_Offset);
      Offset: Unsigned_32 := Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Binary_Header_Byte_count;
   begin
      return Container.Get_Unsigned_8_Array (S, Offset, Byte_Count);
   end Item_Value_Binary_Get_Bytes;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Value_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array) is
      Offset: Unsigned_32 := Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Binary_Header_Byte_count;
   begin
      Container.Set_Unsigned_8_Array (S, Offset, Value);
   end Item_Value_Binary_Set_Bytes;


   -----------------------------------------------------------------------------
   -- Item Value: CRC Binary
   -----------------------------------------------------------------------------
   

   type Item_Value_CRC_Binary_Header is
      record
         CRC: CRC_32;
         Count: Unsigned_32;
      end record;

   for Item_Value_CRC_Binary_Header'Size use 32 + 32;

   for Item_Value_CRC_Binary_Header use
      record
         CRC at 0 range 0..31;
         Count at 4 range 0..31;
      end record;

   Item_Value_CRC_Binary_Header_Byte_count: constant Unsigned_32 := Unsigned_32 (Item_Value_CRC_Binary_Header'Size / 8);

   type Item_Value_CRC_Binary_Header_Ptr is access Item_Value_CRC_Binary_Header;

   function To_Item_Value_CRC_Binary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_CRC_Binary_Header_Ptr);

   function Get_Item_Value_CRC_Binary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_CRC_Binary_Header_Ptr is (To_Item_Value_CRC_Binary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_Binary_Get_CRC (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      BPtr: Item_Value_CRC_Binary_Header_Ptr := Get_Item_Value_CRC_Binary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (BPtr.CRC);
      else
         return BPtr.CRC;
      end if;
   end Item_Value_CRC_Binary_Get_CRC;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_Binary_Set_CRC (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      BPtr: Item_Value_CRC_Binary_Header_Ptr := Get_Item_Value_CRC_Binary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         BPtr.CRC := Swap_Unsigned_32 (Value);
      else
         BPtr.CRC := Value;
      end if;
   end Item_Value_CRC_Binary_Set_CRC;

   
   -----------------------------------------------------------------------------
   
   function Item_Value_CRC_Binary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      BPtr: Item_Value_CRC_Binary_Header_Ptr := Get_Item_Value_CRC_Binary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (BPtr.Count);
      else
         return BPtr.Count;
      end if;
   end Item_Value_CRC_Binary_Get_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_Binary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      BPtr: Item_Value_CRC_Binary_Header_Ptr := Get_Item_Value_CRC_Binary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         BPtr.Count := Swap_Unsigned_32 (Value);
      else
         BPtr.Count := Value;
      end if;
   end Item_Value_CRC_Binary_Set_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_CRC_Binary_Get_Bytes (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Array is
      Byte_Count: Unsigned_32 := Item_Value_CRC_Binary_Get_Count (S, Item_Offset);
      Offset: Unsigned_32 := Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_CRC_Binary_Header_Byte_count;
   begin
      return Container.Get_Unsigned_8_Array (S, Offset, Byte_Count);
   end Item_Value_CRC_Binary_Get_Bytes;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_CRC_Binary_Set_Bytes (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8_Array) is
      Offset: Unsigned_32 := Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_CRC_Binary_Header_Byte_count;
   begin
      Container.Set_Unsigned_8_Array (S, Offset, Value);
   end Item_Value_CRC_Binary_Set_Bytes;
   


   -----------------------------------------------------------------------------
   -- Item Value: Array
   -----------------------------------------------------------------------------
   
   
   type Item_Value_Array_Header is
      record
         Reserved_1: Unsigned_32;
         Element_Type: Item_Type;
         Reserved_2: Unsigned_8;
         Reserved_3: Unsigned_16;
         Element_Count: Unsigned_32;
         Element_Byte_Count: Unsigned_32;
      end record;

   for Item_Value_Array_Header'Size use 32 + 8 + 8 + 16 + 32 + 32 + 8;

   for Item_Value_Array_Header use
      record
         Reserved_1         at 0  range 0..31;
         Element_Type       at 4  range 0..7;
         Reserved_2         at 5  range 0..7;
         Reserved_3         at 6  range 0..15;
         Element_Count      at 8  range 0..31;
         Element_Byte_Count at 12 range 0..31;
      end record;

   Item_Value_Array_Header_Byte_Count: constant Unsigned_32 := Item_Value_Array_Header'Size / 8;

   type Item_Value_Array_Header_Ptr is access Item_Value_Array_Header;

   function To_Item_Value_Array_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Array_Header_Ptr);

   function Get_Item_Value_Array_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Array_Header_Ptr is (To_Item_Value_Array_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

 
   -----------------------------------------------------------------------------
   
   function Item_Value_Array_Get_Element_Type (S: Store; Item_Offset: Unsigned_32) return Item_Type is
      APtr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, Item_Offset);
   begin
      return APtr.Element_Type;
   end Item_Value_Array_Get_Element_Type;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Array_Set_Element_Type (S: Store; Item_Offset: Unsigned_32; Value: Item_Type) is
      APtr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, Item_Offset);
   begin
      APtr.Element_Type := Value;
   end Item_Value_Array_Set_Element_Type;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Array_Get_Element_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      APtr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (APtr.Element_Count);
      else
         return APtr.Element_Count;
      end if;
   end Item_Value_Array_Get_Element_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Array_Set_Element_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      APtr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         APtr.Element_Count := Swap_Unsigned_32 (Value);
      else
         APtr.Element_Count := Value;
      end if;
   end Item_Value_Array_Set_Element_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Array_Get_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      APtr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (APtr.Element_Byte_Count);
      else
         return APtr.Element_Byte_Count;
      end if;
   end Item_Value_Array_Get_Element_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Array_Set_Element_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      APtr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         APtr.Element_Byte_Count := Swap_Unsigned_32 (Value);
      else
         APtr.Element_Byte_Count := Value;
      end if;
   end Item_Value_Array_Set_Element_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Array_Get_First_Element_Offset (S:Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Array_Header_Byte_Count;
   end Item_Value_Array_Get_First_Element_Offset;
   

   -----------------------------------------------------------------------------
   -- Item Value: Sequence
   -----------------------------------------------------------------------------
   
   
   type Item_Value_Sequence_Header is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
      end record;

   for Item_Value_Sequence_Header'Size use 32 + 32;

   for Item_Value_Sequence_Header use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
      end record;

   Item_Value_Sequence_Header_Byte_Count: constant Unsigned_32 := Item_Value_Sequence_Header'Size / 8;

   type Item_Value_Sequence_Header_Ptr is access Item_Value_Sequence_Header;

   function To_Item_Value_Sequence_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Sequence_Header_Ptr);

   function Get_Item_Value_Sequence_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Sequence_Header_Ptr is (To_Item_Value_Sequence_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   
   -----------------------------------------------------------------------------
   
   function Item_Value_Sequence_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      SPtr: Item_Value_Sequence_Header_Ptr := Get_Item_Value_Sequence_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (SPtr.Item_Count);
      else
         return SPtr.Item_Count;
      end if;
   end Item_Value_Sequence_Get_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Sequence_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      SPtr: Item_Value_Sequence_Header_Ptr := Get_Item_Value_Sequence_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         SPtr.Item_Count := Swap_Unsigned_32 (Value);
      else
         SPtr.Item_Count := Value;
      end if;
   end Item_Value_Sequence_Set_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Sequence_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Sequence_Header_Byte_Count;
   end Item_Value_Sequence_Get_First_Item_Offset;
   


   -----------------------------------------------------------------------------
   -- Item Value: Dictionary
   -----------------------------------------------------------------------------

   type Item_Value_Dictionary_Header is
      record
         Reserved: Unsigned_32;
         Item_Count: Unsigned_32;
      end record;

   for Item_Value_Dictionary_Header'Size use 32 + 32;

   for Item_Value_Dictionary_Header use
      record
         Reserved at 0 range 0..31;
         Item_Count at 4 range 0..31;
      end record;

   Item_Value_Dictionary_Header_Byte_Count: constant Unsigned_32 := Item_Value_Dictionary_Header'Size / 8;

   type Item_Value_Dictionary_Header_Ptr is access Item_Value_Dictionary_Header;

   function To_Item_Value_Dictionary_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Dictionary_Header_Ptr);

   function Get_Item_Value_Dictionary_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Dictionary_Header_Ptr is (To_Item_Value_Dictionary_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Dictionary_Get_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      SPtr: Item_Value_Dictionary_Header_Ptr := Get_Item_Value_Dictionary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (SPtr.Item_Count);
      else
         return SPtr.Item_Count;
      end if;
   end Item_Value_Dictionary_Get_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Dictionary_Set_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
            SPtr: Item_Value_Dictionary_Header_Ptr := Get_Item_Value_Dictionary_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         SPtr.Item_Count := Swap_Unsigned_32 (Value);
      else
         SPtr.Item_Count := Value;
      end if;
   end Item_Value_Dictionary_Set_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Dictionary_Get_First_Item_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Dictionary_Header_Byte_Count;
   end Item_Value_Dictionary_Get_First_Item_Offset;
     


   -----------------------------------------------------------------------------
   -- Item Value: Table

   type Item_Value_Table_Header is
      record
         Row_Count: Unsigned_32;
         Column_Count: Unsigned_32;
         Fields_Start: Unsigned_32;
         Row_Byte_Count: Unsigned_32;
      end record;

   for Item_Value_Table_Header'Size use 32 + 32 + 32 + 32;

   for Item_Value_Table_Header use
      record
         Row_Count at 0 range 0..31;
         Column_Count at 4 range 0..31;
         Fields_Start at 8 range 0..31;
         Row_Byte_Count at 12 range 0..31;
      end record;
   
   Item_Value_Table_Header_Byte_Count: constant Unsigned_32 := Item_Value_Table_Header'Size / 8;

   type Item_Value_Table_Header_Ptr is access Item_Value_Table_Header;

   function To_Item_Value_Table_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Table_Header_Ptr);

   function Get_Item_Value_Table_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Table_Header_Ptr is (To_Item_Value_Table_Header_Ptr (Get_Value_Ptr (S, Item_Offset)));

   
   -----------------------------------------------------------------------------
   
   function Item_Value_Table_Get_Row_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (TPtr.Row_Count);
      else
         return TPtr.Row_Count;
      end if;
   end Item_Value_Table_Get_Row_Count;
   

   -----------------------------------------------------------------------------
   
   procedure Item_Value_Table_Set_Row_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         TPtr.Row_Count := Swap_Unsigned_32 (Value);
      else
         TPtr.Row_Count := Value;
      end if;
   end Item_Value_Table_Set_Row_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Table_Get_Column_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (TPtr.Column_Count);
      else
         return TPtr.Column_Count;
      end if;
   end Item_Value_Table_Get_Column_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Table_Set_Column_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         TPtr.Column_Count := Swap_Unsigned_32 (Value);
      else
         TPtr.Column_Count := Value;
      end if;
   end Item_Value_Table_Set_Column_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Table_Get_Fields_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (Get_Value_Offset (S, Item_Offset) + TPtr.Fields_Start);
      else
         return Get_Value_Offset (S, Item_Offset) + TPtr.Fields_Start;
      end if;
   end Item_Value_Table_Get_Fields_Offset;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Table_Get_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (TPtr.Row_Byte_Count);
      else
         return TPtr.Row_Byte_Count;
      end if;
   end Item_Value_Table_Get_Row_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Item_Value_Table_Set_Row_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
      TPtr: Item_Value_Table_Header_Ptr := Get_Item_Value_Table_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         TPtr.Row_Byte_Count := Swap_Unsigned_32 (Value);
      else
         TPtr.Row_Byte_Count := Value;
      end if;
   end Item_Value_Table_Set_Row_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Item_Value_Table_Get_Column_Descriptor_Start_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Table_Header_Byte_Count;
   end Item_Value_Table_Get_Column_Descriptor_Start_Offset;
   

   -----------------------------------------------------------------------------
   -- Table - Column Descriptor

   type Column_Descriptor is
      record
         Name_CRC: Unsigned_16;
         Name_Byte_Count: Unsigned_8;
         Field_Type: Item_Type;
         Name_Offset: Unsigned_32;
         Field_Offset: Unsigned_32;
         Field_Byte_Count: Unsigned_32;
      end record;

   for Column_Descriptor'Size use 16 + 8 + 8 + 32 + 32 + 32;

   for Column_Descriptor use
      record
         Name_CRC         at 0  range 0..15;
         Name_Byte_Count  at 2  range 0..7;
         Field_Type       at 3  range 0..7;
         Name_Offset      at 4  range 0..31;
         Field_Offset     at 8  range 0..31;
         Field_Byte_Count at 12 range 0..31;
      end record;
   
   Column_Descriptor_Byte_Count: constant Unsigned_32 := Column_Descriptor'Size / 8;

   type Column_Descriptor_Ptr is access Column_Descriptor;

   function To_Column_Descriptor_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Column_Descriptor_Ptr);
   
   function Get_Column_Descriptor_Offset (S: Store; Item_Offset: Unsigned_32; Column_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Table_Header_Byte_Count + Column_Offset * Column_Descriptor_Byte_Count;
   end Get_Column_Descriptor_Offset;
   
   pragma Inline (Get_Column_Descriptor_Offset);
   
   
   -----------------------------------------------------------------------------
   
   function Get_Column_Descriptor_Ptr (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Column_Descriptor_Ptr is
   begin
      return To_Column_Descriptor_Ptr (S.Data (Get_Column_Descriptor_Offset (S, Table_Offset, Descriptor))'Access);
   end Get_Column_Descriptor_Ptr;
   
   
   -----------------------------------------------------------------------------

   function Column_Descriptor_Get_Name_CRC (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return CRC_16 is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         return CRC_16 (Swap_Unsigned_32 (Unsigned_32 (CDPtr.Name_CRC)));
      else
         return CDPtr.Name_CRC;
      end if;
   end Column_Descriptor_Get_Name_CRC;
   
   
   -----------------------------------------------------------------------------

   procedure Column_Descriptor_Set_Name_CRC (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: CRC_16) is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         CDPtr.Name_CRC := CRC_16 (Swap_Unsigned_32 (Unsigned_32 (Value)));
      else
         CDPtr.Name_CRC := Value;
      end if;
   end Column_Descriptor_Set_Name_CRC;
   
   
   -----------------------------------------------------------------------------

   function Column_Descriptor_Get_Name_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_8 is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      return CDPtr.Name_Byte_Count;
   end Column_Descriptor_Get_Name_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Column_Descriptor_Set_Name_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_8) is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      CDPtr.Name_Byte_Count := Value;
   end Column_Descriptor_Set_Name_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   function Column_Descriptor_Get_Field_Type (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Item_Type is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      return CDPtr.Field_Type;
   end Column_Descriptor_Get_Field_Type;
   
   
   -----------------------------------------------------------------------------

   procedure Column_Descriptor_Set_Field_Type (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Item_Type) is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      CDPtr.Field_Type := Value;
   end Column_Descriptor_Set_Field_Type;
   
   
   -----------------------------------------------------------------------------

   function Column_Descriptor_Get_Name_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_32 is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (CDPtr.Name_Offset);
      else
         return CDPtr.Name_Offset;
      end if;
   end Column_Descriptor_Get_Name_Offset;
   
   
   -----------------------------------------------------------------------------

   procedure Column_Descriptor_Set_Name_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_32) is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         CDPtr.Name_Offset := Swap_Unsigned_32 (Value);
      else
         CDPtr.Name_Offset := Value;
      end if;
   end Column_Descriptor_Set_Name_Offset;
   
   
   -----------------------------------------------------------------------------

   function Column_Descriptor_Get_Field_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_32 is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (CDPtr.Field_Offset);
      else
         return CDPtr.Field_Offset;
      end if;
   end Column_Descriptor_Get_Field_Offset;
   
   
   -----------------------------------------------------------------------------

   procedure Column_Descriptor_Set_Field_Offset (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_32) is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         CDPtr.Field_Offset := Swap_Unsigned_32 (Value);
      else
         CDPtr.Field_Offset := Value;
      end if;
   end Column_Descriptor_Set_Field_Offset;
   
   
   -----------------------------------------------------------------------------

   function Column_Descriptor_Get_Field_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32) return Unsigned_32 is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         return Swap_Unsigned_32 (CDPtr.Field_Byte_Count);
      else
         return CDPtr.Field_Byte_Count;
      end if;
   end Column_Descriptor_Get_Field_Byte_Count;
   
   
   -----------------------------------------------------------------------------

   procedure Column_Descriptor_Set_Field_Byte_Count (S: Store; Table_Offset: Unsigned_32; Descriptor: Unsigned_32; Value: Unsigned_32) is
      CDPtr: Column_Descriptor_Ptr := Get_Column_Descriptor_Ptr (S, Table_Offset, Descriptor);
   begin
      if S.Swap then
         CDPtr.Field_Byte_Count := Swap_Unsigned_32 (Value);
      else
         CDPtr.Field_Byte_Count := Value;
      end if;
   end Column_Descriptor_Set_Field_Byte_Count;


   -----------------------------------------------------------------------------
   -- Small Item Value: RGBA (Color)




   -----------------------------------------------------------------------------
   -- Item Value: Font

   type Item_Value_Font_Header is
      record
         Size: IEEE_Float_32;
         Family_Name_Byte_Count: Unsigned_8;
         Font_Name_Byte_Count: Unsigned_8;
      end record;

   for Item_Value_Font_Header'Size use 32 + 8 + 8;

   for Item_Value_Font_Header use
      record
         Size at 0 range 0..31;
         Family_Name_Byte_Count at 4 range 0..7;
         Font_Name_Byte_count at 5 range 0..7;
      end record;

   Item_Value_Font_Header_Byte_Count: constant Unsigned_32 := Item_Value_Font_Header'Size / 8;
   
   type Item_Value_Font_Header_Ptr is access Item_Value_Font_Header;

   function To_Item_Value_Font_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Value_Font_Header_Ptr);

   function Get_Item_Value_Font_Header_Ptr (S: Store; Item_Offset: Unsigned_32) return Item_Value_Font_Header_Ptr is
   begin
      return To_Item_Value_Font_Header_Ptr (S.Data (Item_Offset + Get_Value_Offset (S, Item_Offset))'Access);
   end Get_Item_Value_Font_Header_Ptr;
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Font_Get_Size (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32 is
      FPtr: Item_Value_Font_Header_Ptr := Get_Item_Value_Font_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         return Swap_Float_32 (FPtr.Size);
      else
         return FPtr.Size;
      end if;
   end Item_Value_Font_Get_Size;
   
   
   -----------------------------------------------------------------------------
   
   procedure Item_Value_Font_Set_Size (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32) is
      FPtr: Item_Value_Font_Header_Ptr := Get_Item_Value_Font_Header_Ptr (S, Item_Offset);
   begin
      if S.Swap then
         FPtr.Size := Swap_Float_32 (Value);
      else
         FPtr.Size := Value;
      end if;
   end Item_Value_Font_Set_Size;
           
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Font_Get_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
      FPtr: Item_Value_Font_Header_Ptr := Get_Item_Value_Font_Header_Ptr (S, Item_Offset);
   begin
      return FPtr.Family_Name_Byte_Count;
   end Item_Value_Font_Get_Family_Name_Byte_Count;

   
   -----------------------------------------------------------------------------
   
   procedure Item_Value_Font_Set_Family_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
      FPtr: Item_Value_Font_Header_Ptr := Get_Item_Value_Font_Header_Ptr (S, Item_Offset);
   begin
      FPtr.Family_Name_Byte_Count := Value;
   end Item_Value_Font_Set_Family_Name_Byte_Count;
                                       
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Font_Get_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
      FPtr: Item_Value_Font_Header_Ptr := Get_Item_Value_Font_Header_Ptr (S, Item_Offset);
   begin
      return FPtr.Font_Name_Byte_Count;
   end Item_Value_Font_Get_Font_Name_Byte_Count;

   
   -----------------------------------------------------------------------------
   
   procedure Item_Value_Font_Set_Font_Name_Byte_Count (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
      FPtr: Item_Value_Font_Header_Ptr := Get_Item_Value_Font_Header_Ptr (S, Item_Offset);
   begin
      FPtr.Font_Name_Byte_Count := Value;
   end Item_Value_Font_Set_Font_Name_Byte_Count;
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Font_Get_Family_Name_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Offset + Get_Value_Offset (S, Item_Offset) + Item_Value_Font_Header_Byte_Count; 
   end Item_Value_Font_Get_Family_Name_Offset;
   
   
   -----------------------------------------------------------------------------
   
   function Item_Value_Font_Get_Font_Name_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Item_Value_Font_Get_Family_Name_Offset (S, Item_Offset) + Unsigned_32 (Item_Value_Font_Get_Family_Name_Byte_Count (S, Item_Offset));
   end Item_Value_Font_Get_Font_Name_Offset;
      
   
   -----------------------------------------------------------------------------
   
   procedure Create_Layout
    (
     S: Store;
     At_Offset: Unsigned_32;
     Of_Type: Item_Type;
     With_Name: Name_Field_Assistent;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    ) is

      T: Item_Type renames Of_Type;
      O: Unsigned_32 renames At_Offset;
      N: Name_Field_Assistent renames With_Name;
      B: Unsigned_32 renames Using_Byte_Count;
      P: Unsigned_32 renames Parent_Offset;
      
      Item_Ptr: Item_Header_Ptr := To_Item_Header_Ptr (S.Data (At_Offset)'Access);
      
   begin

      if T = Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create illegal item type");
      end if;
      
      Item_Ptr.Type_Field := T;
      Item_Ptr.Options_Field := No_Item_Options;
      Item_Ptr.Flags_Field := No_Item_Flags;
      Item_Ptr.Name_Field_Byte_Count_Field := N.Field_Byte_Count;
      Item_Ptr.Parent_Offset_Field := P;
      Item_Ptr.Byte_Count_Field := B;
      
      Set_Name (S, At_Offset, N);
      
      case T is
         when Illegal => null; -- Cannot happen
         when Null_Type | Bool_Type | Int_8_Type | Int_16_Type | Int_32_Type | UInt_8_Type | UInt_16_Type | UInt_32_Type | Float_32_Type | RGBA_Type =>
            Set_UInt32 (S, O, 0);
         when Int_64_Type =>
            Container.Set_Integer_64 (S, Get_Value_Offset (S, At_Offset), 0);
         when UInt_64_Type =>
            Container.Set_Unsigned_64 (S, Get_Value_Offset (S, At_Offset), 0);
         when Float_64_Type =>
            Container.Set_Float_64 (S, Get_Value_Offset (S, At_Offset), 0.0);
         when String_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
         when Crc_String_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Binary_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
         when Crc_Binary_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Array_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 8, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 12, 0);
         when Dictionary_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Sequence_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
         when Table_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 4, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 8, 0);
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset) + 12, 0);
         when UUID_Type =>
            Container.Set_Unsigned_64 (S, Get_Value_Offset (S, At_Offset), 0);
         when Font_Type =>
            Container.Set_Unsigned_32 (S, Get_Value_Offset (S, At_Offset), 0);
      end case;

   end Create_Layout;

   
   -- Create a new array layout at the given offset
   --
   procedure Create_Array_Layout
     (
      S: Store;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent;
      For_Element_Type: Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) is
  
      Index: Unsigned_32 renames At_Offset;
      Name: Name_Field_Assistent renames With_Name;
      Element_Type: Item_Type renames For_Element_Type;
      Element_Count: Unsigned_32 renames Max_Element_Count;
      
      Element_Byte_Count: Unsigned_32 := Using_Element_Byte_Count;

   begin
      
      -- Check input parameters for validity
      --
      if Element_Byte_Count = 0 then
         Ada.Exceptions.Raise_Exception (BRBON.Byte_Count_Error'Identity, "Cannot create array with 0 as element byte count");
      end if;
      --
      if Element_Count = 0 then
         Ada.Exceptions.Raise_Exception (BRBON.Array_Error'Identity, "Cannot create array with 0 elements");
      end if;
      
      
      -- Calculate the actual element byte count
      -- (all elements must be properly aligned according to their inherent byte count)
      --
      case Element_Type is
         when Illegal => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot create an array of Illegal elements");
         when Null_Type => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot create an array of Null_Type elements");
         when Bool_Type | UInt_8_Type | Int_8_Type => null;
         when UInt_16_Type | Int_16_Type =>
            if Element_Byte_Count mod 2 = 1 then
               Element_Byte_Count := Element_Byte_Count + 1;
            end if;
         when UInt_32_Type | Int_32_Type | Float_32_Type =>
            if Element_Byte_Count mod 2 = 1 then
               Element_Byte_Count := Element_Byte_Count + 1;
            end if;
            if Element_Byte_Count mod 4 = 2 then
               Element_Byte_Count := Element_Byte_Count + 2;
            end if;
         when UInt_64_Type | Int_64_Type | Float_64_Type | UUID_Type | RGBA_Type =>
            Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
         when Sequence_Type | Dictionary_Type =>
            if Element_Byte_Count < Item_Header_Byte_Count then
               Element_Byte_Count := Item_Header_Byte_Count;
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Array_Type =>
            if Element_Byte_Count < Item_Header_Byte_Count + Item_Overhead_Byte_Count (Array_Type) then
               Element_Byte_Count := Item_Header_Byte_Count + Item_Overhead_Byte_Count (Array_Type);
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Table_Type =>
            if Element_Byte_Count < Item_Header_Byte_Count + Item_Overhead_Byte_Count (Table_Type) then
               Element_Byte_Count := Item_Header_Byte_Count + Item_Overhead_Byte_Count (Table_Type);
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when String_Type | Crc_String_Type | Binary_Type | Crc_Binary_Type | Font_Type =>
            Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
      end case;

         
      -- Create basic item layout
      --
      Create_Layout (S                => S,
                     At_Offset        => Index,
                     Of_Type          => Array_Type,
                     With_Name        => Name,
                     Using_Byte_Count => Item_Overhead_Byte_Count (Array_Type) + Element_Byte_Count * Element_Count,
                     Parent_Offset    => 0);
      
      -- Initialize the array specific parameters
      --
      declare
         Array_Header_Ptr: Item_Value_Array_Header_Ptr := Get_Item_Value_Array_Header_Ptr (S, At_Offset);
      begin
         Array_Header_Ptr.Element_Type := Element_Type;
         Array_Header_Ptr.Element_Byte_Count := Element_Byte_Count;
      end;
      
   end Create_Array_Layout;
   
   
   -- Get_Value_Offset
   --
   Use_Small_Value_LUT: constant Array (Item_Type) of Boolean :=
     -- Ill  Null  Bool  i8    i16   i32   i64    u8    u16   u32   u64    f32   f64    str    cstr   bin    cbin   arr    dict   seq    tab    uuid   rgb   font
     (False, True, True, True, True, True, False, True, True, True, False, True, False, False, False, False, False, False, False, False, False, False, True, False);
   
      
end BRBON.Item_Access;

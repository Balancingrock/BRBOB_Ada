with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with CRC_Package; use CRC_Package;

with BRBON.Utils;
with BRBON.Container;
with BRBON.Portal_Package; use BRBON.Portal_Package;


package body BRBON.Item_Package is
   
   
   function Get_Value_Ptr (S: Store; Item_Offset: Unsigned_32) return Unsigned_8_Ptr is
   begin
      return S.Data (Get_Value_Offset (S, Item_Offset))'Access;
   end Get_Value_Ptr;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Bool (S: Store; Item_Offset: Unsigned_32) return Boolean is
   begin
      return Container.Get_Bool (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Bool;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Bool (S: Store; Item_Offset: Unsigned_32; Value: Boolean) is
   begin
      Container.Set_Bool (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Bool;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int8 (S: Store; Item_Offset: Unsigned_32) return Integer_8 is
   begin
      return Container.Get_Integer_8 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int8;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int8 (S: Store; Item_Offset: Unsigned_32; Value: Integer_8) is
   begin
      Container.Set_Integer_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int8;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt8 (S: Store; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt8;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt8 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt8;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int16 (S: Store; Item_Offset: Unsigned_32) return Integer_16 is
   begin
      return Container.Get_Integer_16 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int16;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int16 (S: Store; Item_Offset: Unsigned_32; Value: Integer_16) is
   begin
      Container.Set_Integer_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int16;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt16 (S: Store; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt16;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt16 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt16;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Int32 (S: Store; Item_Offset: Unsigned_32) return Integer_32 is
   begin
      return Container.Get_Integer_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Int32;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Int32 (S: Store; Item_Offset: Unsigned_32; Value: Integer_32) is
   begin
      Container.Set_Integer_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Int32;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_UInt32 (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_UInt32;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_UInt32 (S: Store; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_UInt32;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Float32 (S: Store; Item_Offset: Unsigned_32) return IEEE_Float_32 is
   begin
      return Container.Get_Float_32 (S, Item_Offset + Small_Value_Offset);
   end Small_Value_Get_Float32;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Float32 (S: Store; Item_Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      Container.Set_Float_32 (S, Item_Offset + Small_Value_Offset, Value);
   end Small_Value_Set_Float32;
   
   
   -----------------------------------------------------------------------------
   
   function Small_Value_Get_Color (S: Store; Item_Offset: Unsigned_32) return Color is
   begin
      return Color_Factory (Red   => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset),
                            Green => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1),
                            Blue  => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2),
                            Alpha => Container.Get_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3));
   end Small_Value_Get_Color;
   
   
   -----------------------------------------------------------------------------
   
   procedure Small_Value_Set_Color (S: Store; Item_Offset: Unsigned_32; Value: Color) is
   begin
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset, Get_Red_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 1, Get_Green_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 2, Get_Blue_Component (Value));
      Container.Set_Unsigned_8 (S, Item_Offset + Small_Value_Offset + 3, Get_Alpha_Component (Value));
   end Small_Value_Set_Color;
   
   
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
      
      Set_Name (Item_Ptr, N);
      
      case T is
         when Illegal => null; -- Cannot happen
         when Null_Type | Bool_Type | Int_8_Type | Int_16_Type | Int_32_Type | UInt_8_Type | UInt_16_Type | UInt_32_Type | Float_32_Type | RGBA_Type =>
            Small_Value_Set_UInt32 (S, O, 0);
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
   function Create_Array_Layout
     (
      S: Store;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent;
      For_Element_Type: Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal is
  
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
         Array_Header_Ptr: Item_Value_Header_Array_Ptr := To_Item_Value_Header_Array_Ptr (Get_Value_Ptr (S, At_Offset));
      begin
         Array_Header_Ptr.Element_Type := Element_Type;
         Array_Header_Ptr.Element_Byte_Count := Element_Byte_Count;
      end;
      
      return Portal_Factory (S, Index);
      
   end Create_Array_Layout;
   
   
   -- Get_Value_Offset
   --
   Use_Small_Value_LUT: constant Array (Item_Type) of Boolean :=
     -- Ill  Null  Bool  i8    i16   i32   i64    u8    u16   u32   u64    f32   f64    str    cstr   bin    cbin   arr    dict   seq    tab    uuid   rgb   font
     (False, True, True, True, True, True, False, True, True, True, False, True, False, False, False, False, False, False, False, False, False, False, True, False);
   --                                         
   function Get_Value_Offset (S: Store; Item_Offset: Unsigned_32) return Unsigned_32 is
      
      Item_Ptr : Item_Header_Ptr := To_Item_Header_Ptr (S.Data (Item_Offset)'Access);
      T: Item_Type := Item_Ptr.Type_Field;
   
   begin
      
      if T = Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Store contains illegal item type (16#00#)");
      end if;

      if Use_Small_Value_LUT (T) then
         return Item_Offset + Small_Value_Offset;
      else
         return Item_Offset + Item_Header_Byte_Count + Unsigned_32 (Item_Ptr.Name_Field_Byte_Count_Field);
      end if;
   
   end Get_Value_Offset;
   
   
   
   function Get_Name_CRC (IPtr: Item_Header_Ptr) return Unsigned_16 is
   begin
      return To_Item_Name_Field_Ptr (IPtr.Next_Byte'Access).CRC;
   end Get_Name_CRC;
   
   
   function Get_Name_Byte_Count (IPtr: Item_Header_Ptr) return Unsigned_8 is
   begin
      return To_Item_Name_Field_Ptr (IPtr.Next_Byte'Access).Byte_Count;
   end Get_Name_Byte_Count;
   

   function Get_Name_String (IPtr: Item_Header_Ptr) return String is
      Length: Integer := Integer (Get_Name_Byte_Count (IPtr));
      ASCII_Ptr: Unsigned_8_Ptr := To_Item_Name_Field_Ptr (IPtr.Next_Byte'Access).ASCII_Code'Access;
      subtype StrT is String (1 .. Length);
      type StrTPtr is access StrT;
      function ToStrTPtr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, StrTPtr);
   begin
      return ToStrTPtr (ASCII_Ptr).all;
   end Get_Name_String;
   
   
   function Get_Name_Quick_Check_Value (IPtr: Item_Header_Ptr) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (IPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset);
   end Get_Name_Quick_Check_Value;
     
   procedure Set_Name (IPtr: Item_Header_Ptr; Value: Name_Field_Assistent.Instance) is
   begin
      if Value.Name_Field_Byte_Count > 0 then
         Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Byte_Count_Offset, Value.Name_Field_Byte_Count);
         Container.Set_Unsigned_16 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset, Value.CRC);
         Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Byte_Count_Offset, Unsigned_8 (Value.Ascii_Byte_Count));
         Container.Set_Unsigned_8_Array (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Code_Offset, Value.Ascii_Code (1 .. Unsigned_32 (Value.Ascii_Byte_Count)));
      end if;
   end Set_Name;
   
   
   procedure Set_Name_CRC (IPtr: Item_Header_Ptr; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (CPtr, Item_Offset + Name_Field_CRC_Offset, Value);
   end Set_Name_CRC;

   
   procedure Set_Name_Byte_Count (IPtr: Item_Header_Ptr; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Value);
   end Set_Name_Byte_Count;

   
   procedure Set_Name_String (IPtr: Item_Header_Ptr; Value: String) is
   begin
      Container.Set_String (CPtr, Item_Offset + Name_Field_ASCII_Code_Offset, Value);
   end Set_Name_String;
      
end BRBON.Item_Package;

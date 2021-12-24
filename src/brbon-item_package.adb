with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with CRC_Package;

with BRBON.Utils;
with BRBON.Types; use BRBON.Types;


package body BRBON.Item_Package is

   -- ==========================================================================
   -- API
   -- ==========================================================================

   
   procedure Create_Layout
    (
     CPtr: Types.Unsigned_8_Ptr;
     At_Offset: Unsigned_32;
     Of_Type: Types.Item_Type;
     With_Name: Name_Field_Assistent.Instance;
     Using_Byte_Count: Unsigned_32 := 0;
     Parent_Offset: Unsigned_32 := 0
    ) is

      T: Types.Item_Type renames Of_Type;
      O: Unsigned_32 renames At_Offset;
      N: Name_Field_Assistent.Instance renames With_Name;
      B: Unsigned_32 renames Using_Byte_Count;
      P: Unsigned_32 renames Parent_Offset;

      Value_Offset: Unsigned_32 := At_Offset + Types.Minimum_Item_Byte_Count + Unsigned_32 (With_Name.Name_Field_Byte_Count);
      
   begin

      if T = Types.Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Item_Type'Identity, "Cannot create illegal item type");
      end if;
      
      Item.Set_Type (CPtr, O, T);
      Item.Set_Options (CPtr, O, Types.No_Item_Options);
      Item.Set_Flags (CPtr, O, Types.No_Item_Flags);
      Item.Set_Byte_Count (CPtr, O, B);
      Item.Set_Parent_Offset (CPtr, O, P);
      Item.Set_Name (CPtr, O, N);
      
      case T is
         when Illegal => null; -- Cannot happen
         when Types.Null_Type | Types.Bool_Type | Types.Int_8_Type | Types.Int_16_Type | Types.Int_32_Type | Types.UInt_8_Type | Types.UInt_16_Type | Types.UInt_32_Type | Types.Float_32_Type | Types.RGBA_Type =>
            Item.Set_Small_Value (CPtr, O, 0);
         when Types.Int_64_Type =>
            Container.Set_Integer_64 (CPtr, Value_Offset, 0);
         when Types.UInt_64_Type =>
            Container.Set_Unsigned_64 (CPtr, Value_Offset, 0);
         when Types.Float_64_Type =>
            Container.Set_Float_64 (CPtr, Value_Offset, 0.0);
         when Types.String_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
         when Types.Crc_String_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 4, 0);
         when Types.Binary_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
         when Types.Crc_Binary_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 4, 0);
         when Types.Array_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 4, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 8, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 12, 0);
         when Types.Dictionary_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 4, 0);
         when Types.Sequence_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 4, 0);
         when Types.Table_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 4, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 8, 0);
            Container.Set_Unsigned_32 (CPtr, Value_Offset + 12, 0);
         when Types.UUID_Type =>
            Container.Set_Unsigned_64 (CPtr, Value_Offset, 0);
         when Types.Font_Type =>
            Container.Set_Unsigned_32 (CPtr, Value_Offset, 0);
      end case;

   end Create_Layout;

   
   -- Create a new array layout at the given offset
   --
   function Create_Array_Layout
     (
      CPtr: Container.Instance_Ptr;
      At_Offset: Unsigned_32;
      With_Name: Name_Field_Assistent.Instance;
      For_Element_Type: Types.Item_Type;
      Using_Element_Byte_Count: Unsigned_32;
      Max_Element_Count: Unsigned_32
     ) return Portal.Instance is
  
      Index: Unsigned_32 renames At_Offset;
      Name: BRBON.Name_Field_Assistent.Instance renames With_Name;
      Element_Type: Types.Item_Type renames For_Element_Type;
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
         when Types.Illegal => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot create an array of Illegal elements");
         when Types.Null_Type => Ada.Exceptions.Raise_Exception (BRBON.Illegal_Item_Type'Identity, "Cannot create an array of Null_Type elements");
         when Types.Bool_Type | Types.UInt_8_Type | Types.Int_8_Type => null;
         when Types.UInt_16_Type | Types.Int_16_Type =>
            if Element_Byte_Count mod 2 = 1 then
               Element_Byte_Count := Element_Byte_Count + 1;
            end if;
         when Types.UInt_32_Type | Types.Int_32_Type | Float_32_Type =>
            if Element_Byte_Count mod 2 = 1 then
               Element_Byte_Count := Element_Byte_Count + 1;
            end if;
            if Element_Byte_Count mod 4 = 2 then
               Element_Byte_Count := Element_Byte_Count + 2;
            end if;
         when Types.UInt_64_Type | Types.Int_64_Type | Float_64_Type | Types.UUID_Type | Types.RGBA_Type =>
            Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
         when Types.Sequence_Type | Types.Dictionary_Type =>
            if Element_Byte_Count < Types.Minimum_Item_Byte_Count then
               Element_Byte_Count := Types.Minimum_Item_Byte_Count;
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Types.Array_Type =>
            if Element_Byte_Count < Types.Minimum_Item_Byte_Count + Types.Item_Overhead_Byte_Count (Types.Array_Type) then
               Element_Byte_Count := Types.Minimum_Item_Byte_Count + Types.Item_Overhead_Byte_Count (Types.Array_Type);
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Types.Table_Type =>
            if Element_Byte_Count < Types.Minimum_Item_Byte_Count + Types.Item_Overhead_Byte_Count (Types.Table_Type) then
               Element_Byte_Count := Types.Minimum_Item_Byte_Count + Types.Item_Overhead_Byte_Count (Types.Table_Type);
            else
               Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
            end if;
         when Types.String_Type | Types.Crc_String_Type | Types.Binary_Type | Types.Crc_Binary_Type | Types.Font_Type =>
            Element_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_of_8 (Element_Byte_Count);
      end case;

         
      -- Create basic item layout
      --
      Create_Layout (CPtr             => CPtr,
                     At_Offset        => Index,
                     Of_Type          => Types.Array_Type,
                     With_Name        => Name,
                     Using_Byte_Count => Types.Item_Overhead_Byte_Count (Types.Array_Type) + Element_Byte_Count * Element_Count,
                     Parent_Offset    => 0);
      
      -- Initialize the array specific parameters
      --
      Container.Set_Unsigned_8 (CPtr, Item.Value_Offset (CPtr, Index) + Array_Element_Type_Offset, Types.To_Unsigned_8 (Element_Type));
      Container.Set_Unsigned_32 (CPtr, Item.Value_Offset (CPtr, Index) + Array_Element_Byte_Count_Offset, Element_Byte_Count);
      
      
      return Portal.Factory (CPtr, Index);
      
   end Create_Array_Layout;
   
   
   -- Get_Value_Offset
   --
   Use_Small_Value_LUT: constant Array (Types.Item_Type) of Boolean :=
     -- Ill  Null  Bool  i8    i16   i32   i64    u8    u16   u32   u64    f32   f64    str    cstr   bin    cbin   arr    dict   seq    tab    uuid   rgb   font
     (False, True, True, True, True, True, False, True, True, True, False, True, False, False, False, False, False, False, False, False, False, False, True, False);
   --                                         
   function Value_Offset (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32 is
      
      T: Types.Item_Type := Types.Illegal; -- := Item.Get_Type (CPtr, Item_Offset);
   
   begin
      
      if T = Types.Illegal then
         Ada.Exceptions.Raise_Exception (Illegal_Block_Type'Identity, "Container contains illegal item type (16#00#)");
      end if;

      if Use_Small_Value_LUT (T) then
         return Item_Offset + Small_Value_Offset;
      else
         return Item_Offset + Types.Minimum_Item_Byte_Count + Unsigned_32 (Item.Get_Name_Field_Byte_Count (CPtr, Item_Offset));
      end if;
   
   end Value_Offset;
   
   
   --   function Get_Type (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Types.Item_Type is
   function Get_Type (Item_Ptr: Types.Unsigned_8_Ptr) return Types.Item_Type is
   begin
      return To_Item_Layout_Ptr (Item_Ptr).Type_Field;
   end Get_Type;
   
   
   function Get_Options (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Types.Item_Options is
   begin
      return Types.To_Item_Options (Container.Get_Unsigned_8 (CPtr, Item_Offset + Options_Offset));
   end Get_Options;
   
   
   function Get_Flags (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Types.Item_Flags is
   begin
      return Types.To_Item_Flags (Container.Get_Unsigned_8 (CPtr, Item_Offset + Flags_Offset));
   end Get_Flags;
   
   
   function Get_Name_Field_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (CPtr, Item_Offset + Name_Field_Byte_Count_Offset);
   end Get_Name_Field_Byte_Count;
   
   
   function Get_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (CPtr, Item_Offset + Byte_Count_Offset);
   end Get_Byte_Count;
   
   
   function Get_Small_Value (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (CPtr, Item_Offset + Small_Value_Offset);
   end Get_Small_Value;
   
   
   function Get_Parent_Offset (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (CPtr, Item_Offset + Parent_Offset_Offset);
   end Get_Parent_Offset;
   
   
   function Get_Name_CRC (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_16 is
   begin
      return Container.Get_Unsigned_16 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset);
   end Get_Name_CRC;
   
   
   function Get_Name_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_8 is
   begin
      return Container.Get_Unsigned_8 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Byte_Count_Offset);
   end Get_Name_Byte_Count;
   
 --
   function Get_Name_String (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return String is
      Length: Unsigned_32 := Unsigned_32 (Get_Name_Byte_Count (CPtr, Item_Offset));
   begin
      return Container.Get_String (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Code_Offset, Length);
   end Get_Name_String;
   
   
   function Get_Name_Quick_Check_Value (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32) return Unsigned_32 is
   begin
      return Container.Get_Unsigned_32 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset);
   end Get_Name_Quick_Check_Value;
   
   
   procedure Set_Type (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Types.Item_Type) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Type_Offset, Types.To_Unsigned_8 (Value));
   end Set_Type;

   
   procedure Set_Options (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Types.Item_Options) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Options_Offset, Types.To_Unsigned_8 (Value));
   end Set_Options;

   
   procedure Set_Flags (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Types.Item_Flags) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Flags_Offset, Types.To_Unsigned_8 (Value));
   end Set_Flags;

   
   procedure Set_Name_Field_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Byte_Count_Offset, Value);
   end Set_Name_Field_Byte_Count;

   
   procedure Set_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (CPtr, Item_Offset + Byte_Count_Offset, Value);
   end Set_Byte_Count;

   
   procedure Set_Small_Value (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (CPtr, Item_Offset + Small_Value_Offset, Value);
   end Set_Small_Value;

   
   procedure Set_Parent_Offset (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      Container.Set_Unsigned_32 (CPtr, Item_Offset + Parent_Offset_Offset, Value);
   end Set_Parent_Offset;

   
   procedure Set_Name (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Name_Field_Assistent.Instance) is
   begin
      if Value.Name_Field_Byte_Count > 0 then
         Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Byte_Count_Offset, Value.Name_Field_Byte_Count);
         Container.Set_Unsigned_16 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_CRC_Offset, Value.CRC);
         Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Byte_Count_Offset, Unsigned_8 (Value.Ascii_Byte_Count));
         Container.Set_Unsigned_8_Array (CPtr, Item_Offset + Name_Field_Offset + Name_Field_ASCII_Code_Offset, Value.Ascii_Code (1 .. Unsigned_32 (Value.Ascii_Byte_Count)));
      end if;
   end Set_Name;
   
   
   procedure Set_Name_CRC (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      Container.Set_Unsigned_16 (CPtr, Item_Offset + Name_Field_CRC_Offset, Value);
   end Set_Name_CRC;

   
   procedure Set_Name_Byte_Count (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      Container.Set_Unsigned_8 (CPtr, Item_Offset + Name_Field_ASCII_Byte_Count_Offset, Value);
   end Set_Name_Byte_Count;

   
   procedure Set_Name_String (CPtr: Container.Instance_Ptr; Item_Offset: Unsigned_32; Value: String) is
   begin
      Container.Set_String (CPtr, Item_Offset + Name_Field_ASCII_Code_Offset, Value);
   end Set_Name_String;
      
end BRBON.Item_Package;

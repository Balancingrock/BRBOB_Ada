with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.UTF_Encoding;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Interfaces.C.Pointers;

with BRBON; use BRBON;
with Storage_Area; use Storage_Area;
with UUID_Package; use UUID_Package;
with Color_Package; use Color_Package;
with Font_Package; use Font_Package;
with Pointer_Math; use Pointer_Math;

package Item is


   type Item_Name_Assistent (String_Length: Unsigned_32) is private;

   function Create_Item_Name_Assistent (S: Item_Name) return Item_Name_Assistent;


   -- Item Access
   --
   type Item_Access is tagged
      record
         Storage: Storage_Area_Ptr;
         Offset: Unsigned_32;
         Header_Ptr: Item_Header_Ptr;
      end record;


   -- ===============
   -- Types
   -- ===============

   procedure Create_Null        (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Get_Bool            (I: Item_Access'Class) return Boolean; pragma Inline (Get_Bool);
   procedure Set_Bool           (I: Item_Access'Class; Value: Boolean); pragma Inline (Set_Bool);
   procedure Create_Bool        (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Boolean := False);

   function Get_Integer_8       (I: Item_Access'Class) return Integer_8; pragma Inline (Get_Integer_8);
   procedure Set_Integer_8      (I: Item_Access'Class; Value: Integer_8); pragma Inline (Set_Integer_8);
   procedure Create_Integer_8   (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0);

   function Get_Integer_16      (I: Item_Access'Class) return Integer_16; pragma Inline (Get_Integer_16);
   procedure Set_Integer_16     (I: Item_Access'Class; Value: Integer_16); pragma Inline (Set_Integer_16);
   procedure Create_Integer_16  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0);

   function Get_Integer_32      (I: Item_Access'Class) return Integer_32; pragma Inline (Get_Integer_32);
   procedure Set_Integer_32     (I: Item_Access'Class; Value: Integer_32); pragma Inline (Set_Integer_32);
   procedure Create_Integer_32  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0);

   function Get_Integer_64      (I: Item_Access'Class) return Integer_64; pragma Inline (Get_Integer_64);
   procedure Create_Integer_64  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0);
   procedure Set_Integer_64     (I: Item_Access'Class; Value: Integer_64); pragma Inline (Set_Integer_64);

   function Get_Unsigned_8      (I: Item_Access'Class) return Unsigned_8; pragma Inline (Get_Unsigned_8);
   procedure Set_Unsigned_8     (I: Item_Access'Class; Value: Unsigned_8); pragma Inline (Set_Unsigned_8);
   procedure Create_Unsigned_8  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0);

   function Get_Unsigned_16     (I: Item_Access'Class) return Unsigned_16; pragma Inline (Get_Unsigned_16);
   procedure Set_Unsigned_16    (I: Item_Access'Class; Value: Unsigned_16); pragma Inline (Set_Unsigned_16);
   procedure Create_Unsigned_16 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0);

   function Get_Unsigned_32     (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Unsigned_32);
   procedure Set_Unsigned_32    (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Unsigned_32);
   procedure Create_Unsigned_32 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0);

   function Get_Unsigned_64     (I: Item_Access'Class) return Unsigned_64; pragma Inline (Get_Unsigned_64);
   procedure Set_Unsigned_64    (I: Item_Access'Class; Value: Unsigned_64); pragma Inline (Set_Unsigned_64);
   procedure Create_Unsigned_64 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0);

   function Get_Float_32        (I: Item_Access'Class) return IEEE_Float_32; pragma Inline (Get_Float_32);
   procedure Set_Float_32       (I: Item_Access'Class; Value: IEEE_Float_32); pragma Inline (Set_Float_32);
   procedure Create_Float_32    (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0);

   function Get_Float_64        (I: Item_Access'Class) return IEEE_Float_64; pragma Inline (Get_Float_64);
   procedure Set_Float_64       (I: Item_Access'Class; Value: IEEE_Float_64); pragma Inline (Set_Float_64);
   procedure Create_Float_64    (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0);

   procedure Get_String                (I: Item_Access'Class; Value: out String); pragma Inline (Get_String);
   procedure Set_String                (I: Item_Access'Class; Value: String); pragma Inline (Set_String);
   procedure Create_String             (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");

   function Get_CRC_String_CRC         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_String_CRC);
   function Get_CRC_String_Byte_Count  (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_String_Byte_Count);
   procedure Set_CRC_String            (I: Item_Access'Class; Value: String); pragma Inline (Set_CRC_String);
   procedure Get_CRC_String            (I: Item_Access'Class; Value: out String); pragma Inline (Get_CRC_String);
   procedure Create_CRC_String         (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");

   function Get_Binary_Byte_Count      (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Binary_Byte_Count);
   procedure Set_Binary                (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Set_Binary);
   procedure Get_Binary                (I: Item_Access'Class; Value: out Array_Of_Unsigned_8); pragma Inline (Get_Binary);
   procedure Create_Binary             (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);

   function Get_CRC_Binary_CRC         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_Binary_CRC);
   function Get_CRC_Binary_Byte_Count  (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_Binary_Byte_Count);
   procedure Set_CRC_Binary            (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Set_CRC_Binary);
   procedure Get_CRC_Binary            (I: Item_Access'Class; Value: out Array_Of_Unsigned_8); pragma Inline (Get_CRC_Binary);
   procedure Create_CRC_Binary         (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);

   function Get_Array_Element_Type        (I: Item_Access'Class) return BR_Item_Type; pragma Inline (Get_Array_Element_Type);
   function Get_Array_Element_Count       (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Array_Element_Count);
   function Get_Array_Element_Byte_Count  (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Array_Element_Byte_Count);
   function Get_Array_Element_Offset      (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32; pragma Inline (Get_Array_Element_Offset);
   procedure Set_Array_Element_Count      (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Count);
   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Byte_Count);
   procedure Create_Array                 (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128);

   function Get_Dictionary_Item_Count         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Dictionary_Item_Count);
   procedure Set_Dictionary_Item_Count        (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Dictionary_Item_Count);
   procedure Create_Dictionary                (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Get_Sequence_Item_Count           (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Sequence_Item_Count);
   procedure Set_Sequence_Item_Count          (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Sequence_Item_Count);
   procedure Create_Sequence                  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   function Get_Table_Layout_Ptr              (I: Item_Access'Class) return BR_Table_Layout_Ptr is (To_BR_Table_Layout_Ptr (I.Storage.Get_Unsigned_8_Ptr (I.Offset))); pragma Inline (Get_Table_Layout_Ptr);
   function Get_Column_Descriptor_Layout_Ptr  (I: Item_Access'Class; Column_Index: Unsigned_32) return BR_Table_Column_Name_Layout_Ptr; pragma Inline (Get_Column_Descriptor_Layout_Ptr);
   function Get_Column_Name_Layout_Ptr        (I: Item_Access'Class) return BR_Table_Column_Name_Layout_Ptr; pragma Inline (Get_Table_Layout_Ptr);
   procedure Create_Table                     (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);

   procedure Get_UUID           (I: Item_Access'Class; Value: out UUID); pragma Inline (Get_UUID);
   procedure Set_UUID           (I: Item_Access'Class; Value: UUID); pragma Inline (Set_UUID);
   procedure Create_UUID        (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID);

   function Get_Color           (I: Item_Access'Class) return Color; pragma Inline (Get_Color);
   procedure Set_Color          (I: Item_Access'Class; Value: Color); pragma Inline (Set_Color);
   procedure Create_Color       (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black);

   function Get_Family_Name_Byte_Count  (I: Item_Access'Class) return Unsigned_8; pragma Inline (Get_Family_Name_Byte_Count);
   function Get_Font_Name_Byte_Count    (I: Item_Access'Class) return Unsigned_8; pragma Inline (Get_Font_Name_Byte_Count);
   procedure Get_Family_Name            (I: Item_Access'Class; Value: out String); pragma Inline (Get_Family_Name);
   procedure Get_Font_Name              (I: Item_Access'Class; Value: out String); pragma Inline (Get_Font_Name);
   procedure Set_Font                   (I: Item_Access'Class; Value: Font); pragma Inline (Set_Font);
   procedure Create_Font                (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font);


   function Small_Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + 12);
   pragma Inline (Small_Value_Offset);

   function Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + 16 + Unsigned_32 (I.Header_Ptr.Name_Field_Byte_Count));
   pragma Inline (Value_Offset);

   function Get_Item_Small_Value_Ptr (I: Item_Access'Class) return Unsigned_8_Ptr is (I.Storage.Data.all(0)'Access + 12);
   pragma inline (Get_Item_Small_Value_Ptr);
   -- Sets the name of an item, does nothing if the name assistent is empty.
   -- Note: Does not check for any conditions, it will simply write to the proper indexes and update the Item Name_Field_Byte_Count.
   --
   procedure Set_Name (I: Item_Access'Class; Name: Item_Name_Assistent);

   function Create_Item_Access  (S: Storage_Area_Ptr; O: Unsigned_32) return Item_Access;

private

   type Item_Name_Assistent (String_Length: Unsigned_32) is tagged
      record
         CRC_16: Unsigned_16;
         Ascii_Code: Array_Of_Unsigned_8 (1 .. String_Length);
         Quick_Check: Unsigned_32;
         Name_Field_Byte_Count: Unsigned_8;
      end record;

   function Ascii_Code_Count (Assistent: Item_Name_Assistent) return Unsigned_8 is (Unsigned_8 (Assistent.Ascii_Code'Length));
   pragma Inline (Ascii_Code_Count);


end Item;

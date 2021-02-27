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


   -- All types available for storage into an Item_Manager.
   --
   type BR_Item_Type is (
                      BR_Null,         -- A null has no associated value, it simply exists.
                      BR_Bool,         -- Corresponding to Standard.Boolean.
                      BR_Int8,         -- An integer with a size of 8 bits (Byte, char).
                      BR_Int16,        -- An integer with a size of 16 bits.
                      BR_Int32,        -- An integer with a size of 32 bits.
                      BR_Int64,        -- An integer with a size of 64 bits.
                      BR_UInt8,        -- An integer with a size of 8 bits and range 0 .. 2**8-1.
                      BR_UInt16,       -- An integer with a size of 16 bits and range 0 .. 2**16-1.
                      BR_UInt32,       -- An integer with a size of 32 bits and range 0 .. 2**32-1.
                      BR_UInt64,       -- An integer with a size of 64 bits and range of 0 .. 2**64-1.
                      BR_Float32,      -- An IEEE 754 32 bit float. Accurate to about 6 decimals, range approx 1.1e-38 to 3.4e38.
                      BR_Float64,      -- An IEEE 754 64 bit float. Accurate to about 15 digits, range aprox 2.2e-308 to 1.7e+308.
                      BR_String,       -- Corresponds to Standard.String.
                      BR_CRC_String,   -- A string with an associated CRC-16 code for fast searches.
                      BR_Binary,       -- A series of bytes, corresponds to array (1..Count) of Br_UInt8.
                      BR_CRC_Binary,   -- A binary with associated CRC-16 code fro fast searches.
                      BR_Array,        -- An array of Item_Types.
                      BR_Dictionary,   -- A dictionary associates a key (string) with a value (Brbon.Item_Type).
                      BR_Sequence,     -- A sequence of Brbon.Item_Type's.
                      BR_Table,        -- A 2 dimension array of Brbon.Item_Type's addressed by column (string or index) and row (index).
                      BR_UUID,         -- A UUID, an array of 16 Br_UInt8 values returned as array or string.
                      BR_Color,        -- A RGBA (Red Green Blue Alpha) for color specifications.
                      BR_Font          -- A font specification (family name and font name).
                     );
   for BR_Item_Type'Size use 8;
   for BR_Item_Type use (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

   -- Minimum items sizes for items without a name, including the value length if the value length is fixed and assuming empty when the value size is variable.
   -- Always a multiple of 8.
   --
   Minimum_Item_Byte_Count: Array (BR_Item_Type'Range) of Unsigned_32 :=
     (
      BR_Null => 32,
      BR_Bool => 32,
      BR_Int8 => 32,
      BR_Int16 => 32,
      BR_Int32 => 32,
      BR_Int64 => 32 + 8,
      BR_UInt8 => 32,
      BR_UInt16 => 32,
      BR_UInt32 => 32,
      BR_UInt64 => 32 + 8,
      BR_Float32 => 32,
      BR_Float64 => 32 + 8,
      BR_String => 32 + 8,
      BR_CRC_String => 32 + 8,
      BR_Binary => 32 + 4,
      BR_CRC_Binary => 32 + 8,
      BR_Array => 32 + 16,
      BR_Sequence => 32 + 8,
      BR_Dictionary => 32 + 8,
      BR_Table => 32 + 16,
      BR_UUID => 32 + 16,
      BR_Color => 32,
      BR_Font => 32 + 8);

   -- A packed array of 8 bits, used for Options and Flags.
   --
   type Bits_8 is array (Integer range 0..7) of Boolean with Pack;
   --
   function To_Bits_8 is new Ada.Unchecked_Conversion (Unsigned_8, Bits_8);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Bits_8, Unsigned_8);


   -- Option associated with stored items (currently unused)
   --
   type Item_Options is new Bits_8;
   function To_Item_Options is new Ada.Unchecked_Conversion (Unsigned_8, Item_Options);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Options, Unsigned_8);
   No_Options : Item_Options := (False, False, False, False, False, False, False, False);


   -- Item flags for transitionary events to be recorded in an item (currently unused)
   --
   type Item_Flags is new Bits_8;
   function To_Item_Flags is new Ada.Unchecked_Conversion (Unsigned_8, Item_Flags);
   function To_Unsigned_8 is new Ada.Unchecked_Conversion (Item_Flags, Unsigned_8);
   No_Flags : Item_Flags := (False, False, False, False, False, False, False, False);

   -- Item Name
   --
   subtype Item_Name_Index is Integer range 1..245;
   package Item_Name_Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length (Item_Name_Index'Last);
   subtype Item_Name is Item_Name_Bounded_String.Bounded_String;
   --
   No_Name: constant Item_Name := Item_Name_Bounded_String.To_Bounded_String("");


   type Name_Assistent (String_Length: Unsigned_32) is private;

   function Create_Name_Assistent (S: Item_Name) return Name_Assistent;


   -- Item Access
   --
   type Item_Access is tagged private;


   -- ===============
   -- Creating Types
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

   function Get_String_Byte_Count      (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_String_Byte_Count);
   procedure Set_String_Byte_Count     (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_String_Byte_Count);
   procedure Set_String_Value          (I: Item_Access'Class; Value: String); pragma Inline (Set_String_Value);
   procedure Get_String                (I: Item_Access'Class; Value: out String); pragma Inline (Get_String);
   procedure Set_String                (I: Item_Access'Class; Value: String); pragma Inline (Set_String);
   procedure Create_String             (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");

   function Get_CRC_String_CRC         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_String_CRC);
   function Get_CRC_String_Byte_Count  (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_String_Byte_Count);
   procedure Set_CRC_String_CRC        (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_CRC_String_CRC);
   procedure Set_CRC_String_Byte_Count (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_CRC_String_Byte_Count);
   procedure Set_CRC_String_Value      (I: Item_Access'Class; Value: String); pragma Inline (Set_CRC_String_Value);
   procedure Set_CRC_String            (I: Item_Access'Class; Value: String); pragma Inline (Set_CRC_String);
   procedure Get_CRC_String            (I: Item_Access'Class; Value: out String); pragma Inline (Get_CRC_String);
   procedure Create_CRC_String         (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: String := "");

   function Get_Binary_Byte_Count      (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Binary_Byte_Count);
   procedure Set_Binary_Byte_Count     (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Binary_Byte_Count);
   procedure Set_Binary_Value          (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Set_Binary_Value);
   procedure Set_Binary                (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Set_Binary_Value);
   procedure Get_Binary                (I: Item_Access'Class; Value: out Array_Of_Unsigned_8); pragma Inline (Get_Binary);
   procedure Create_Binary             (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);

   function Get_CRC_Binary_CRC         (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_Binary_CRC);
   function Get_CRC_Binary_Byte_Count  (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_CRC_Binary_Byte_Count);
   procedure Set_CRC_Binary_CRC        (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_CRC_Binary_CRC);
   procedure Set_CRC_Binary_Byte_Count (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_CRC_Binary_CRC);
   procedure Set_CRC_Binary_Value      (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Set_CRC_Binary_Value);
   procedure Set_CRC_Binary            (I: Item_Access'Class; Value: Array_Of_Unsigned_8); pragma Inline (Set_CRC_Binary);
   procedure Get_CRC_Binary            (I: Item_Access'Class; Value: out Array_Of_Unsigned_8); pragma Inline (Get_CRC_Binary);
   procedure Create_CRC_Binary         (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);

   function Get_Array_Element_Type        (I: Item_Access'Class) return BR_Item_Type;
   function Get_Array_Element_Count       (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Array_Element_Count);
   function Get_Array_Element_Byte_Count  (I: Item_Access'Class) return Unsigned_32; pragma Inline (Get_Array_Element_Byte_Count);
   function Get_Array_Element_Offset      (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32; pragma Inline (Get_Array_Element_Offset);
   procedure Set_Array_Zeros              (I: Item_Access'Class); pragma Inline (Set_Array_Zeros);
   procedure Set_Array_Element_Count      (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Zeros);
   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32); pragma Inline (Set_Array_Element_Byte_Count);
   procedure Set_Array_Element_Type       (I: Item_Access'Class; Value: BR_Item_Type); pragma Inline (Set_Array_Element_Type);
   procedure Create_Array                (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool);

   procedure Create_Dictionary  (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);
   procedure Create_Sequence    (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);
   procedure Create_Table       (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0);
   procedure Create_UUID        (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID);
   procedure Create_Color       (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black);
   procedure Create_Font        (I: Item_Access'Class; Name: Item_Name := No_Name; Byte_Count: Unsigned_32 := 0; Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font);


   -- Sets the name of an item, does nothing if the name assistent is empty.
   -- Note: Does not check for any conditions, it will simply write to the proper indexes and update the Item Name_Field_Byte_Count.
   --
   procedure Set_Name (I: Item_Access'Class; Name: Name_Assistent);

   function Create_Item_Access  (S: Storage_Area_Ptr; O: Unsigned_32) return Item_Access;

private

   type Name_Assistent (String_Length: Unsigned_32) is tagged
      record
         CRC_16: Unsigned_16;
         Ascii_Code: Array_Of_Unsigned_8 (1 .. String_Length);
         Quick_Check: Unsigned_32;
         Name_Field_Byte_Count: Unsigned_8;
      end record;
   function Ascii_Code_Count (Assistent: Name_Assistent) return Unsigned_8 is (Unsigned_8 (Assistent.Ascii_Code'Length));
   pragma Inline (Ascii_Code_Count);

   -- Layout of an item header
   --
   type Item_Header is
      record
         Item_Type: BR_Item_Type;
         Options: Item_Options;
         Flags: Item_Flags;
         Name_Field_Byte_Count: Unsigned_8;
         Byte_Count: Unsigned_32;
         Parent_Offset: Unsigned_32;
         Small_Value: Unsigned_32;
      end record;
   --
   for Item_Header use
      record
         Item_Type             at 0  range 0..7;
         Options               at 1  range 0..7;
         Flags                 at 2  range 0..7;
         Name_Field_Byte_Count at 3  range 0..7;
         Byte_Count            at 4  range 0..31;
         Parent_Offset         at 8  range 0..31;
         Small_Value           at 12 range 0..31;
      end record;


   -- A pointer to an item layout
   --
   type Item_Header_Ptr is access Item_Header;
   --
   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_Ptr);


   -- Layout of the name field
   --
   type Item_Name_Field is
      record
         CRC_16: Unsigned_16;
         Ascii_Count: Unsigned_8;
      end record;
   --
   for Item_Name_Field use
      record
         CRC_16      at 0 range 0..15;
         Ascii_Count at 2 range 0..7;
      end record;
   --
   type Item_Name_Field_Ptr is access Item_Name_Field;
   --
   function To_Item_Name_Field_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Name_Field_ptr);
   --
   Item_Name_Field_Ascii_Code_Offset: Unsigned_32 := Item_Header'Size + Item_Name_Field'Size;


   type Item_Access is tagged
      record
         Storage: Storage_Area_Ptr;
         Offset: Unsigned_32;
      end record;

   function Header_Ptr (I: Item_Access) return Item_Header_Ptr is (To_Item_Header_Ptr (I.Storage.Get_Unsigned_8_Ptr (I.Offset)));
   pragma inline (Header_Ptr);

   function Get_Item_Name_Field_Offset (I: Item_Access) return Unsigned_32 is (I.Offset + Item_Header'Size);
   pragma inline (Get_Item_Name_Field_Offset);

   function Get_Item_Name_Field_Ptr (I: Item_Access) return Item_Name_Field_Ptr is (To_Item_Name_Field_Ptr (I.Storage.Get_Unsigned_8_Ptr (I.Get_Item_Name_Field_Offset)));
   pragma inline (Get_Item_Name_Field_Ptr);

   function Get_Item_Small_Value_Offset (I: Item_Access) return Unsigned_32 is (I.Offset + (Item_Header'Size - 4));
   pragma inline (Get_Item_Small_Value_Offset);

   function Get_Item_Small_Value_Ptr (I: Item_Access) return Unsigned_8_Ptr is (I.Storage.Data.all(0)'Access + I.Get_Item_Small_Value_Offset);
   pragma inline (Get_Item_Small_Value_Ptr);

   function Get_Item_Name_Field_Byte_Count (I: Item_Access) return Unsigned_32 is (Unsigned_32 (I.Header_Ptr.Name_Field_Byte_Count));
   pragma inline (Get_Item_Name_Field_Byte_Count);

   function Get_Item_Value_Offset (I: Item_Access) return Unsigned_32 is (I.Offset + Item_Header'Size + I.Get_Item_Name_Field_Byte_Count);
   pragma inline (Get_Item_Value_Offset);

   function Get_Item_Value_Ptr (I: Item_Access) return Unsigned_8_Ptr is (I.Storage.Data.all(0)'Access + I.Get_Item_Value_Offset);
   pragma inline (Get_Item_Value_Ptr);


   -- Returns true if the item type specification is valid.
   --
   function Is_Valid_Item_Type (Ptr: Unsigned_8_Ptr) return Boolean;


   -- Sets the type of an item.
   --
   procedure Item_Type (Ptr: Unsigned_8_Ptr; Value: BR_Item_Type);


   -- Returns the type of the item.
   -- Can raise Enum_Mapping_Failed.
   -- Use Is_Valid_Item_Type first to avoid raising the exception.
   --
   function Item_Type (Ptr: Unsigned_8_Ptr) return BR_Item_Type;


   -- Returns a pointer to the value area
   --
   function Get_Value_Area_Ptr (Ptr: Item_Header_Ptr) return Unsigned_8_Ptr;


   -- The name array
   --
   type Ascii_Array is array (Item_Name_Index range <>) of Unsigned_8;


   -- Layout of the name field
   --
   type Name_Field is
      record
         CRC: Unsigned_16;
         Byte_Count: Unsigned_8;
      end record;
   --
   for Name_Field use
      record
         CRC        at 0 range 0..15;
         Byte_Count at 2 range 0..7;
      end record;


   -- =======================
   -- Value access
   -- =======================


   -- String Value Access
   --
   --type String_Layout is
   --   record
   --      Byte_Count: Unsigned_32;
   --   end record;
   --
   --type String_Layout_Ptr is access String_Layout;
   --function To_String_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, String_Layout_Ptr);
   --
   --procedure Set_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: Ada.Strings.UTF_Encoding.UTF_8_String);
   --procedure Get_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Ada.Strings.UTF_Encoding.UTF_8_String);


   -- CRC String Value Access
   --
   --type CRC_String_Layout is
   --   record
   --      CRC_32: Unsigned_32;
   --      Byte_Count: Unsigned_32;
   --   end record;
   --
   --type CRC_String_Layout_Ptr is access CRC_String_Layout;
   --function To_CRC_String_Layout is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, CRC_String_Layout_Ptr);
   --
   --procedure Set_CRC_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: Ada.Strings.UTF_Encoding.UTF_8_String);
   --procedure Get_CRC_String_UTF_8_Code (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Ada.Strings.UTF_Encoding.UTF_8_String);


   -- Binary Value Access
   --
   --type Binary_Layout is
   --   record
   --      Byte_Count: Unsigned_32;
   --   end record;
   --
   --type Binary_Layout_Ptr is access Binary_Layout;
   --function To_Binary_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Binary_Layout_Ptr);
   --
   --procedure Set_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: Array_Of_Unsigned_8);
   --procedure Get_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Array_Of_Unsigned_8);


   -- CRC Binary Access
   --
   --type CRC_Binary_Layout is
   --   record
   --      CRC_32: Unsigned_32;
   --      Byte_Count: Unsigned_32;
   --   end record;
   --
   --type CRC_Binary_Layout_Ptr is access CRC_Binary_Layout;
   --function To_CRC_Binary_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, CRC_Binary_Layout_Ptr);
   --
   --procedure Set_CRC_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: Array_Of_Unsigned_8);
   --procedure Get_CRC_Binary_Data (Value_Area_Ptr: Unsigned_8_Ptr; Value: out Array_Of_Unsigned_8);


   -- Array Access
   --
   --type Array_Layout is
   --   record
   --      Reserved: Unsigned_32;
   --      Element_Type: BR_Item_Type;
   --      Zero_1: Unsigned_8;
   --      Zero_2: Unsigned_16;
   --      Element_Count: Unsigned_32;
   --      Element_Byte_Count: Unsigned_32;
   --   end record;
   --
   --for Array_Layout use
   --   record
   --      Reserved at 0 range 0..31;
   --      Element_Type at 4 range 0..7;
   --      Zero_1 at 5 range 0..7;
   --      Zero_2 at 6 range 0..15;
   --      Element_Count at 8 range 0..31;
   --      Element_Byte_Count at 12 range 0..31;
   --   end record;
   --
   --type Array_Layout_Ptr is access Array_Layout;
   --function To_Array_Layout_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Array_Layout_Ptr);
   --
   --procedure Set_Array_Element (Index: Unsigned_32; Bytes: Byte_Source);
   --procedure Get_Array_Element (Index: Unsigned_32; Consumer: Byte_Target);


end Item;

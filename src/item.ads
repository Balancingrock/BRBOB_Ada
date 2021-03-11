with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.UTF_Encoding;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Interfaces.C.Pointers;

with BRBON.Types; use BRBON.Types;
with BRBON.Container; use BRBON.Container;
with UUID_Package; use UUID_Package;
with Color_Package; use Color_Package;
with Font_Package; use Font_Package;
with Pointer_Math; use Pointer_Math;

-- Provides static access to the fields of an item in storage.
-- It does not provide checks, and it will not shift any data to accomodate updates.
-- It uses the access operation provided by the package Storage_Area and hence all accesses will respect the endianness of the Storage container.
--
package Item is


   -- Item header offsets and sizes
   -- All offsets are given from the start of the first byte in the header.
   --
   Item_Header_Offset_Type: constant Unsigned_32 := 0;                   -- 1 byte
   Item_Header_Offset_Options: constant Unsigned_32 := 1;                -- 1 byte
   Item_Header_Offset_Flags: constant Unsigned_32 := 2;                  -- 1 byte
   Item_Header_Offset_Name_Field_Byte_Count: constant Unsigned_32 := 3;  -- 1 byte
   Item_Header_Offset_Byte_Count: constant Unsigned_32 := 4;             -- 4 bytes
   Item_Header_Offset_Parent_Offset: constant Unsigned_32 := 8;          -- 4 bytes
   Item_Header_Offset_Small_Value: constant Unsigned_32 := 12;           -- 4 bytes
   Item_Header_Byte_Count: constant Unsigned_32 := 16;
   Item_Name_Field_Offset_CRC_16: constant Unsigned_32 := 16;            -- 2 bytes
   Item_Name_Field_Offset_Byte_Count: constant Unsigned_32 := 19;        -- 1 byte
   Item_Name_Field_Offset_ASCII_Code: constant Unsigned_32 := 20;        -- Up to 248 bytes.


   -- Used to determine the name field contents from a string.
   --
   type Item_Name_Assistent (String_Length: Unsigned_32) is private;


   -- Create all name field components from a given string.
   --
   function Create_Item_Name_Assistent (S: BR_Item_Name) return Item_Name_Assistent;


   -- Item Access
   --
   type Item_Access is tagged
      record
         Storage: Binary_Store_Ptr; -- Pointer to the storage area itself
         Offset: Unsigned_32;       -- Offset to the first header byte in the storage area
      end record;


   -- =========================
   -- Offsets for value access
   -- =========================

   -- Offset of the small value field from the start of the storage area of the item designated by I.
   --
   function Small_Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + Item_Header_Offset_Small_Value);
   pragma Inline (Small_Value_Offset);


   -- Offset of the nominal value field from the start of the storage area of the item designated by I.
   -- Note that the presence of a name field is reflected in this offset.
   --
   function Value_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Offset + Item_Header_Offset_Byte_Count + Unsigned_32 (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Offset_Byte_Count)));
   pragma Inline (Value_Offset);


   -- ======================
   -- Header common access
   -- ======================

   -- Set the type of the item accessed through I.
   --
   procedure Assign_Item_Type (I: Item_Access'Class; Value: BR_Item_Type);
   pragma Inline (Assign_Item_Type);

   -- Returns true if the item type of the item access through I is a valid type code.
   -- Use this operation to verify that a 'Item_Type' operation will not raise an Illegal_Item_Type exception.
   --
   function Is_Valid_Item_Type (I: Item_Access'Class) return Boolean is (I.Storage.Valid_Item_Type (I.Offset + Item_header_Offset_Type));
   pragma Inline (Is_Valid_Item_Type);

   -- Returns the type of the item accessed through I.
   -- Can raise the Illegal_Item_Type exception of the byte code cannot be mapped to a valid item type byte code.
   -- Use the operation 'Is_Valid_Item_Type' to ensure that no exception will be raised.
   --
   function Item_Type (I: Item_Access'Class) return BR_Item_Type is (I.Storage.Get_Item_Type (I.Offset + Item_Header_Offset_Type));
   pragma Inline (Item_Type);

   -- Set the item option field of the item accessed through I.
   -- Note: Currently not used, should always be zero to ensure future compatibility.
   --
   procedure Assign_Item_Options (I: Item_Access'Class; Value: BR_Item_Options);
   pragma Inline (Assign_Item_Options);

   -- Return the item options of the item accessed through I.
   -- Note: Currently not used, assume zero to ensure future compatibility.
   --
   function Item_Options (I: Item_Access'Class) return BR_Item_Options is (I.Storage.Get_Item_Options (I.Offset + Item_Header_Offset_Options));
   pragma Inline (Item_Options);

   -- Set the item flags of the item accessed though I.
   -- This field should not be used through the api. For internal use only.
   --
   procedure Assign_Item_Flags (I: Item_Access'Class; Value: BR_Item_Flags);
   pragma Inline (Assign_Item_Flags);

   -- Return the item flags of the item accessed through I.
   --
   function Item_Flags (I: Item_Access'Class) return BR_Item_Flags is (I.Storage.Get_Item_Flags (I.Offset + Item_header_Offset_Flags));
   pragma Inline (Item_Flags);

   -- Set the name field byte count of the item accessed through I.
   --
   procedure Assign_Item_Name_Field_Byte_Count (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Assign_Item_Name_Field_Byte_Count);

   -- Return the name field byte count.
   -- Note this is not the length of the string, but the length of the field the string is stored in.
   --
   function Item_Name_Field_Byte_Count (I: Item_Access'Class) return Unsigned_8 is (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Offset_Byte_Count));
   pragma Inline (Item_Name_Field_Byte_Count);

   --Set the byte count of the item itself (includes everything) that is access through I.
   --
   procedure Assign_Item_Byte_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Assign_Item_Byte_Count);

   -- Returns the total byte count of the item referenced through I.
   --
   function Item_Byte_Count (I: Item_Access'Class) return Unsigned_32 is (I.Storage.Get_Unsigned_32 (I.Offset + Item_Header_Offset_Byte_Count));
   pragma Inline (Item_Byte_Count);

   -- Sets the parent offset of the item referenced through I.
   --
   procedure Assign_Item_Parent_Offset (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Assign_Item_Parent_Offset);

   -- Return the offset of the parent of the item referenced through I.
   -- Note: This offset can be the offset of I itself if there is no parent.
   --
   function Item_Parent_Offset (I: Item_Access'Class) return Unsigned_32 is (I.Storage.Get_Unsigned_32 (I.Offset + Item_Header_Offset_Parent_Offset));
   pragma Inline (Item_Parent_Offset);


   -- ======================
   -- Item name access
   -- ======================

   -- Returns the CRC-16 from the name field associated with the item referenced through I.
   --
   function Item_Name_CRC (I: Item_Access'Class) return Unsigned_16 is
     (I.Storage.Get_Unsigned_16 (I.Offset + Item_Name_Field_Offset_CRC_16));
   pragma Inline (Item_Name_CRC);

   -- Returns the length (in bytes) of the ASCII sequence of the item name of the item referenced through I (as stored in the name field).
   --
   function Item_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Offset + Item_Name_Field_Offset_Byte_Count));
   pragma Inline (Item_Name_CRC);

   -- Return the item name of the item referenced through I.
   --
   function Item_Name (I: Item_Access'Class) return String is
     (I.Storage.Get_String (I.Offset + Item_Name_Field_Offset_ASCII_Code, Unsigned_32 (I.Item_Name_Byte_Count)));
   pragma Inline (Item_Name);

   -- Assigns the given name to the item referenced through I.
   -- Note that the name field byte count will be updated as necessary, but it is assumed that this is possible without the need to shift data around.
   --
   procedure Assign_Item_Name (I: Item_Access'Class; Value: Item_Name_Assistent);



   -- ===============
   -- BR_Null
   -- ===============

   -- Creates a null-item starting at the offset given in I.
   --
   procedure Create_Null_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Null); Parent_Offset: Unsigned_32 := 0);


   -- ================
   -- BR_Bool
   -- ================

   -- Returns the boolean value contained in the item referenced through I.
   --
   function Boolean_Value (I: Item_Access'Class) return Boolean is
     (I.Storage.Get_Bool (I.Small_Value_Offset));
   pragma Inline (Boolean_Value);

   -- Sets the boolean value in the item referenced through I.
   --
   procedure Assign_Boolean_Value (I: Item_Access'Class; Value: Boolean);
   pragma Inline (Assign_Boolean_Value);

   -- Creates a boolean item starting at the offset given in I.
   --
   procedure Create_Boolean_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Bool); Parent_Offset: Unsigned_32 := 0; Value: Boolean := False);


   -- ===============
   -- BR_Int8
   -- ===============

   -- Return the integer_8 value of the item referenced through I.
   --
   function Integer_8_Value (I: Item_Access'Class) return Integer_8 is
     (I.Storage.Get_Integer_8 (I.Small_Value_Offset));
   pragma Inline (Integer_8_Value);

   -- Set the integer_8 value of the item referenced through I.
   --
   procedure Assign_Integer_8_Value (I: Item_Access'Class; Value: Integer_8);
   pragma Inline (Assign_Integer_8_Value);

   -- Create an integer_8 item starting at the offset given in I.
   --
   procedure Create_Integer_8_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int8); Parent_Offset: Unsigned_32 := 0; Value: Integer_8 := 0);


   -- ===============
   -- BR_Int16
   -- ===============

   -- Return the integer_16 value of the item referenced through I.
   --
   function Integer_16_Value (I: Item_Access'Class) return Integer_16 is
      (I.Storage.Get_Integer_16 (I.Small_Value_Offset));
   pragma Inline (Integer_16_Value);

   -- Set the integer_16 value of the item referenced through I.
   --
   procedure Assign_Integer_16_Value (I: Item_Access'Class; Value: Integer_16);
   pragma Inline (Assign_Integer_16_Value);

   -- Create an integer_16 item starting at the offset given in I.
   --
   procedure Create_Integer_16_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int16); Parent_Offset: Unsigned_32 := 0; Value: Integer_16 := 0);


   -- ================
   -- BR_Int32
   -- ================

   -- Return the integer_32 value of the item referenced through I.
   --
   function Integer_32_Value (I: Item_Access'Class) return Integer_32 is
      (I.Storage.Get_Integer_32 (I.Small_Value_Offset));
   pragma Inline (Integer_32_Value);

   -- Set the integer_32 value of the item referenced through I.
   --
   procedure Assign_Integer_32_Value (I: Item_Access'Class; Value: Integer_32);
   pragma Inline (Assign_Integer_32_Value);

   -- Create an integer_32 item starting at the offset given in I.
   --
   procedure Create_Integer_32_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int32); Parent_Offset: Unsigned_32 := 0; Value: Integer_32 := 0);


   -- =================
   -- BR_Int64
   -- =================

   -- Return the integer_64 value of the item referenced through I.
   --
   function Integer_64_Value (I: Item_Access'Class) return Integer_64 is
      (I.Storage.Get_Integer_64 (I.Value_Offset));
   pragma Inline (Integer_64_Value);

   -- Set the integer_64 value of the item referenced through I.
   --
   procedure Assign_Integer_64_Value (I: Item_Access'Class; Value: Integer_64);
   pragma Inline (Assign_Integer_64_Value);

   -- Create an integer_64 item starting at the offset given in I.
   --
   procedure Create_Integer_64_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Int64); Parent_Offset: Unsigned_32 := 0; Value: Integer_64 := 0);


   -- =================
   -- BR_UInt8
   -- =================

   -- Return the unsigned_8 value of the item referenced through I.
   --
   function Unsigned_8_Value (I: Item_Access'Class) return Unsigned_8 is
      (I.Storage.Get_Unsigned_8 (I.Small_Value_Offset));
   pragma Inline (Unsigned_8_Value);

   -- Set the unsigned_8 value of the item referenced through I.
   --
   procedure Assign_Unsigned_8_Value (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Assign_Unsigned_8_Value);

   -- Create an unsigned_8 item starting at the offset given in I.
   --
   procedure Create_Unsigned_8_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt8); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_8 := 0);


   -- =================
   -- BR_UInt16
   -- =================

   -- Return the unsigned_16 value of the item referenced through I.
   --
   function Unsigned_16_Value (I: Item_Access'Class) return Unsigned_16 is
      (I.Storage.Get_Unsigned_16 (I.Small_Value_Offset));
   pragma Inline (Unsigned_16_Value);

   -- Set the unsigned_16 value of the item referenced through I.
   --
   procedure Assign_Unsigned_16_Value (I: Item_Access'Class; Value: Unsigned_16);
   pragma Inline (Assign_Unsigned_16_Value);

   -- Create an unsigned_16 item starting at the offset given in I.
   --
   procedure Create_Unsigned_16_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt16); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_16 := 0);


   -- =================
   -- BR_UInt32
   -- =================

   -- Return the unsigned_32 value of the item referenced through I.
   --
   function Unsigned_32_Value (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Small_Value_Offset));
   pragma Inline (Unsigned_32_Value);

   -- Set the unsigned_32 value of the item referenced through I.
   --
   procedure Assign_Unsigned_32_Value (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Assign_Unsigned_32_Value);

   -- Create an unsigned_16 item starting at the offset given in I.
   --
   procedure Create_Unsigned_32_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt32); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_32 := 0);


   -- =================
   -- BR_UInt64
   -- =================

   -- Return the unsigned_64 value of the item referenced through I.
   --
   function Unsigned_64_Value (I: Item_Access'Class) return Unsigned_64 is
      (I.Storage.Get_Unsigned_64 (I.Value_Offset));
   pragma Inline (Unsigned_64_Value);

   -- Set the unsigned_64 value of the item referenced through I.
   --
   procedure Assign_Unsigned_64_Value (I: Item_Access'Class; Value: Unsigned_64);
   pragma Inline (Assign_Unsigned_64_Value);

   -- Create an unsigned_64 item starting at the offset given in I.
   --
   procedure Create_Unsigned_64_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UInt64); Parent_Offset: Unsigned_32 := 0; Value: Unsigned_64 := 0);


   -- =================
   -- BR_Float32
   -- =================

   -- Return the float_32 value of the item referenced through I.
   --
   function Float_32_Value (I: Item_Access'Class) return IEEE_Float_32 is
      (I.Storage.Get_Float_32 (I.Small_Value_Offset));
   pragma Inline (Float_32_Value);

   -- Set the float_32 value of the item referenced through I.
   --
   procedure Assign_Float_32_Value (I: Item_Access'Class; Value: IEEE_Float_32);
   pragma Inline (Assign_Float_32_Value);

   -- Create an float_64 item starting at the offset given in I.
   --
   procedure Create_Float_32_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Float32); Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_32 := 0.0);


   -- =================
   -- BR_Float64
   -- =================

   -- Return the float_64 value of the item referenced through I.
   --
   function Float_64_Value (I: Item_Access'Class) return IEEE_Float_64 is
      (I.Storage.Get_Float_64 (I.Value_Offset));
   pragma Inline (Float_64_Value);

   -- Set the float_32 value of the item referenced through I.
   --
   procedure Assign_Float_64_Value (I: Item_Access'Class; Value: IEEE_Float_64);
   pragma Inline (Assign_Float_64_Value);

   -- Create an float_64 item starting at the offset given in I.
   --
   procedure Create_Float_64_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Float64); Parent_Offset: Unsigned_32 := 0; Value: IEEE_Float_64 := 0.0);



   -- =================
   -- BR_String
   -- =================

   -- Offset of the byte-count of the string (in the value area)
   --
   String_Value_Offset_Byte_Count: constant Unsigned_32 := 0; -- 4 bytes

   -- Offset of the start of the utf8 code (in the value area)
   --
   String_Value_Offset_UTF8_Code: constant Unsigned_32 := 4; -- N bytes

   -- Return the number of bytes in the string, note that this does not need to be equal to the number of characters.
   --
   function String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + String_Value_Offset_Byte_Count));
   pragma Inline (String_Byte_Count);

   -- Return the string value of the item referenced through I.
   --
   function String_Value (I: Item_Access'Class) return String is
      (I.Storage.Get_String (I.Value_Offset + String_Value_Offset_UTF8_Code, I.String_Byte_Count));
   pragma Inline (String_Value);

   -- Set the float_32 value of the item referenced through I.
   --
   procedure Assign_String_Value (I: Item_Access'Class; Value: String);
   pragma Inline (Assign_String_Value);

   -- Create a string item starting at the offset given in I.
   --
   procedure Create_String_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_String); Parent_Offset: Unsigned_32 := 0; Value: String := "");


   -- =================
   -- BR_CRC_String
   -- =================

   -- Offset of the CRC-16 of the string (in the value area)
   --
   CRC_String_Value_Offset_CRC: constant Unsigned_32 := 0; -- 4 bytes

   -- Offset of the byte-count of the string (in the value area)
   --
   CRC_String_Value_Offset_Byte_Count: constant Unsigned_32 := 4; -- 4 bytes

   -- Offset of the start of the utf8 code (in the value area)
   --
   CRC_String_Value_Offset_UTF8_Code: constant Unsigned_32 := 8; -- N bytes

   -- Return the CRC-32 of the item referenced through I.
   --
   function CRC_String_CRC (I: Item_Access'Class) return Unsigned_32 is
     (I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_String_Value_Offset_CRC));
   pragma Inline (CRC_String_CRC);

   -- Return the string count of the item referenced through I.
   --
   function CRC_String_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
     (I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_String_Value_Offset_Byte_Count));
   pragma Inline (CRC_String_Byte_Count);

   -- Return the string value of the item referenced through I.
   --
   function CRC_String_Value (I: Item_Access'Class) return String is
      (I.Storage.Get_String (I.Value_Offset + CRC_String_Value_Offset_UTF8_Code, I.CRC_String_Byte_Count));
   pragma Inline (CRC_String_Value);

   -- Assign a new string to the item referenced through I.
   -- This will also update the associated CRC and byte-count.
   --
   procedure Assign_CRC_String_Value (I: Item_Access'Class; Value: String);
   pragma Inline (Assign_CRC_String_Value);

   -- Create a CRC string item starting at the offset given in I.
   --
   procedure Create_CRC_String_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_CRC_String); Parent_Offset: Unsigned_32 := 0; Value: String := "");


   -- =================
   -- BR_Binary
   -- =================

   -- Offset of the byte-count of the array (in the value area)
   --
   Binary_Value_Offset_Byte_Count: constant Unsigned_32 := 0; -- 4 bytes

   -- Offset of the start of the binary byte array (in the value area)
   --
   Binary_Value_Offset_Bytes: constant Unsigned_32 := 4; -- byte-count bytes

   -- Return the byte count of the array referenced through I.
   --
   function Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Binary_Value_Offset_Byte_Count));
   pragma Inline (Binary_Byte_Count);

   -- Returns the binary of the item referenced through I.
   --
   function Binary_Value (I: Item_Access'Class) return Array_Of_Unsigned_8 is
      (I.Storage.Get_Unsigned_8_Array (I.Value_Offset + Binary_Value_Offset_Bytes, I.Binary_Byte_Count));
   pragma Inline (Binary_Value);

   -- Assigns a new binary to the item referenced through I.
   -- This will also update the associated byte-count.
   --
   procedure Assign_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8);
   pragma Inline (Assign_Binary_Value);

   -- Create a binary item starting at the offset given in I.
   --
   procedure Create_Binary_Item  (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Binary); Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);


   -- =====================
   -- BR_CRC_Binary
   -- =====================

   -- Offset from the start of the CRC Binary item value area.
   --
   CRC_Binary_Value_Offset_CRC: constant Unsigned_32 := 0;       -- 4 bytes
   CRC_Binary_Value_Offset_Byte_Count: constant Unsigned_32 := 4; -- 4 bytes
   CRC_Binary_Value_Offset_Bytes: constant Unsigned_32 := 8;      -- byte-count bytes

   -- Returns the CRC of the item referenced through I.
   --
   function CRC_Binary_CRC (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_Offset_CRC));
   pragma Inline (CRC_Binary_CRC);

   -- Returns the byte-count of the item referenced through I.
   --
   function CRC_Binary_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + CRC_Binary_Value_Offset_Byte_Count));
   pragma Inline (CRC_Binary_Byte_Count);

   -- Returns the binary if the item referenced through I.
   --
   function CRC_Binary_Value (I: Item_Access'Class) return Array_Of_Unsigned_8 is
      (I.Storage.Get_Unsigned_8_Array (I.Value_Offset + CRC_Binary_Value_Offset_Bytes, I.CRC_Binary_Byte_Count));
   pragma Inline (CRC_Binary_Value);

   -- Assigns a new binary to the item referenced through I.
   -- This will also update the associated byte-count and CRC values.
   --
   procedure Assign_CRC_Binary_Value (I: Item_Access'Class; Value: Array_Of_Unsigned_8);
   pragma Inline (Assign_CRC_Binary_Value);

   -- Create a CRC-binary item starting at the offset given in I.
   --
   procedure Create_CRC_Binary_Item   (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_CRC_Binary); Parent_Offset: Unsigned_32 := 0; Value: Array_Of_Unsigned_8 := Short_Array_Of_Unsigned_8);


   -- ====================
   -- BR_Array
   -- ====================

   -- Offset from the start of the array item value area.
   --
   Array_Value_Offset_Reserved_32: constant Unsigned_32 := 0;          -- 4 bytes
   Array_Value_Offset_Element_Type: constant Unsigned_32 := 4;         -- 1 bytes
   Array_Value_Offset_Reserved_8: constant Unsigned_32 := 5;           -- 1 byte
   Array_Value_Offset_Reserved_16: constant Unsigned_32 := 6;          -- 2 bytes
   Array_Value_Offset_Element_Count: constant Unsigned_32 := 8;        -- 4 bytes
   Array_Value_Offset_Element_Byte_Count: constant Unsigned_32 := 12;  -- 4 bytes
   Array_Value_Offset_Element_Base: constant Unsigned_32 := 16;        -- Count * Byte_Count bytes

   -- Returns the type of the element stored in the array of the item referenced by I.
   --
   function Array_Element_Type (I: Item_Access'Class) return BR_Item_Type is
      (I.Storage.Get_Item_Type (I.Value_Offset + Array_Value_Offset_Element_Type));
   pragma Inline (Array_Element_Type);

   -- Returns the number of elements in the array of the item referenced by I.
   --
   function Array_Element_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Array_Value_Offset_Element_Count));
   pragma Inline (Array_Element_Count);

   -- Returns the byte count of each element in the array of the item referenced by I.
   --
   function Array_Element_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Array_Value_Offset_Element_Byte_Count));
   pragma Inline (Array_Element_Byte_Count);

   -- Returns the offset of an element in the array of the item referenced by I.
   --
   function Array_Element_Offset (I: Item_Access'Class; Index: Unsigned_32) return Unsigned_32 is
     (I.Value_Offset + Array_Value_Offset_Element_Base + (I.Array_Element_Count - 1) * I.Array_Element_Byte_Count);
   pragma Inline (Array_Element_Offset);

   -- Set the number of elements in the array of the item referenced by I.
   --
   procedure Set_Array_Element_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Array_Element_Count);

   -- Set the byte count of the elements in the array of the item referenced by I.
   --
   procedure Set_Array_Element_Byte_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Array_Element_Byte_Count);

   -- Creates a new array item at the offset given in I.
   --
   procedure Create_Array_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Array); Parent_Offset: Unsigned_32 := 0; Element_Type: BR_Item_Type := BR_Bool; Element_Byte_Count: Unsigned_32 := 128);


   -- =====================
   -- BR_Dictionary
   -- =====================

   -- Offset from the start of the dictionary item value area.
   --
   Dictionary_Value_Offset_Reserved_32: constant Unsigned_32 := 0;   -- 4 bytes
   Dictionary_Value_Offset_Count: constant Unsigned_32 := 4;         -- 4 bytes
   Dictionary_Value_Offset_Items_Base: constant Unsigned_32 := 8;    -- N bytes.

   -- Returns the number of items in the dictionary item referenced by I.
   --
   function Dictionary_Item_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Dictionary_Value_Offset_Count));
   pragma Inline (Dictionary_Item_Count);

   -- Sets the number of items in the dictionary referenced through I.
   --
   procedure Set_Dictionary_Item_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Dictionary_Item_Count);

   -- Creates a new dictionary item at the offset given in I.
   --
   procedure Create_Dictionary_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Dictionary); Parent_Offset: Unsigned_32 := 0);


   -- =====================
   -- BR_Sequence
   -- =====================

   -- Offset from the start of the sequence item value area.
   --
   Sequence_Value_Offset_Reserved_32: constant Unsigned_32 := 0;   -- 4 bytes
   Sequence_Value_Offset_Count: constant Unsigned_32 := 4;         -- 4 bytes
   Sequence_Value_Offset_Items_Base: constant Unsigned_32 := 8;    -- N bytes.

   -- Returns the number of items in the sequence item referenced by I.
   --
   function Sequence_Item_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Sequence_Value_Offset_Count));
   pragma Inline (Sequence_Item_Count);

   -- Sets the number of items in the sequence referenced through I.
   --
   procedure Set_Sequence_Item_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Sequence_Item_Count);

   -- Creates a new sequence item at the offset given in I.
   --
   procedure Create_Sequence_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Sequence); Parent_Offset: Unsigned_32 := 0);


   -- =================
   -- BR_Table
   -- =================

   -- Offset from the start of the table item value area.
   --
   Table_Value_Offset_Row_Count: constant Unsigned_32 := 0;                   -- 4 bytes
   Table_Value_Offset_Column_Count: constant Unsigned_32 := 4;                -- 4 bytes
   Table_Value_Offset_Fields_Offset: constant Unsigned_32 := 8;               -- 4 bytes
   Table_Value_Offset_Row_Byte_Count: constant Unsigned_32 := 12;             -- 4 bytes
   Table_Value_Offset_Column_Descriptor_Base: constant Unsigned_32 := 16;     -- N bytes

   -- Offsets from the start of a table descriptor
   --
   Column_Descriptor_Offset_Name_CRC_16: constant Unsigned_32 := 0;           -- 2 bytes
   Column_Descriptor_Offset_Name_Field_Byte_Count: constant Unsigned_32 := 2; -- 1 byte
   Column_Descriptor_Offset_Field_Type: constant Unsigned_32 := 3;            -- 1 byte
   Column_Descriptor_Offset_Name_Field_Offset: constant Unsigned_32 := 4;     -- 4 bytes
   Column_Descriptor_Offset_Field_Offset: constant Unsigned_32 := 8;          -- 4 bytes
   Column_Descriptor_Offset_Field_Byte_Count: constant Unsigned_32 := 12;     -- 4 bytes

   -- The number of bytes in a table descriptor.
   --
   Column_Descriptor_Byte_Count: constant Unsigned_32 := 16;

   -- Offsets from the start of a column name field.
   --
   Column_Name_Field_Offset_ASCII_Byte_Count: constant Unsigned_32 := 0;      -- 1 byte
   Column_Name_Field_Offset_Reserved_8: constant Unsigned_32 := 1;            -- 1 byte
   Column_Name_Field_Offset_Reserved_16: constant Unsigned_32 := 2;           -- 2 byte
   Column_Name_Field_Offset_ASCII_Code: constant Unsigned_32 := 4;            -- N bytes

   -- The number of rows in the table referenced by I.
   --
   function Table_Row_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Row_Count));
   pragma Inline (Table_Row_Count);

   -- Sets the number of rows in the table referenced by I.
   --
   procedure Set_Table_Row_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Table_Row_Count);

   -- The number of columns in the table referenced by I.
   --
   function Table_Column_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Column_Count));
   pragma Inline (Table_Column_Count);

   -- Sets the number of columns in the table referenced by I.
   --
   procedure Set_Table_Column_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Table_Column_Count);

   -- Returns the offset of the field of first column in the first row of the table referenced by I.
   --
   function Table_Fields_Offset (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Fields_Offset));
   pragma Inline (Table_Fields_Offset);

   -- Sets the offset of the first field of the first column in the first row of the table referenced by I.
   --
   procedure Set_Table_Fields_Offset (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Table_Fields_Offset);

   -- Returns the byte count of a single row in the table referenced by I.
   --
   function Table_Row_Byte_Count (I: Item_Access'Class) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Value_Offset + Table_Value_Offset_Row_Byte_Count));
   pragma Inline (Table_Row_Byte_Count);

   -- Sets the byte count for a single row in the table referenced by I.
   --
   procedure Set_Table_Row_Byte_Count (I: Item_Access'Class; Value: Unsigned_32);
   pragma Inline (Set_Table_Row_Byte_Count);

   -- Returns the offset for a table descriptor in the table referenced by I.
   --
   function Table_Column_Descriptor_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
      (I.Value_Offset + Table_Value_Offset_Column_Descriptor_Base + (Column_Index - 1) * Column_Descriptor_Byte_Count);
   pragma Inline (Table_Column_Descriptor_Offset);

   -- Returns the CRC-16 of a column name in the table referenced by I.
   --
   function Column_Name_CRC (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_16 is
      (I.Storage.Get_Unsigned_16 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Name_CRC_16));
   pragma Inline (Column_Name_CRC);

   -- Sets the CRC-16 of a column name in the table referenced by I.
   --
   procedure Set_Column_Name_CRC (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Column_Name_CRC);

   -- Returns the byte-count of a column name field in the table referenced by I.
   --
   function Column_Name_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Name_Field_Byte_Count));
   pragma Inline (Column_Name_Field_Byte_Count);

   -- Sets the byte-count of a column name field in the table referenced by I.
   --
   procedure Set_Column_Name_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_16);
   pragma Inline (Set_Column_Name_Field_Byte_Count);

   -- Returns the item type of a column in the table referenced by I.
   --
   function Column_Type (I: Item_Access'Class; Column_Index: Unsigned_32) return BR_Item_Type is
      (I.Storage.Get_Item_Type (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Field_Type));
   pragma Inline (Column_Type);

   -- Sets the item type of a column in the table referenced by I.
   --
   procedure Set_Column_Type (I: Item_Access'Class; Column_Index: Unsigned_32; Value: BR_Item_Type);
   pragma Inline (Set_Column_Type);

   -- Returns the name field offset of a column in the table referenced by I
   --
   function Column_Name_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Name_Field_Offset));
   pragma Inline (Column_Name_Field_Offset);

   -- Gets the name field offset of a column in the table referenced by I
   --
   procedure Set_Column_Name_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Column_Name_Field_Offset);

   -- Returns the field offset within in a row in the table referenced by I.
   --
   function Column_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Field_Offset));
   pragma Inline (Column_Field_Offset);

   -- Sets the field offset within in a row in the table referenced by I.
   --
   procedure Set_Column_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Column_Field_Offset);

   -- Returns the column field byte-count in the table referenced by I.
   --
   function Column_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
      (I.Storage.Get_Unsigned_32 (I.Table_Column_Descriptor_Offset (Column_Index) + Column_Descriptor_Offset_Field_Byte_Count));
   pragma Inline (Column_Field_Byte_Count);

   -- Returns the column field byte-count in the table referenced by I.
   --
   procedure Set_Column_Field_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Column_Field_Byte_Count);

   -- Returns the offset of the name field of a column in the table referenced by I.
   --
   function Column_Name_Field_Base_Offset (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_32 is
     (I.Value_Offset + I.Column_Name_Field_Offset (Column_Index));
   pragma Inline (Column_Name_Field_Base_Offset);

   -- Returns the byte count of a column name in the table referenced by I.
   --
   function Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32) return Unsigned_8 is
      (I.Storage.Get_Unsigned_8 (I.Column_Name_Field_Base_Offset (Column_Index) + Column_Name_Field_Offset_ASCII_Byte_Count));
   pragma Inline (Column_Name_Byte_Count);

   -- Sets the byte count of a column name in the table referenced by I.
   --
   procedure Set_Column_Name_Byte_Count (I: Item_Access'Class; Column_Index: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Column_Name_Byte_Count);

   -- Returns the name of a column in the table referenced by I.
   --
   function Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32) return String is
     (I.Storage.Get_String (I.Column_Name_Field_Base_Offset (Column_Index) + Column_Name_Field_Offset_ASCII_Code, Unsigned_32 (I.Column_Name_Byte_Count (Column_Index))));
   pragma Inline (Column_Name);

   -- Sets the name of a column in the table referenced by I.
   --
   procedure Set_Column_Name (I: Item_Access'Class; Column_Index: Unsigned_32; Value: String);
   pragma Inline (Set_Column_Name);

   -- Returns the offset for column field in a row in the table referenced by I.
   --
   function Table_Field_Offset (I: Item_Access'Class; Column_Index: Unsigned_32; Row_Index: Unsigned_32) return Unsigned_32 is
      (I.Table_Fields_Offset + I.Table_Row_Byte_Count * (Row_Index - 1) + I.Column_Field_Offset (Column_index));
   pragma Inline (Table_Field_Offset);

   -- Creates a new table item at the offset given in I.
   --
   procedure Create_Table_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Table); Parent_Offset: Unsigned_32 := 0; Column_Specifications: Array_Of_Table_Column_Specification);


   -- ===================
   -- BR_UUID
   -- ===================

   -- Return the UUID value if the item referenced by I.
   --
   function UUID_Value (I: Item_Access'Class) return UUID is
      (UUID (I.Storage.Get_Unsigned_8_Array (I.Value_Offset, 16)));
   pragma Inline (UUID_Value);

   -- Set a new UUID value in the item referenced by I.
   --
   procedure Assign_UUID_Value (I: Item_Access'Class; Value: UUID);
   pragma Inline (Assign_UUID_Value);

   -- Creates a new UUID item at the offset given in I.
   --
   procedure Create_UUID_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_UUID); Parent_Offset: Unsigned_32 := 0; Value: UUID := Null_UUID);


   -- ===================
   -- BR_Color
   -- ===================

   -- Offset from the start of the color item value area.
   --
   Color_Small_Value_Offset_Red: constant Unsigned_32 := 0;
   Color_Small_Value_Offset_Green: constant Unsigned_32 := 1;
   Color_Small_Value_Offset_Blue: constant Unsigned_32 := 2;
   Color_Small_Value_Offset_Alpha: constant Unsigned_32 := 3;

   -- Return the Color value if the item referenced by I.
   --
   function Color_Value (I: Item_Access'Class) return Color;
   pragma Inline (Color_Value);

   -- Return the red component of the item referenced by I.
   --
   function Red (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Red));
   pragma Inline (Red);

   -- Set the red component of the item referenced by I.
   --
   procedure Set_Red (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Set_Red);

   -- Return the green component of the item referenced by I.
   --
   function Green (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Green));
   pragma Inline (Green);

   -- Set the green component of the item referenced by I.
   --
   procedure Set_Green (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Set_Green);

   -- Return the blue component of the item referenced by I.
   --
   function Blue (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Blue));
   pragma Inline (Blue);

   -- Set the blue component of the item referenced by I.
   --
   procedure Set_Blue (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Set_Blue);

   -- Return the alpha component of the item referenced by I.
   --
   function Alpha (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Small_Value_Offset + Color_Small_Value_Offset_Alpha));
   pragma Inline (Alpha);

   -- Set the alpha component of the item referenced by I.
   --
   procedure Set_Alpha (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Set_Alpha);

   -- Set a new color value in the item referenced by I.
   --
   procedure Assign_Color_Value (I: Item_Access'Class; Value: Color);
   pragma Inline (Assign_Color_Value);

   -- Creates a new color item at the offset given in I.
   --
   procedure Create_Color_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Color); Parent_Offset: Unsigned_32 := 0; Value: Color := Color_Black);


   -- ===================
   -- BR_Font
   -- ===================

   -- Offset from the start of the color item value area.
   --
   Font_Value_Offset_Points: constant Unsigned_32 := 0;
   Font_Value_Offset_Family_Name_Byte_Count: constant Unsigned_32 := 4;
   Font_Value_Offset_Font_Name_Byte_Count: constant Unsigned_32 := 5;
   Font_Value_Offset_Family_Name_String: constant Unsigned_32 := 6;

   -- Returns the font size for the item referenced by I.
   --
   function Points (I: Item_Access'Class) return IEEE_Float_32 is
     (I.Storage.Get_Float_32 (I.Value_Offset + Font_Value_Offset_Points));
   pragma Inline (Points);

   -- Set the font size for the item referenced by I.
   --
   procedure Set_Points (I: Item_Access'Class; Value: IEEE_Float_32);
   pragma Inline (Set_Points);

   -- Returns the number of bytes in the family name of the item referenced by I.
   --
   function Family_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Value_Offset + Font_Value_Offset_Family_Name_Byte_Count));
   pragma Inline (Family_Name_Byte_Count);

   -- Set the number of bytes in the family name of the item referenced by I.
   --
   procedure Set_Family_Name_Byte_Count (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Set_Family_Name_Byte_Count);

   -- Returns the number of bytes in the font name of the item referenced by I.
   --
   function Font_Name_Byte_Count (I: Item_Access'Class) return Unsigned_8 is
     (I.Storage.Get_Unsigned_8 (I.Value_Offset + Font_Value_Offset_Family_Name_Byte_Count));
   pragma Inline (Font_Name_Byte_Count);

   -- Set the number of bytes in the family name of the item referenced by I.
   --
   procedure Set_Font_Name_Byte_Count (I: Item_Access'Class; Value: Unsigned_8);
   pragma Inline (Set_Font_Name_Byte_Count);

   -- Return the family name of the item referenced by I.
   --
   function Family_Name (I: Item_Access'Class) return String is
     (I.Storage.Get_String (I.Value_Offset + Font_Value_Offset_Family_Name_String, Unsigned_32 (I.Family_Name_Byte_Count)));
   pragma Inline (Family_Name);

   -- Set the family name of the item referenced by I.
   --
   procedure Set_Family_Name (I: Item_Access'Class; Value: String);
   pragma Inline (Set_Family_Name);

   -- Return the font name of the item referenced by I.
   --
   function Font_Name (I: Item_Access'Class) return String is
     (I.Storage.Get_String (I.Value_Offset + Font_Value_Offset_Family_Name_String + Unsigned_32 (I.Family_Name_Byte_Count), Unsigned_32 (I.Font_Name_Byte_Count)));
   pragma Inline (Font_Name);

   -- Set the font name of the item referenced by I.
   -- Warning: Unprotected operation, make sure to first set the family name, then the font name. Otherwise the font name might overwrite the family name.
   --
   procedure Set_Font_Name (I: Item_Access'Class; Value: String);
   pragma Inline (Set_Font_Name);

   -- Return the font value if the item referenced by I.
   --
   function Font_Value (I: Item_Access'Class) return Font;
   pragma Inline (Font_Value);

   -- Set a new font value in the item referenced by I.
   --
   procedure Set_Font_Value (I: Item_Access'Class; Value: Font);
   pragma Inline (Set_Font_Value);

   -- Creates a new color item at the offset given in I.
   --
   procedure Create_Font_Item (I: Item_Access'Class; Name: BR_Item_Name := No_Name; Byte_Count: Unsigned_32 := Minimum_Item_Byte_Count (BR_Font); Parent_Offset: Unsigned_32 := 0; Value: Font := Default_Font);


   -- ====================
   -- Item_Access
   -- ====================

   -- Create a new item_access type
   --
   function Create_Item_Access  (S: Binary_Store_Ptr; O: Unsigned_32) return Item_Access;

private

   -- Used to set the components of an item name.
   --
   type Item_Name_Assistent (String_Length: Unsigned_32) is tagged
      record
         CRC_16: Unsigned_16;
         Ascii_Code: Array_Of_Unsigned_8 (1 .. String_Length);
         Quick_Check: Unsigned_32;
         Name_Field_Byte_Count: Unsigned_8;
      end record;

   -- Returns the number of bytes in the name that is or must be stored in the name field.
   --
   function Ascii_Code_Count (Assistent: Item_Name_Assistent) return Unsigned_8 is (Unsigned_8 (Assistent.Ascii_Code'Length));
   pragma Inline (Ascii_Code_Count);


end Item;

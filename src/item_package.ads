with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;
with Ada.Strings.Bounded;

with CRC_Package;

with BRBON; use BRBON;
with Container_Package; use Container_Package;



package Item_Package is


   -- All available item types
   --
   type Item_Type is
     (
      Illegal_Type,
      Null_Type,
      Bool_Type,
      Int_8_Type,
      Int_16_Type,
      Int_32_Type,
      Int_64_Type,
      UInt_8_Type,
      UInt_16_Type,
      UInt_32_Type,
      UInt_64_Type,
      Float_32_Type,
      Float_64_Type,
      String_Type,
      CRC_String_Type,
      Binary_Type,
      CRC_Binary_Type,
      Array_Type,
      Dictionary_Type,
      Sequence_Type,
      Table_Type,
      UUID_Type,
      RGBA_Type,
      Font_Type
     );

   for Item_Type'Size use 8;

   for Item_Type use
     (
      Illegal_Type    => 16#00#,
      Null_Type       => 16#01#,
      Bool_Type       => 16#02#,
      Int_8_Type      => 16#03#,
      Int_16_Type     => 16#04#,
      Int_32_Type     => 16#05#,
      Int_64_Type     => 16#06#,
      UInt_8_Type     => 16#07#,
      UInt_16_Type    => 16#08#,
      UInt_32_Type    => 16#09#,
      UInt_64_Type    => 16#0A#,
      Float_32_Type   => 16#0B#,
      Float_64_Type   => 16#0C#,
      String_Type     => 16#0D#,
      CRC_String_Type => 16#0E#,
      Binary_Type     => 16#0F#,
      CRC_Binary_Type => 16#10#,
      Array_Type      => 16#11#,
      Dictionary_Type => 16#12#,
      Sequence_Type   => 16#13#,
      Table_Type      => 16#14#,
      UUID_Type       => 16#15#,
      RGBA_Type       => 16#16#,
      Font_Type       => 16#17#
     );


   -- The defined item options (none at this time)
   --
   type Item_Options is
      record
         Option_0: Boolean;
         Option_1: Boolean;
         Option_2: Boolean;
         Option_3: Boolean;
         Option_4: Boolean;
         Option_5: Boolean;
         Option_6: Boolean;
         Option_7: Boolean;
      end record;

   for Item_Options use
      record
         Option_0 at 0 range 0..0;
         Option_1 at 0 range 1..1;
         Option_2 at 0 range 2..2;
         Option_3 at 0 range 3..3;
         Option_4 at 0 range 4..4;
         Option_5 at 0 range 5..5;
         Option_6 at 0 range 6..6;
         Option_7 at 0 range 7..7;
      end record;

   No_Item_Options: Item_Options := (False, False, False, False, False, False, False, False);


   -- The defined flags (none at this time)
   -- Note: Flags are for API implemention to use, not the API user.
   --
   type Item_Flags is
      record
         Flag_0: Boolean;
         Flag_1: Boolean;
         Flag_2: Boolean;
         Flag_3: Boolean;
         Flag_4: Boolean;
         Flag_5: Boolean;
         Flag_6: Boolean;
         Flag_7: Boolean;
      end record;

   for Item_Flags use
      record
         Flag_0 at 0 range 0..0;
         Flag_1 at 0 range 1..1;
         Flag_2 at 0 range 2..2;
         Flag_3 at 0 range 3..3;
         Flag_4 at 0 range 4..4;
         Flag_5 at 0 range 5..5;
         Flag_6 at 0 range 6..6;
         Flag_7 at 0 range 7..7;
      end record;

   No_Item_Flags: Item_Flags := (False, False, False, False, False, False, False, False);


   -- The maximum length of an item name
   --
   Maximum_Item_Name_Byte_Count: constant := 245;

   package Item_Name_Package is new Ada.Strings.Bounded.Generic_Bounded_Length (Maximum_Item_Name_Byte_Count);

   No_Name: Item_Name_Package.Bounded_String := Item_Name_Package.To_Bounded_String ("");


   -- The item header that is common for all items
   --
   type Item_Header is
      record
         The_Type: Item_Type;
         Options: Item_Options;
         Flags: Item_Flags;
         Name_Field_Byte_Count: Unsigned_8;
         Byte_Count: Unsigned_32;
         Parent_Offset: Unsigned_32;
      end record;

   for Item_Header use
      record
         The_Type at 0 range 0..7;
         Options at 1 range 0..7;
         Flags at 2 range 0..7;
         Name_Field_Byte_Count at 3 range 0..7;
         Byte_Count at 4 range 0..31;
         Parent_Offset at 8 range 0..31;
      end record;

   Item_Header_Byte_Count: constant := 12;

   type Item_Header_Ptr is access Item_Header;

   function To_Item_Header_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_Ptr);


   -- An item header with a name attached to it
   --
   type Item_Header_With_Name is
      record
         The_Item_Header: Item_Header;
         Small_Value: Unsigned_32;
         Name_CRC: CRC_Package.CRC_16;
         Name_Byte_Count: Unsigned_8;
         Name_Byte_Code: aliased Unsigned_8;
      end record;

   for Item_Header_With_Name use
      record
         The_Item_Header at                          0 range 0 .. (Item_Header_Byte_Count * 8) - 1;
         Small_Value     at Item_Header_Byte_Count + 0 range 0..31;
         Name_CRC        at Item_Header_Byte_Count + 4 range 0..15;
         Name_Byte_Count at Item_Header_Byte_Count + 6 range 0..7;
         Name_Byte_Code  at Item_Header_Byte_Count + 7 range 0..7;
      end record;

   type Item_Header_With_Name_Ptr is access Item_Header_With_Name;

   function To_Item_Header_With_Name_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Item_Header_With_Name_Ptr);
   function To_Item_Header_With_Name_Ptr is new Ada.Unchecked_Conversion (Item_Header_Ptr, Item_Header_With_Name_Ptr);

   Item_Name_Byte_Code_Offset: constant Unsigned_32 := Item_Header_Byte_Count + 7;


   -- Accessing header data

   function Get_Item_Type (C: Container; Item_Offset: Unsigned_32) return Item_Type;
   pragma Inline (Get_Item_Type);

   procedure Set_Item_Type (C: Container; Item_Offset: Unsigned_32; Value: Item_Type);
   pragma Inline (Set_Item_Type);

   function Get_Item_Options (C: Container; Item_Offset: Unsigned_32) return Item_Options;
   pragma Inline (Get_Item_Options);

   procedure Set_Item_Options (C: Container; Item_Offset: Unsigned_32; Value: Item_Options);
   pragma Inline (Set_Item_Options);

   function Get_Item_Flags (C: Container; Item_Offset: Unsigned_32) return Item_Flags;
   pragma Inline (Get_Item_Flags);

   procedure Set_Item_Flags (C: Container; Item_Offset: Unsigned_32; Value: Item_Flags);
   pragma Inline (Set_Item_Flags);

   function Get_Item_Name_Field_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Item_Name_Field_Byte_Count);

   procedure Set_Item_Name_Field_Byte_Count (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Item_Name_Field_Byte_Count);

   function Get_Item_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Item_Byte_Count);

   procedure Set_Item_Byte_Count (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Item_Byte_Count);

   function Get_Item_Parent_Offset (C: Container; Item_Offset: Unsigned_32) return Unsigned_32;
   pragma Inline (Get_Item_Parent_Offset);

   procedure Set_Item_Parent_Offset (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_32);
   pragma Inline (Set_Item_Parent_Offset);

   function Get_Item_Name_CRC (C: Container; Item_Offset: Unsigned_32) return CRC_Package.CRC_16;
   pragma Inline (Get_Item_Name_CRC);

   procedure Set_Item_Name_CRC (C: Container; Item_Offset: Unsigned_32; Value: CRC_Package.CRC_16);
   pragma Inline (Set_Item_Name_CRC);

   function Get_Item_Name_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_8;
   pragma Inline (Get_Item_Name_Byte_Count);

   procedure Set_Item_Name_Byte_Count (C: Container; Item_Offset: Unsigned_32; Value: Unsigned_8);
   pragma Inline (Set_Item_Name_Byte_Count);

   function Get_Item_Name (C: Container; Item_Offset: Unsigned_32) return Item_Name_Package.Bounded_String;
   pragma Inline (Get_Item_Name);

   procedure Set_Item_Name (C: Container; Item_Offset: Unsigned_32; Value: Item_Name_Package.Bounded_String);
   pragma Inline (Set_Item_Name);


   -- Support functions

   function Get_Item_Header_Ptr (C: Container; Item_Offset: Unsigned_32) return Item_Header_Ptr is (To_Item_Header_Ptr (C.Get_Unsigned_8_Ptr (Item_Offset)));
   pragma Inline (Get_Item_Header_Ptr);

   function Get_Item_Header_With_Name_Ptr (C: Container; Item_Offset: Unsigned_32) return Item_Header_With_Name_Ptr is (To_Item_Header_With_Name_Ptr (C.Get_Unsigned_8_Ptr (Item_Offset)));
   pragma Inline (Get_Item_Header_With_Name_Ptr);

   function Get_Item_Header_With_Name_Byte_Count (C: Container; Item_Offset: Unsigned_32) return Unsigned_32 is (Item_Header_Byte_Count + 4 + Unsigned_32 (Get_Item_Name_Field_Byte_Count (C, Item_Offset)));
   pragma Inline (Get_Item_Header_With_Name_Byte_Count);


end Item_Package;

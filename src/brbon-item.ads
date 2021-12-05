with Interfaces; use Interfaces;

with BRBON.Types; use BRBON.Types;


package BRBON.Item is

   type Instance is tagged private;

   type Item_Type is
     (
      Illegal,
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
      Crc_String_Type,
      Binary_Type,
      Crc_Binary_Type,
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
      Illegal         => 0,
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
      Crc_String_Type => 16#0E#,
      Binary_Type     => 16#0F#,
      Crc_Binary_Type => 16#10#,
      Array_Type      => 16#11#,
      Dictionary_Type => 16#12#,
      Sequence_Type   => 16#13#,
      Table_Type      => 16#14#,
      UUID_Type       => 16#15#,
      RGBA_Type       => 16#16#,
      Font_Type       => 16#17#
     );

   type Item_Flags is new Unsigned_8;

   type Item_Options is new Unsigned_8;

   function Get_Type (I: in out Instance) return Item_Type;
   procedure Set_Type (I: in out Instance; T: Item_Type);

   function Get_Options (I: in out Instance) return Item_Options;
   procedure Set_Options (I: in out Instance; O: Item_Options);

   function Get_Flags (I: in out Instance) return Item_Flags;
   procedure Set_Flags (I: in out Instance; F: Item_Flags);

   function Get_Name_Field_Byte_Count (I: in out Instance) return Unsigned_8;
   procedure Set_Name_Field_Byte_Count (I: in out Instance; Byte_Count: Unsigned_8);

   function Get_Byte_Count (I: in out Instance) return Unsigned_32;
   procedure Set_Byte_Count (I: in out Instance; Byte_Count: Unsigned_32);

   function Get_Parent_Offset (I: in out Instance) return Unsigned_32;
   procedure Set_Parent_Offset (I: in out Instance; Offset: Unsigned_32);

   function Get_Small_Value (I: in out Instance) return Unsigned_32;
   procedure Set_Small_Value (I: in out Instance; Small_Value: Unsigned_32);

private

   type Instance is tagged record
      First_Byte_Ptr: Array_Of_Unsigned_8_Ptr;
   end record;

end BRBON.Item;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNAT.Byte_Swapping;

with Crc_Package;

with BRBON.Utils;
with BRBON.Item_Package;
--with BRBON.Types; use BRBON.Types;


package body BRBON.Name_Field_Assistent_Package is


   function Swap_CRC_16 is new GNAT.Byte_Swapping.Swapped2 (CRC_Package.CRC_16);


   -----------------------------------------------------------------------------

   function Swap_Status (NFA: Name_Field_Assistent) return Boolean is (NFA.Swap);


   -----------------------------------------------------------------------------

   function Quick_Check_Value (NFA: Name_Field_Assistent) return Quick_Check_Value is (NFA.Quick_Check);


   -----------------------------------------------------------------------------

   function CRC (NFA: Name_Field_Assistent) return CRC_16 is (NFA.CRC);


   -----------------------------------------------------------------------------

   function Name_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_8 is (NFA.Name_Byte_Count);


   -----------------------------------------------------------------------------

   function Field_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_8 is (NFA.Field_Byte_Count);


   -----------------------------------------------------------------------------

   function Name (NFA: Name_Field_Assistent) return String is (NFA.Name);


   -----------------------------------------------------------------------------

   function Compare_Quick_Check (NFA: Name_Field_Assistent; Ptr: Quick_Check_Value_Ptr) return Boolean is
   begin
      return Ptr.all = NFA.Quick_Check;
   end Compare_Quick_Check;


   -----------------------------------------------------------------------------

   function Compare_String (NFA: Name_Field_Assistent; Ptr: Unsigned_8_Ptr; Byte_Count: Unsigned_8) return Boolean is
      type Arr is new Unsigned_8_Array (1 .. Byte_Count);
      type Arr_Ptr is access Arr;
      function To_Arr_Ptr is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Arr_Ptr);
   begin
      if Byte_Count /= NFA.Ascii_Byte_Count then
         return false;
      else
         return To_Arr_Ptr (Ptr).all = NFA.Ascii_Code;
      end if;
   end Compare_String;



   -----------------------------------------------------------------------------

   function Name_Field_Assistent_Factory (Name: String; S: Store) return Name_Field_Assistent is

      QC: Private_Quick_Check_Value;
      Assistent: Name_Field_Assistent;

   begin

      -- Avoid empty names
      --
      if Name'Length = 0 then
         Assistent.CRC := 0;
         Assistent.Field_Byte_Count := 0;
         Assistent.Ascii_Byte_Count := 0;
         return Assistent;
      end if;

      -- Don't accept too long names
      --
      if Name'Length > Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Max_Name_Length'Image & ")");
      end if;

      -- Set the actual name length
      --
      Assistent.Ascii_Byte_Count := Unsigned_8 (Name'Length);

      -- Set the CRC and the swap status
      --
      Assistent.Swap := S.Swap;
      if S.Swap then
         Assistent.CRC := Swap_CRC_16 (CRC_Package.Calculate_CRC_16 (Name));
      else
         Assistent.CRC := CRC_Package.Calculate_CRC_16 (Name);
      end if;

      -- Set the byte code
      -- Note: Unchecked conversion assignments don't seem to work
      --
      Assistent.Name := Name;


      -- Set the field size
      --
      Assistent.Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + Name'Length);

      -- Set the quick check value
      --
      QC.CRC := Assistent.CRC;
      QC.Count := Assistent.ASCII_Byte_Count;
      QC.Char := Assistent.ASCII_Code (1);
      Assistent.Quick_Check := To_Quick_Check_Value (QC);

      return Assistent;

   end Name_Field_Assistent_Factory;


end BRBON.Name_Field_Assistent_Package;

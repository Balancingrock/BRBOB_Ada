with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNAT.Byte_Swapping;

with Crc_Package;

with BRBON.Utils;
with BRBON.Item_Package;
with BRBON.Types; use BRBON.Types;


package body BRBON.Name_Field_Assistent_Package is


   function Swap_CRC_16 is new GNAT.Byte_Swapping.Swapped2 (CRC_Package.CRC_16);

   function Get_Swap_Status (NFA: Name_Field_Assistent) return Boolean is (NFA.Swap);

   function Get_Quick_Check_Value (NFA: Name_Field_Assistent) return Unsigned_32 is (NFA.Quick_Check);


--   function Get_Minimum_Item_Byte_Count (NFA: Name_Field_Assistent) return Unsigned_32 is

--   begin

--      return Unsigned_32 (NFA.Field_Byte_Count) + BRBON.Item_Package.Item_Header_Byte_Count;

--   end Get_Minimum_Item_Byte_Count;


   -----------------------------------------------------------------------------

   function Name_Field_Assistent_Factory (Name: String; S: Store) return Name_Field_Assistent is

      QC: Quick_Check_Type;
      Assistent: Name_Field_Assistent;
      Index: Unsigned_32 := 1;

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
      if Name'Length > Types.Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Types.Max_Name_Length'Image & ")");
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
      for C of Name loop
         Assistent.Ascii_Code (Index) := Character'Pos (C);
         Index := Index + 1;
      end loop;

      -- Set the field size
      --
      Assistent.Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + Name'Length);

      -- Set the quick check value
      --
      QC.CRC := Assistent.CRC;
      QC.Count := Assistent.ASCII_Byte_Count;
      QC.Char := Assistent.ASCII_Code (1);
      Assistent.Quick_Check := Quick_Check_As_Unsigned_32 (QC);

      return Assistent;

   end Name_Field_Assistent_Factory;


end BRBON.Name_Field_Assistent_Package;

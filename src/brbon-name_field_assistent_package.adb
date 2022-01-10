with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with GNAT.Byte_Swapping;

with Crc_Package;

with BRBON.Utils;
use BRBON.Item_Name_Bounded_String_Package;


package body BRBON.Name_Field_Assistent_Package is


   type Quick_Check_Record is
      record
         CRC: Unsigned_16;
         Count: Unsigned_8;
         Char: Unsigned_8;
      end record;

   for Quick_Check_Record'Size use 32;

   for Quick_Check_Record use
      record
         CRC at 0 range 0..15;
         Count at 2 range 0..7;
         Char at 3 range 0..7;
      end record;

   function To_Quick_Check_Value is new Ada.Unchecked_Conversion (Quick_Check_Record, Quick_Check_Value);


   -----------------------------------------------------------------------------

   function Swap_Status (N: Name_Field_Assistent) return Boolean is (N.Swap);


   -----------------------------------------------------------------------------

   function Quick_Check_Passes (N: Name_Field_Assistent; Ptr: Quick_Check_Value_Ptr) return Boolean is (Ptr.all = N.Quick_Check);


   -----------------------------------------------------------------------------

   function Name_Check_Passes (N: Name_Field_Assistent; Name: Item_Name) return Boolean is (N.Name = Name);


   -----------------------------------------------------------------------------

   function Swap_CRC_16 is new GNAT.Byte_Swapping.Swapped2 (CRC_Package.CRC_16);


   -----------------------------------------------------------------------------

   function Name_Field_Assistent_Factory (Name: String; C: Container) return Name_Field_Assistent is

      QC: Quick_Check_Record;
      Assistent: Name_Field_Assistent;

   begin

      -- Avoid empty names
      --
      if Name'Length = 0 then
         Assistent.CRC := 0;
         Assistent.Field_Byte_Count := 0;
         Assistent.Name_Byte_Count := 0;
         return Assistent;
      end if;

      -- Don't accept too long names
      --
      if Name'Length > Maximum_Item_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Maximum_Item_Name_Length'Image & ")");
      end if;

      -- Set the actual name length
      --
      Assistent.Name_Byte_Count := Unsigned_8 (Name'Length);

      -- Set the CRC and the swap status
      --
      Assistent.Swap := C.Swap_Status;
      if Assistent.Swap then
         Assistent.CRC := Swap_CRC_16 (CRC_Package.Calculate_CRC_16 (Name));
      else
         Assistent.CRC := CRC_Package.Calculate_CRC_16 (Name);
      end if;

      -- Set the byte code
      -- Note: Unchecked conversion assignments don't seem to work
      --
      Assistent.Name := Item_Name_Bounded_String_Package.To_Bounded_String (Name);


      -- Set the field size
      --
      Assistent.Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + Name'Length);

      -- Set the quick check value
      --
      QC.CRC := Assistent.CRC;
      QC.Count := Assistent.Name_Byte_Count;
      QC.Char := Character'Pos (Item_Name_Bounded_String_Package.Element (Assistent.Name, 1));
      Assistent.Quick_Check := To_Quick_Check_Value (QC);

      return Assistent;

   end Name_Field_Assistent_Factory;


end BRBON.Name_Field_Assistent_Package;

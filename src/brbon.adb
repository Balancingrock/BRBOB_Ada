with Ada.Exceptions;

with GNAT.Byte_Swapping;

with CRC_Package; use CRC_Package;

with BRBON.Utils; use BRBON.Utils;



package body BRBON is


   function Swap_CRC_16 is new GNAT.Byte_Swapping.Swapped2 (CRC_Package.CRC_16);
   
   
   -----------------------------------------------------------------------------
   
   procedure Set_Data_Byte_Order (B: in out Store; Value: Byte_Storage_Order) is
   
   begin
   
      B.Swap := Value /= BRBON.Machine_Byte_Storage_Order;
   
   end Set_Data_Byte_Order;

   
   
   -----------------------------------------------------------------------------

   function Name_Field_Assistent_Factory (Name: String; S: Store) return Name_Field_Assistent is

      QC: Quick_Check_Value;
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
      if Name'Length > Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Max_Name_Length'Image & ")");
      end if;

      -- Set the actual name length
      --
      Assistent.Name_Byte_Count := Unsigned_8 (Name'Length);

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
      Assistent.Name := Item_Name_Bounded_String_Package.To_Bounded_String (Name);


      -- Set the field size
      --
      Assistent.Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + Name'Length);

      -- Set the quick check value
      --
      QC.CRC := Assistent.CRC;
      QC.Count := Assistent.Name_Byte_Count;
      QC.Char := Character'Pos (Item_Name_Bounded_String_Package.Element (Assistent.Name, 1));
      Assistent.Quick_Check := QC;

      return Assistent;

   end Name_Field_Assistent_Factory;

end BRBON;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with Crc_Package;

with BRBON.Utils;


package body BRBON.Name_Field_Assistent is


   function Get_Minimum_Item_Byte_Count (I: Instance) return Unsigned_32 is
   begin
      return Unsigned_32 (I.Name_Field_Byte_Count) + Types.Minimum_Item_Byte_Count;
   end Get_Minimum_Item_Byte_Count;


   function Create_Name_Field_Assistent (S: String) return Instance is

      Assistent: Instance;
      Index: Unsigned_32 := 1;

   begin

      -- Avoid empty names
      --
      if S'Length = 0 then
         Assistent.CRC := 0;
         Assistent.Name_Field_Byte_Count := 0;
         Assistent.Ascii_Byte_Count := 0;
         return Assistent;
      end if;

      -- Don't accept too long names
      --
      if S'Length > Types.Max_Name_Length then
         Ada.Exceptions.Raise_Exception (Name_Error'Identity, "Name length exceeds maximum (" & Types.Max_Name_Length'Image & ")");
      end if;

      -- Set the actual name length
      --
      Assistent.Ascii_Byte_Count := Unsigned_8 (S'Length);

      -- Set the CRC
      --
      Assistent.CRC := CRC_Package.Calculate_CRC_16 (S);

      -- Set the byte code
      -- Note: Unchecked conversion assignments don't seem to work
      --
      for C of S loop
         Assistent.Ascii_Code (Index) := Character'Pos (C);
         Index := Index + 1;
      end loop;

      -- Set the field size
      --
      Assistent.Name_Field_Byte_Count := Utils.Round_Up_To_Nearest_Multiple_Of_8 (3 + S'Length);

      return Assistent;

   end Create_Name_Field_Assistent;


   function Get_Quick_Check_Value (I: in out Instance) return Unsigned_32 is
      A: aliased Instance := I;
      type Instance_Ptr is access all Instance;
      type Unsigned_32_Ptr is access all Unsigned_32;
      function To_Unsigned_32 is new Ada.Unchecked_Conversion (Instance_Ptr, Unsigned_32_Ptr);
      Ptr: Instance_Ptr := A'Access;
   begin
      return To_Unsigned_32 (Ptr).all;
   end Get_Quick_Check_Value;

end BRBON.Name_Field_Assistent;

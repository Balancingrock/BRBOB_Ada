with Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with BRBON.Configure;
with BRBON.Utils;


package body Serializable is


   function Copy_Next_Byte (Source: in out Instance; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Cursor <= Source.Last then
         Byte := Source.Base_Ptr.all (Source.Cursor);
         Source.Cursor := Source.Cursor + 1;
         if Source.Cursor > Source.Last then
            if Source.Must_Deallocate then
               Deallocate_Array_Of_Unsigned_8 (Source.Base_Ptr);
            end if;
         end if;
         return True;
      else
         return False;
      end if;
   end Copy_Next_Byte;


   -- String
   --
   function Create_With_Copy (Copy_Bytes_From: String) return Instance is
      Source: String renames Copy_Bytes_From;
      A_Ptr: Array_Of_Unsigned_8_Ptr;
      I: Unsigned_32 := 0;
   begin
      A_Ptr := new Array_Of_Unsigned_8 (0 .. Source'Length - 1);
      for C of Source loop
         A_Ptr.all(I) := Character'Pos (C);
         I := I + 1;
      end loop;
      return (A_Ptr, 0, 0, A_Ptr.all'Last, True);
   end Create_With_Copy;


   -- Binary
   --
   function Create_With_Copy (Copy_Bytes_From: Array_Of_Unsigned_8) return Instance is
      Source: Array_Of_Unsigned_8 renames Copy_Bytes_From;
      A_Ptr: Array_Of_Unsigned_8_Ptr;
   begin
      A_Ptr := new Array_Of_Unsigned_8 (0 .. Source'Length - 1);
      A_Ptr.all := Source;
      return (A_Ptr, 0, 0, A_Ptr.all'Last, True);
   end Create_With_Copy;


   -- No copy
   --
   function Create_Without_Copy (Use_In_Place: Array_Of_Unsigned_8_Ptr; First: Unsigned_32; Last: Unsigned_32) return Instance is
   begin
      return (Use_In_Place, First, First, Last, False);
   end Create_Without_Copy;


   function Is_Empty (Source: in out Instance) return Boolean is
   begin
      return Source.Cursor > Source.Last;
   end Is_Empty;


   function Remaining_Bytes (Source: in out Instance) return Integer is
   begin
      return Integer (Source.Last) - Integer (Source.Cursor) + 1;
   end Remaining_Bytes;

   function Index_Of_Last_Byte (Source: in out Instance) return Unsigned_32 is
   begin
      --if Source.Remaining_Bytes = Integer (Source.Last - Source.First + 1) then
      --   return Source.First;
      --else
         return Source.Cursor - Source.First;
      --end if;
   end Index_Of_Last_Byte;

   function Compare (Source: in out Instance; Expected_Values: Array_Of_Unsigned_8) return Boolean is
      Failed: Exception;
      Byte: Unsigned_8;
      I: Unsigned_32 := Expected_Values'First;
   begin

      if Source.Remaining_Bytes /= Expected_Values'Length then
         raise Failed;
      end if;

      while Source.Copy_Next_Byte (Byte) loop
        if Byte /= Expected_Values (I) then
               raise Failed;
         end if;
         I := I + 1;
      end loop;

      return True;

   exception

      when Failed => return False;

   end Compare;


   function Compare (Source: in out Instance; Expected_Values: Array_Of_Unsigned_8; Dont_Care: Array_Of_Boolean) return Boolean is
      Failed: Exception;
      Byte: Unsigned_8;
      DI: Unsigned_32 := Dont_Care'First;
      EI: Unsigned_32 := Expected_Values'First;
   begin

      if Source.Remaining_Bytes /= Expected_Values'Length then
         raise Failed;
      end if;

      while Source.Copy_Next_Byte (Byte) loop
         if not Dont_Care (DI) then
            if Byte /= Expected_Values (EI) then
               raise Failed;
            end if;
         end if;
         DI := DI + 1;
         EI := EI + 1;
      end loop;

      return True;

   exception

      when Failed => return False;

   end Compare;


   procedure Dump_2_Lines (Source: in out Instance; Around: Unsigned_32 := 0; Show_Cursor: Boolean := false) is
      I: Unsigned_32 := Source.First + Around;
   begin
      if Source.Remaining_Bytes > 0 or not Source.Must_Deallocate then
         BRBON.Utils.Put_Hex_8_Two_Lines (Source.Base_Ptr.all, I, Show_Cursor);
      else
         Put_Line ("The serializable has already deallocated");
      end if;
   end Dump_2_Lines;


   procedure Put_All (Source: in out Instance) is
      I: Unsigned_32 := Source.Base_Ptr.all'First;
      Total: Unsigned_32 := Source.Last - Source.First + 1;
      Ptr_Valid: String (1 .. 7);
   begin
      if Source.Remaining_Bytes < 1 and Source.Must_Deallocate then
         Ptr_Valid := "Invalid";
      else
         Ptr_Valid := "Valid  ";
         end if;
      New_Line (2);
      Put_Line ("Data ptr:        " & Ptr_Valid);
      Put_Line ("Index of First: " & Source.First'Image);
      Put_Line ("Index of Last:  " & Source.Last'Image);
      Put_Line ("Index of Cursor:" & Source.Cursor'Image);
      Put_Line ("Must_Deallocate: " & Source.Must_Deallocate'Image);
      Put_Line ("Total nof Bytes:" & Total'Image);
      Put_Line ("Remaining Bytes:" & Source.Remaining_Bytes'Image);
      Put_Line ("Data:");
      while I < Source.Base_Ptr.all'Last loop
         BRBON.Utils.Put_Hex_8_Two_Lines (Source.Base_Ptr.all, I, False);
         New_Line;
         I := I + 16;
      end loop;
   end Put_All;


end Serializable;

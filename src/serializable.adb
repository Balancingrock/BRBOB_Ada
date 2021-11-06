with Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with BRBON.Configure; use BRBON.Configure;


package body Serializable is

   procedure Dump_Instance (Source: in out Instance) is
   begin
      New_Line (2);
      Put_Line ("Serializable.Instance.First            = " & Source.First'Image);
      Put_Line ("Serializable.Instance.Cursor           = " & Source.Cursor'Image);
      Put_Line ("Serializable.Instance.Last             = " & Source.Last'Image);
      Put_Line ("Serializable.Instance.Must_Deallocate  = " & Source.Must_Deallocate'Image);
   end Dump_Instance;

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
   function New_Instance (Copy_Bytes_From: String) return Instance is
      Source: String renames Copy_Bytes_From;
      A_Ptr: Array_Of_Unsigned_8_Ptr;
      I: Unsigned_32 := 1;
   begin
      A_Ptr := new Array_Of_Unsigned_8 (1 .. Source'Length);
      for C of Source loop
         A_Ptr.all(I) := Character'Pos (C);
         I := I + 1;
      end loop;
      return (A_Ptr, 1, 1, A_Ptr.all'Last, True);
   end New_Instance;


   -- Binary
   --
   function New_Instance (Copy_Bytes_From: Array_Of_Unsigned_8) return Instance is
      Source: Array_Of_Unsigned_8 renames Copy_Bytes_From;
      A_Ptr: Array_Of_Unsigned_8_Ptr;
   begin
      A_Ptr := new Array_Of_Unsigned_8 (1 .. Source'Length);
      A_Ptr.all := Source;
      return (A_Ptr, 1, 1, A_Ptr.all'Last, True);
   end New_Instance;


   -- No copy
   --
   function New_Instance (Use_In_Place: Array_Of_Unsigned_8_Ptr; First: Unsigned_32; Last: Unsigned_32) return Instance is
   begin
      return (Use_In_Place, First, First, Last, False);
   end New_Instance;


   function Is_Empty (Source: in out Instance) return Boolean is
   begin
      return Source.Cursor > Source.Last;
   end Is_Empty;


   function Remaining_Bytes (Source: in out Instance) return Integer is
   begin
      return Integer (Source.Last) - Integer (Source.Cursor) + 1;
   end Remaining_Bytes;

end Serializable;

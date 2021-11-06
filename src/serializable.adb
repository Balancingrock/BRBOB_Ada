with Interfaces.C;

with BRBON.Configure; use BRBON.Configure;


package body Serializable is


   function Copy_Next_Byte (Source: in out Instance; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remaining > 0 then
         Byte := Source.Base_Ptr.all (Source.Base_Ptr.all'Last - (Source.Remaining - 1));
         Source.Remaining := Source.Remaining - 1;
         return True;
      else
         Deallocate_Array_Of_Unsigned_8 (Source.Base_Ptr);
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
      return (A_Ptr, Source'Length);
   end New_Instance;


   -- Binary
   --
   function New_Instance (Copy_Bytes_From: Array_Of_Unsigned_8) return Instance is
      Source: Array_Of_Unsigned_8 renames Copy_Bytes_From;
      A_Ptr: Array_Of_Unsigned_8_Ptr;
   begin
      A_Ptr := new Array_Of_Unsigned_8 (1 .. Source'Length);
      A_Ptr.all := Source;
      return (A_Ptr, Source'Length);
   end New_Instance;


end Serializable;

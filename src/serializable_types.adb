with Interfaces.C;

with BRBON_Configure; use BRBON_Configure;


package body Serializable_Types is



   function Next (Source: in out Serializable; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remaining > 0 then
         Byte := Source.Bytes (Source.Bytes'Last - Source.Remaining);
         Source.Remaining := Source.Remaining - 1;
         return True;
      else
         Deallocate_Array_Of_Unsigned_8 (Source.Bytes);
         return False;
      end if;
   end Next;


   -- String
   --
   function New_Serializable (Str: String) return Serializable is
      Arr: Array_Of_Unsigned_8_Ptr;
      I: Unsigned_32 := 1;
   begin
      Arr := new Array_Of_Unsigned_8 (1 .. Str'Length);
      for C of Str loop
         Arr.all(I) := Character'Pos (C);
      end loop;
      return (Arr, Arr'Length);
   end New_Serializable;


   -- Binary
   --
   function New_Serializable (Arr: Array_Of_Unsigned_8) return Serializable is
      A: Array_Of_Unsigned_8_Ptr;
   begin
      A := new Array_Of_Unsigned_8 (1 .. Arr'Length);
      A.all := Arr;
      return (A, A'Length);
   end New_Serializable;


end Serializable_Types;

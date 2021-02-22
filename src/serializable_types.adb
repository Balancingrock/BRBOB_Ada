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
   function New_Serializable (Ptr: String_Ptr) return Serializable is
      Arr: Array_Of_Unsigned_8_Ptr;
      I: Unsigned_32 := 1;
   begin
      Arr := new Array_Of_Unsigned_8 (1 .. Ptr.all'Length);
      for C of Ptr.all loop
         Arr.all(I) := Character'Pos (C);
      end loop;
      return (Arr, Ptr.all'Length);
   end New_Serializable;


   -- Binary
   --
   function New_Serializable (Ptr: Array_Of_Unsigned_8_Ptr) return Serializable is
      Arr: Array_Of_Unsigned_8_Ptr;
   begin
      Arr := new Array_Of_Unsigned_8 (1 .. Ptr.all'Length);
      Arr.all := Ptr.all;
      return (Arr, Arr'Length);
   end New_Serializable;


end Serializable_Types;

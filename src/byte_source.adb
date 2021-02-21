with Interfaces.C;

package body Byte_Source is


   function Next (Source: in out String_Source; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 0 then return False; end if;
      Source.Remainder := Source.Remainder - 1;
      Byte := Character'Pos (Source.Ptr.all (Source.Ptr.all'Length - Natural (Source.Remainder)));
      return True;
   end Next;

   function Next (Source: in out Array_Of_Unsigned_8_Source; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 0 then return False; end if;
      Byte := Source.Ptr.all(Source.Length - Source.Remainder);
      Source.Remainder := Source.Remainder - 1;
      return True;
   end Next;

end Byte_Source;

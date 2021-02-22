with Interfaces.C;

with BRBON_Configure; use BRBON_Configure;


package body Decomposable_Types is


   -- String
   --
   function Next (Source: in out Decomposable_String; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 0 then return False; end if;
      Source.Remainder := Source.Remainder - 1;
      Byte := Character'Pos (Source.Ptr.all (Source.Ptr.all'Length - Natural (Source.Remainder)));
      return True;
   end Next;


   -- Array of Unsigned_8
   --
   function Next (Source: in out Decomposable_Array_Of_Unsigned_8; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 0 then return False; end if;
      Byte := Source.Ptr.all(Source.Ptr.all'Last - Source.Remainder);
      Source.Remainder := Source.Remainder - 1;
      return True;
   end Next;


   -- Boolean
   --
   function Next (Source: in out Decomposable_Boolean; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 1 then
         Source.Remainder := 0;
         if Source.Value then
            Byte := 2#1111_1111#;
         else
            Byte := 0;
         end if;
         return True;
      else
         return False;
      end if;
   end Next;


   -- Integer_8
   --
   function Next (Source: in out Decomposable_Integer_8; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 1 then
         Source.Remainder := 0;
         Byte := To_Unsigned_8 (Source.Value);
         return True;
      else
         return False;
      end if;
   end Next;


   -- Unsigned_8
   --
   function Next (Source: in out Decomposable_Unsigned_8; Byte: out Unsigned_8) return Boolean is
   begin
      if Source.Remainder = 1 then
         Source.Remainder := 0;
         Byte := Source.Value;
         return True;
      else
         return False;
      end if;
   end Next;


   -- Integer_16
   --
   function Next (Src: in out Decomposable_Integer_16; Byte: out Unsigned_8) return Boolean is
   begin
      if Src.Remainder > 0 then
         Byte := Src.Value (Src.Value'Last - Src.Remainder);
         Src.Remainder := Src.Remainder - 1;
         return True;
      else
         return False;
      end if;
   end Next;
   --
   function New_Decomposable_Integer_16 (Value: Integer_16; Endianness: BRBON.Endianness) return Decomposable_Integer_16 is
   begin
      if Endianness = Machine_Endianness then
         return (2, To_Two_Bytes (Value));
      else
         return (2, To_Two_Bytes (Swap_Integer_16 (Value)));
      end if;
   end New_Decomposable_Integer_16;


   -- Unsigned_16
   --
   function Next (Src: in out Decomposable_Unsigned_16; Byte: out Unsigned_8) return Boolean is
   begin
      if Src.Remainder > 0 then
         Byte := Src.Value (Src.Value'Last - Src.Remainder);
         Src.Remainder := Src.Remainder - 1;
         return True;
      else
         return False;
      end if;
   end Next;
   --
   function New_Decomposable_Unsigned_16 (Value: Unsigned_16; Endianness: BRBON.Endianness) return Decomposable_Unsigned_16 is
   begin
      if Endianness = Machine_Endianness then
         return (2, To_Two_Bytes (Value));
      else
         return (2, To_Two_Bytes (Swap_Unsigned_16 (Value)));
      end if;
   end New_Decomposable_Unsigned_16;


   -- Integer_32
   --
   function Next (Src: in out Decomposable_Integer_32; Byte: out Unsigned_8) return Boolean is
   begin
      if Src.Remainder > 0 then
         Byte := Src.Value (Src.Value'Last - Src.Remainder);
         Src.Remainder := Src.Remainder - 1;
         return True;
      else
         return False;
      end if;
   end Next;
   --
   function New_Decomposable_Integer_32 (Value: Integer_32; Endianness: BRBON.Endianness) return Decomposable_Integer_32 is
   begin
      if Endianness = Machine_Endianness then
         return (4, To_Four_Bytes (Value));
      else
         return (4, To_Four_Bytes (Swap_Integer_32 (Value)));
      end if;
   end New_Decomposable_Integer_32;


end Decomposable_Types;

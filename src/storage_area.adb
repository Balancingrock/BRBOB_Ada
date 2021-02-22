with Ada.Unchecked_Conversion;


package body Storage_Area is


   function Allocate_And_Create (Byte_Count: Unsigned_32; Using_Endianness: Endianness) return Storage_Area_Ptr is
      S: Storage_Area_Ptr;
   begin
      S := new Storage_Area;
      S.all.Uses_Endianness := Using_Endianness;
      S.all.Data := new Array_Of_Unsigned_8 (1 .. Byte_Count);
      return S;
   end Allocate_And_Create;


   procedure Finalization (S: in out Storage_Area) is
   begin
      Deallocate_Array_Of_Unsigned_8 (S.Data);
   end Finalization;


   -- Operational

   procedure Set_Unsigned_8 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_8) is
   begin
      S.Data.all (Offset) := Value;
   end Set_Unsigned_8;


   function Get_Unsigned_8 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_8 is
   begin
      return S.Data.all (Offset);
   end Get_Unsigned_8;


   procedure Set_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_16) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Unsigned_16 (Value));
      end if;
   end Set_Unsigned_16;


   function Get_Unsigned_16 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_16 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Unsigned_16 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Unsigned_16 (To_Unsigned_16 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Unsigned_16;


   procedure Set_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Unsigned_32 (Value));
      end if;
   end Set_Unsigned_32;


   function Get_Unsigned_32 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_32 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Unsigned_32 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Unsigned_32 (To_Unsigned_32 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Unsigned_32;


   procedure Set_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32; Value: Unsigned_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Unsigned_64 (Value));
      end if;
   end Set_Unsigned_64;


   function Get_Unsigned_64 (S: Storage_Area; Offset: Unsigned_32) return Unsigned_64 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Unsigned_64 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Unsigned_64 (To_Unsigned_64 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Unsigned_64;


   procedure Set_Integer_8 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_8) is
   begin
      S.Data.all (Offset) := To_Unsigned_8 (Value);
   end Set_Integer_8;


   function Get_Integer_8 (S: Storage_Area; Offset: Unsigned_32) return Integer_8 is
   begin
      return To_Integer_8 (S.Data.all (Offset));
   end Get_Integer_8;


   procedure Set_Integer_16 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_16) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 1) := To_Two_Bytes (Swap_Integer_16 (Value));
      end if;
   end Set_Integer_16;


   function Get_Integer_16 (S: Storage_Area; Offset: Unsigned_32) return Integer_16 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Integer_16 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Integer_16 (To_Integer_16 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Integer_16;


   procedure Set_Integer_32 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Integer_32 (Value));
      end if;
   end Set_Integer_32;


   function Get_Integer_32 (S: Storage_Area; Offset: Unsigned_32) return Integer_32 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Integer_32 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Integer_32 (To_Integer_32 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Integer_32;


   procedure Set_Integer_64 (S: Storage_Area; Offset: Unsigned_32; Value: Integer_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Integer_64 (Value));
      end if;
   end Set_Integer_64;


   function Get_Integer_64 (S: Storage_Area; Offset: Unsigned_32) return Integer_64 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Integer_64 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Integer_64 (To_Integer_64 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Integer_64;


   procedure Set_Float_32 (S: Storage_Area; Offset: Unsigned_32; Value: IEEE_Float_32) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 3) := To_Four_Bytes (Swap_Float_32 (Value));
      end if;
   end Set_Float_32;


   function Get_Float_32 (S: Storage_Area; Offset: Unsigned_32) return IEEE_Float_32 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Float_32 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Float_32 (To_Float_32 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Float_32;


   procedure Set_Float_64 (S: Storage_Area; Offset: Unsigned_32; Value: IEEE_Float_64) is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Value);
      else
         S.Data.all (Offset .. Offset + 7) := To_Eight_Bytes (Swap_Float_64 (Value));
      end if;
   end Set_Float_64;


   function Get_Float_64 (S: Storage_Area; Offset: Unsigned_32) return IEEE_Float_64 is
   begin
      if S.Uses_Endianness = Machine_Endianness then
         return To_Float_64 (S.Data.all (Offset .. Offset + 1));
      else
         return Swap_Float_64 (To_Float_64 (S.Data.all (Offset .. Offset + 1)));
      end if;
   end Get_Float_64;


   procedure Set_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: Array_Of_Unsigned_8) is
   begin
      S.Data.all (Offset .. Offset + Value'Last) := Value;
   end Set_Unsigned_8_Array;


   procedure Get_Unsigned_8_Array (S: Storage_Area; Offset: Unsigned_32; Value: in out Array_Of_Unsigned_8) is
   begin
      Value := S.Data.all (Offset .. Offset + Value'Last);
   end Get_Unsigned_8_Array;


   -- ==============
   -- Create items
   -- ==============





end Storage_Area;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with BRBON.Container; use BRBON.Container;


package body Support is

   function Put_As_Line (Arr: in Array_Of_Unsigned_8) return String is

      package Hex_IO is new Modular_IO (Unsigned_8);
      use Hex_IO;

      Result: Unbounded_String := To_Unbounded_String ("");
      Temp: String := "16#00#";

   begin

      Hex_IO.Default_Width := 7;
      Hex_IO.Default_Base := 16;

      for B of Arr loop
         Hex_IO.Put (Temp, B);
         if B < 16#10# then
            Result := Result & " 16#0" & Temp (5..6);
         else
            Result := Result & " " & Temp;
         end if;
      end loop;

      return To_String (Result);

   end Put_As_Line;


   function Verify_Small_Bytes (Container: in out Instance; Offset: Unsigned_32; Expected: Array_Of_Unsigned_8) return Test_Result is

      Actual: Array_Of_Unsigned_8 (Expected'Range);

   begin

      Test_Support_Get_Bytes (Container, Start => 0, Dest => Actual);

      if Actual /= Expected then
         Put_Line (" - Failed, Expected: " & Put_As_Line (Expected) & ", Found: " & Put_As_Line (Actual));
         return Failed;
      end if;
      return Passed;

   end Verify_Small_Bytes;


   function Verify_Array_Of_Unsigned_8 (Found: in out Serializable.Instance; Expected: Array_Of_Unsigned_8_Ptr; Skip_Map: Array_Of_Boolean) return Test_Result is

      Cursor: Unsigned_32;
      S: Serializable.Instance;

   begin
      if Found.Remaining_Bytes /= Expected.all'Length then

         New_Line (2);
         Put_Line ("Compare failed due to size mismatch");
         Put_Line ("Expected:" & Expected'Length'Image & " bytes, found:" & Found.Remaining_Bytes'Image & " bytes.");

         return Failed;

      elsif not Found.Compare (Expected.all, Skip_Map) then

         Cursor := Found.Index_Of_Last_Byte;

         New_Line (2);
         Put_Line ("Block verification failed");

         New_Line;
         Put_Line ("Expected:");
         S := Serializable.Create_With_Copy (Expected.all);
         S.Dump_2_Lines (Around => Cursor);

         New_Line (2);
         Put_Line ("Found:");
         Found.Dump_2_Lines (Around => Cursor, Show_Cursor => True);

         return Failed;

      else

         return Passed;

      end if;

   end Verify_Array_Of_Unsigned_8;


end Support;

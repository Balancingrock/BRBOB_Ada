with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

separate (Container_Tests)

function Read_Write (Count: in out Integer) return Test_Result is


   Container: Instance := Factory (Buffer'Access, Machine_Endianness);

   Str: String := "Hello";

   Path: String := Test_Files_Root & "rw_test.brbon";

begin

   Set_String (Container, 0, Str);

   -- Write file
   --
   begin
      Write_To_File (Container, Path);
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (" - Failed: Cannot store file in given path: " & Path);
         return Failed;
   end;

   -- Read file
   --
   declare
      Second: Instance := Factory (Buffer'Access, Path, Machine_Endianness);
      Act: String := Get_String (Second, Offset => 0, Length => 5);
   begin
      if Str /= Act then
         Put_Line (" - Failed, Read " & Act & " Expected: " & Str);
         return Failed;
      end if;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (" - Failed: Cannot read file under given path: " & Path);
         return Failed;
   end;

   return Passed;

end Read_Write;

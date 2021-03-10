with Ada.IO_Exceptions;

separate (Container_Tests)

function Read_Write (Count: in out Integer) return Test_Result is


   Byte_Count: Unsigned_32 := 1000;
   Container: Storage_Area := Storage_Area_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);

   Str: String := "Hello";

   Path: String := Test_Files_Root & "rw_test.brbon";

begin

   Container.Set_String (0, Str);

   begin
      Container.Write_to_File (Filepath => Path);
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (" - Failed: Cannot store file in given path: " & Path);
         return Failed;
   end;

   declare

      Second: Storage_Area := Storage_Area_Factory (Path, Machine_Endianness);
      Act: String := Second.Get_String (Offset => 0, Length => 5);

   begin

      if Str /= Act then
         Put_Line (" - Failed, Read " & Act & " Expected: " & Str);
         return Failed;
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (" - Failed: Cannot store read under given path: " & Path);
         return Failed;
   end;

   return Passed;

end Read_Write;

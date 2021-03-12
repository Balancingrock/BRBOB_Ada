separate (Container_Tests)

function String_Access (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Container: Byte_Store := Byte_Store_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);

   Str: String := "Hello";

begin

   Container.Set_String (0, Str);

   declare
      Act: String := Container.Get_String (0, 5);
   begin
      if Act /= Str then
         Put_Line (" - Failed, Expected: " & Str & ", Found: " & Act);
         return Failed;
      end if;
   end;

   return Verify_Small_Bytes (Container, 0, (16#48#, 16#65#, 16#6C#, 16#6C#, 16#6F#, 16#00#, 16#00#));

end String_Access;

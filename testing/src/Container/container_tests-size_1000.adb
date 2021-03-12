separate (Container_Tests)

function Size_1000 (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Container: Byte_Store := Byte_Store_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);
begin

   if Container.Length /= 1000 then
      Put_Line ("Container length error, expected 1000, found " & Unsigned_32'Image(Container.Length));
      return Failed;
   end if;

   return Passed;

end Size_1000;

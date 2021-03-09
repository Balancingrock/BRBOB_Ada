separate (Container_Tests)

function Size_0 (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 0;
   Container: Storage_Area := Storage_Area_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);
begin

   if Container.Length /= 80 then
      Put_Line ("Container length error, expected 80, found " & Unsigned_32'Image(Container.Length));
      return Failed;
   end if;

   return Passed;

end Size_0;

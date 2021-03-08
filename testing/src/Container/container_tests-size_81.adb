separate (Container_Tests)

function Size_81 return Test_Result is

   Byte_Count: Unsigned_32 := 81;
   Container: Storage_Area := Storage_Area_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);
begin

   if Container.Length /= 88 then
      Put_Line ("Container length error, expected 88, found " & Unsigned_32'Image(Container.Length));
      return Failed;
   end if;

   return Passed;

end Size_81;

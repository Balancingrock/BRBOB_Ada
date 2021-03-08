separate (Container_Tests)

function Read_Write return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Container: Storage_Area := Storage_Area_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);
begin

   Container.

   return Passed;

end Read_Write;

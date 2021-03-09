separate (Container_Tests)

function UInt8_Access (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Container: Storage_Area := Storage_Area_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);


   function Verify (Location: Unsigned_32; Expected: Unsigned_8) return Test_Result is
      Actual: Unsigned_8 := Container.Get_Unsigned_8 (Location);
   begin
      if  Expected /= Actual then
         declare
            E_Str: String := Unsigned_8'Image (Expected);
            A_Str: String := Unsigned_8'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset 0, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
         end;
         return Failed;
      end if;
      return Passed;
   end Verify;


   function Test (Location: Unsigned_32; Value: Unsigned_8) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Location, 0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Container.Set_Unsigned_8 (Location, Value);
      Result := Verify (Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Container.Set_Unsigned_8 (Location, 0);
      Result := Verify (Location, 0);

      return Passed;

   end Test;



   Result: Test_Result;

begin

   Result := Test (0, 7);
   if Result = Failed then return Failed; end if;

   Result := Test (24, 5);
   if Result = Failed then return Failed; end if;

   Result := Test (999, 7);
   if Result = Failed then return Failed; end if;


   Container.Set_Unsigned_8 (Offset => 0, Value => 5);
   Container.Set_Unsigned_8 (Offset => 1, Value => 12);

   Result := Verify_Small_Bytes (Container, 0, (5, 12));

   return Result;

end UInt8_Access;

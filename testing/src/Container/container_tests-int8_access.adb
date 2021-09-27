separate (Container_Tests)

function Int8_Access (Count: in out Integer) return Test_Result is

   Container: Instance := Factory (Buffer'Access, Machine_Endianness);


   function Verify (Location: Unsigned_32; Expected: Integer_8) return Test_Result is
      Actual: Integer_8 := Container.Get_Integer_8 (Location);
   begin
      if  Expected /= Actual then
         declare
            E_Str: String := Integer_8'Image (Expected);
            A_Str: String := Integer_8'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset " & Unsigned_32'Image (Location));
         end;
         return Failed;
      end if;
      return Passed;
   end Verify;


   function Test (Location: Unsigned_32; Value: Integer_8) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Location, 0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Container.Set_Integer_8 (Location, Value);
      Result := Verify (Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Container.Set_Integer_8 (Location, 0);
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


   Container.Set_Integer_8 (Offset => 0, Value => 5);
   Container.Set_Integer_8 (Offset => 1, Value => 12);

   Result := Verify_Small_Bytes (Container, 0, (5, 12));

   return Result;

end Int8_Access;

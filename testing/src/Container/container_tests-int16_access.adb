separate (Container_Tests)

function Int16_Access (Count: in out Integer) return Test_Result is

   Big_Container: Instance := Factory (Buffer'Access, Big);
   Little_Container: Instance := Factory (LBuffer'Access, Little);
   TCount: Integer := 0;


   function Verify (Container: Instance; Location: Unsigned_32; Expected: Integer_16) return Test_Result is

      Actual: Integer_16 := Get_Integer_16 (Container, Location);

   begin

      if  Expected /= Actual then

         declare
            E_Str: String := Integer_16'Image (Expected);
            A_Str: String := Integer_16'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset " & Unsigned_32'Image (Location) & ", at TCount = " & Integer'Image (TCount));
         end;
         return Failed;

      end if;

      return Passed;

   end Verify;


   function Test (Container: in out Instance; Location: Unsigned_32; Value: Integer_16) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Container, Location, 0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Set_Integer_16 (Container, Location, Value);
      Result := Verify (Container, Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Set_Integer_16 (Container, Location, 0);
      Result := Verify (Container, Location, 0);

      return Passed;

   end Test;


   Result: Test_Result;

begin

   -- Big endian container

   TCount := 1;
   Result := Test (Big_Container, 0, 16#0123#);
   if Result = Failed then return Failed; end if;

   TCount := 2;
   Result := Test (Big_Container, 24, 16#4567#);
   if Result = Failed then return Failed; end if;

   TCount := 3;
   Result := Test (Big_Container, 998, -16#79AB#);
   if Result = Failed then return Failed; end if;


   Set_Integer_16 (Big_Container, Offset => 0, Value => 16#0123#);
   Set_Integer_16 (Big_Container, Offset => 2, Value => -16#4BCD#);

   TCount := 4;
   Result := Verify_Small_Bytes (Big_Container, 0, (16#01#, 16#23#, 16#AB#, 16#CD#));
   if Result = Failed then return Failed; end if;


   -- Little endian container

   TCount := 5;
   Result := Test (Little_Container, 0, 16#0123#);
   if Result = Failed then return Failed; end if;

   TCount := 6;
   Result := Test (Little_Container, 24, 16#4567#);
   if Result = Failed then return Failed; end if;

   TCount := 7;
   Result := Test (Little_Container, 998, -16#79AB#);
   if Result = Failed then return Failed; end if;


   Set_Integer_16 (Little_Container, Offset => 0, Value => 16#0123#);
   Set_Integer_16 (Little_Container, Offset => 2, Value => -16#4BCD#);

   TCount := 8;
   Result := Verify_Small_Bytes (Little_Container, 0, (16#23#, 16#01#, 16#CD#, 16#AB#));


   return Result;

end Int16_Access;

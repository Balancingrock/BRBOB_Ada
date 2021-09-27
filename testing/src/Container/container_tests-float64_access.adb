separate (Container_Tests)

function Float64_Access (Count: in out Integer) return Test_Result is

   Big_Container: Instance := Store_Factory (Buffer'Access, Big);
   Little_Container: Instance := Store_Factory (LBuffer'Access, Little);
   TCount: Integer := 0;


   function Verify (Container: Instance; Location: Unsigned_32; Expected: IEEE_Float_64) return Test_Result is

      Actual: IEEE_Float_64 := Container.Get_Float_64 (Location);

   begin

      if  Expected /= Actual then

         declare
            E_Str: String := IEEE_Float_64'Image (Expected);
            A_Str: String := IEEE_Float_64'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset " & Unsigned_32'Image (Location) & ", at TCount = " & Integer'Image (TCount));
         end;
         return Failed;

      end if;

      return Passed;

   end Verify;


   function Test (Container: in out Instance; Location: Unsigned_32; Value: IEEE_Float_64) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Container, Location, 0.0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Container.Set_Float_64 (Location, Value);
      Result := Verify (Container, Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Container.Set_Float_64 (Location, 0.0);
      Result := Verify (Container, Location, 0.0);

      return Passed;

   end Test;


   Result: Test_Result;

begin

   -- Big endian container

   TCount := 1;
   Result := Test (Big_Container, 0, 1.25E4);
   if Result = Failed then return Failed; end if;

   TCount := 2;
   Result := Test (Big_Container, 24, -4.2E-6);
   if Result = Failed then return Failed; end if;

   TCount := 3;
   Result := Test (Big_Container, 992, 3.6E11);
   if Result = Failed then return Failed; end if;


   Big_Container.Set_Float_64 (Offset => 0, Value => 2.2E2);
   Big_Container.Set_Float_64 (Offset => 8, Value => 3.3E3);

   TCount := 4;
   Result := Verify_Small_Bytes (Big_Container, 0, (16#40#, 16#6B#, 16#80#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#40#, 16#A9#, 16#C8#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
   if Result = Failed then return Failed; end if;


   -- Little endian container

   TCount := 5;
   Result := Test (Little_Container, 0, 1.25E4);
   if Result = Failed then return Failed; end if;

   TCount := 6;
   Result := Test (Little_Container, 24, -4.1E-6);
   if Result = Failed then return Failed; end if;

   TCount := 7;
   Result := Test (Little_Container, 992, 3.6E11);
   if Result = Failed then return Failed; end if;


   Little_Container.Set_Float_64 (Offset => 0, Value => 2.2E2);
   Little_Container.Set_Float_64 (Offset => 8, Value => 3.3E3);

   TCount := 8;
   Result := Verify_Small_Bytes (Little_Container, 0, (16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#80#, 16#6B#, 16#40#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#C8#, 16#A9#, 16#40#));


   return Result;

end Float64_Access;

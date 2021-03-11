separate (Container_Tests)

function Float32_Access (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Big_Container: Binary_Store := Binary_Store_Factory (Byte_Count, Big);
   Little_Container: Binary_Store := Binary_Store_Factory (Byte_Count, Little);
   TCount: Integer := 0;


   function Verify (Container: Binary_Store; Location: Unsigned_32; Expected: IEEE_Float_32) return Test_Result is

      Actual: IEEE_Float_32 := Container.Get_Float_32 (Location);

   begin

      if  Expected /= Actual then

         declare
            E_Str: String := IEEE_Float_32'Image (Expected);
            A_Str: String := IEEE_Float_32'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset " & Unsigned_32'Image (Location) & ", at TCount = " & Integer'Image (TCount));
         end;
         return Failed;

      end if;

      return Passed;

   end Verify;


   function Test (Container: in out Binary_Store; Location: Unsigned_32; Value: IEEE_Float_32) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Container, Location, 0.0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Container.Set_Float_32 (Location, Value);
      Result := Verify (Container, Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Container.Set_Float_32 (Location, 0.0);
      Result := Verify (Container, Location, 0.0);

      return Passed;

   end Test;



   Result: Test_Result;


begin

   -- Big endian container

   TCount := 1;
   Result := Test (Big_Container, 0, 3.7);
   if Result = Failed then return Failed; end if;

   TCount := 2;
   Result := Test (Big_Container, 24, -12.8E-5);
   if Result = Failed then return Failed; end if;

   TCount := 3;
   Result := Test (Big_Container, 996, 0.3E15);
   if Result = Failed then return Failed; end if;


   Big_Container.Set_Float_32 (Offset => 0, Value => 4.0);
   Big_Container.Set_Float_32 (Offset => 4, Value => 1.0);

   TCount := 4;
   Result := Verify_Small_Bytes (Big_Container, 0, (16#40#, 16#80#, 16#00#, 16#00#, 16#3F#, 16#80#, 16#00#, 16#00#));
   if Result = Failed then return Failed; end if;


   -- Little endian container

   TCount := 5;
   Result := Test (Little_Container, 0, 3.7);
   if Result = Failed then return Failed; end if;

   TCount := 6;
   Result := Test (Little_Container, 24, -12.8E-5);
   if Result = Failed then return Failed; end if;

   TCount := 7;
   Result := Test (Little_Container, 996, 0.3E15);
   if Result = Failed then return Failed; end if;


   Little_Container.Set_Float_32 (Offset => 0, Value => 4.0);
   Little_Container.Set_Float_32 (Offset => 4, Value => 1.0);

   TCount := 8;
   Result := Verify_Small_Bytes (Little_Container, 0, (16#00#, 16#00#, 16#80#, 16#40#, 16#00#, 16#00#, 16#80#, 16#3F#));


   return Result;

end Float32_Access;

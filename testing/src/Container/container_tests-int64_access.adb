separate (Container_Tests)

function Int64_Access (Count: in out Integer) return Test_Result is

   Big_Container: Instance := Factory (Buffer'Access, Big);
   Little_Container: Instance := Factory (LBuffer'Access, Little);
   TCount: Integer := 0;


   function Verify (Container: Instance; Location: Unsigned_32; Expected: Integer_64) return Test_Result is

      Actual: Integer_64 := Get_Integer_64 (Container, Location);

   begin

      if  Expected /= Actual then

         declare
            E_Str: String := Integer_64'Image (Expected);
            A_Str: String := Integer_64'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset " & Unsigned_32'Image (Location) & ", at TCount = " & Integer'Image (TCount));
         end;
         return Failed;

      end if;

      return Passed;

   end Verify;


   function Test (Container: in out Instance; Location: Unsigned_32; Value: Integer_64) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Container, Location, 0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Set_Integer_64 (Container, Location, Value);
      Result := Verify (Container, Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Set_Integer_64 (Container, Location, 0);
      Result := Verify (Container, Location, 0);

      return Passed;

   end Test;


   Result: Test_Result;

begin

   -- Big endian container

   TCount := 1;
   Result := Test (Big_Container, 0, 16#0123456789ABCDEF#);
   if Result = Failed then return Failed; end if;

   TCount := 2;
   Result := Test (Big_Container, 24, 16#4567456745674567#);
   if Result = Failed then return Failed; end if;

   TCount := 3;
   Result := Test (Big_Container, 992, 16#1111222233445566#);
   if Result = Failed then return Failed; end if;


   Set_Integer_64 (Big_Container, Offset => 0, Value => 16#0123456789ABCDEF#);
   Set_Integer_64 (Big_Container, Offset => 8, Value => 16#1122334455667788#);

   TCount := 4;
   Result := Verify_Small_Bytes (Big_Container, 0, (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#, 16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#));
   if Result = Failed then return Failed; end if;


   -- Little endian container

   TCount := 5;
   Result := Test (Little_Container, 0, 16#0123456789ABCDEF#);
   if Result = Failed then return Failed; end if;

   TCount := 6;
   Result := Test (Little_Container, 24, 16#4567456745674567#);
   if Result = Failed then return Failed; end if;

   TCount := 7;
   Result := Test (Little_Container, 992, 16#1111222233445566#);
   if Result = Failed then return Failed; end if;


   Set_Integer_64 (Little_Container, Offset => 0, Value => 16#0123456789ABCDEF#);
   Set_Integer_64 (Little_Container, Offset => 8, Value => 16#1122334455667788#);

   TCount := 8;
   Result := Verify_Small_Bytes (Little_Container, 0, (16#EF#, 16#CD#, 16#AB#, 16#89#, 16#67#, 16#45#, 16#23#, 16#01#, 16#88#, 16#77#, 16#66#, 16#55#, 16#44#, 16#33#, 16#22#, 16#11#));


   return Result;

end Int64_Access;

separate (Container_Tests)

function UInt32_Access (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Big_Container: Binary_Store := Binary_Store_Factory (Byte_Count, Big);
   Little_Container: Binary_Store := Binary_Store_Factory (Byte_Count, Little);
   TCount: Integer := 0;


   function Verify (Container: Binary_Store; Location: Unsigned_32; Expected: Unsigned_32) return Test_Result is

      Actual: Unsigned_32 := Container.Get_Unsigned_32 (Location);

   begin

      if  Expected /= Actual then

         declare
            E_Str: String := Unsigned_32'Image (Expected);
            A_Str: String := Unsigned_32'Image (Actual);
         begin
            Put_Line (" - Failed, Expected " & E_Str & ", found " & A_Str & " at offset " & Unsigned_32'Image (Location) & ", at TCount = " & Integer'Image (TCount));
         end;
         return Failed;

      end if;

      return Passed;

   end Verify;


   function Test (Container: in out Binary_Store; Location: Unsigned_32; Value: Unsigned_32) return Test_Result is

      Result: Test_Result;

   begin

      -- Step 1, verify zero

      Result := Verify (Container, Location, 0);
      if Result = Failed then return Failed; end if;

      -- Step 2, assignment & test

      Container.Set_Unsigned_32 (Location, Value);
      Result := Verify (Container, Location, Value);
      if Result = Failed then return Failed; end if;

      -- Undo the test

      Container.Set_Unsigned_32 (Location, 0);
      Result := Verify (Container, Location, 0);

      return Passed;

   end Test;



   Result: Test_Result;


begin

   -- Big endian container

   TCount := 1;
   Result := Test (Big_Container, 0, 16#01234567#);
   if Result = Failed then return Failed; end if;

   TCount := 2;
   Result := Test (Big_Container, 24, 16#456789AB#);
   if Result = Failed then return Failed; end if;

   TCount := 3;
   Result := Test (Big_Container, 996, 16#89ABCDEF#);
   if Result = Failed then return Failed; end if;


   Big_Container.Set_Unsigned_32 (Offset => 0, Value => 16#01234567#);
   Big_Container.Set_Unsigned_32 (Offset => 4, Value => 16#89ABCDEF#);

   TCount := 4;
   Result := Verify_Small_Bytes (Big_Container, 0, (16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#AB#, 16#CD#, 16#EF#));
   if Result = Failed then return Failed; end if;


   -- Little endian container

   TCount := 5;
   Result := Test (Little_Container, 0, 16#01234567#);
   if Result = Failed then return Failed; end if;

   TCount := 6;
   Result := Test (Little_Container, 24, 16#456789AB#);
   if Result = Failed then return Failed; end if;

   TCount := 7;
   Result := Test (Little_Container, 996, 16#89ABCDEF#);
   if Result = Failed then return Failed; end if;


   Little_Container.Set_Unsigned_32 (Offset => 0, Value => 16#01234567#);
   Little_Container.Set_Unsigned_32 (Offset => 4, Value => 16#89ABCDEF#);

   TCount := 8;
   Result := Verify_Small_Bytes (Little_Container, 0, (16#67#, 16#45#, 16#23#, 16#01#, 16#EF#, 16#CD#, 16#AB#, 16#89#));


   return Result;

end UInt32_Access;

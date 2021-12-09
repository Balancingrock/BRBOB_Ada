separate (Container_Tests)

function Array_Access (Count: in out Integer) return Test_Result is

   Container: Instance := Factory (Buffer'Access, Machine_Endianness);

   Arr: Array_Of_Unsigned_8 := (16#12#, 16#33#, 16#A6#);

begin

   Set_Unsigned_8_Array (Container, 0, Arr);

   declare
      Act: Array_Of_Unsigned_8 := Get_Unsigned_8_Array (Container, 0, 3);
   begin
      if Act /= Arr then
         Put_Line (" - Failed, Expected: " & Put_As_Line (Arr) & ", Found: " & Put_As_Line (Act));
         return Failed;
      end if;
   end;

   return Verify_Small_Bytes (Container, 0, (16#12#, 16#33#, 16#A6#, 16#00#));

end Array_Access;

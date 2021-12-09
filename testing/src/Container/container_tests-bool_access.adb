separate (Container_Tests)

function Bool_Access (Count: in out Integer) return Test_Result is

   Container: Instance := Factory (Buffer'Access, Machine_Endianness);

begin

   if Get_Bool (Container, Offset => 0) then
      Put_Line (" - Failed, Expected False, found True at offset 0, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
      return Failed;
   end if;

   if Get_Bool (Container, Offset => 24) then
      Put_Line (" - Failed, Expected False, found True at offset 24, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
      return Failed;
   end if;

      if Get_Bool (Container, Offset => 999) then
      Put_Line (" - Failed, Expected False, found True at offset 999, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
      return Failed;
   end if;

   Set_Bool (Container, Offset => 0,
                       Value  => True);

   Set_Bool (Container, Offset => 24,
                       Value  => True);

   Set_Bool (Container, Offset => 999,
                       Value  => True);

   if not Get_Bool (Container, Offset => 0) then
      Put_Line (" - Failed, Expected True, found False, at offset 0");
      return Failed;
   end if;

   if not Get_Bool (Container, Offset => 24) then
      Put_Line (" - Failed, Expected True, found False at offset 24");
      return Failed;
   end if;

   if not Get_Bool (Container, Offset => 999) then
      Put_Line (" - Failed, Expected True, found False at offset 999");
      return Failed;
   end if;

   Set_Bool (Container, Offset => 0,
                       Value  => False);

   Set_Bool (Container, Offset => 24,
                       Value  => False);

   Set_Bool (Container, Offset => 999,
                       Value  => False);

   if Get_Bool (Container, Offset => 0) then
      Put_Line (" - Failed, Expected False, found True at offset 0");
      return Failed;
   end if;

   if Get_Bool (Container, Offset => 24) then
      Put_Line (" - Failed, Expected False, found True at offset 24");
      return Failed;
   end if;

      if Get_Bool (Container, Offset => 999) then
      Put_Line (" - Failed, Expected False, found True at offset 999");
      return Failed;
   end if;

   Set_Bool (Container, Offset => 0, Value => True);
   Set_Bool (Container, Offset => 1, Value => False);

   declare
      subtype Comparable is Array_Of_Unsigned_8 (1 .. 2);
      Actual: Comparable;
      Expected: Comparable := (1, 0);
   begin
      Test_Support_Get_Bytes (Container, Start => 0, Dest => Actual);
      if Actual /= Expected then
         Put_Line (" - Failed, Expected: " & Put_As_Line (Expected) & ", Found: " & Put_As_Line (Actual));
         return Failed;
      end if;
   end;

   return Passed;

end Bool_Access;

separate (Container_Tests)

function Bool_Access (Count: in out Integer) return Test_Result is

   Byte_Count: Unsigned_32 := 1000;
   Container: Byte_Store := Byte_Store_Factory (Byte_Count       => Byte_Count,
                                                    Using_Endianness => Machine_Endianness);
begin

   if Container.Get_Bool (Offset => 0) then
      Put_Line (" - Failed, Expected False, found True at offset 0, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
      return Failed;
   end if;

   if Container.Get_Bool (Offset => 24) then
      Put_Line (" - Failed, Expected False, found True at offset 24, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
      return Failed;
   end if;

      if Container.Get_Bool (Offset => 999) then
      Put_Line (" - Failed, Expected False, found True at offset 999, make sure the Zero_New_Storage constant in BRBON.Configure is set to True");
      return Failed;
   end if;

   Container.Set_Bool (Offset => 0,
                       Value  => True);

   Container.Set_Bool (Offset => 24,
                       Value  => True);

   Container.Set_Bool (Offset => 999,
                       Value  => True);

   if not Container.Get_Bool (Offset => 0) then
      Put_Line (" - Failed, Expected True, found False, at offset 0");
      return Failed;
   end if;

   if not Container.Get_Bool (Offset => 24) then
      Put_Line (" - Failed, Expected True, found False at offset 24");
      return Failed;
   end if;

   if not Container.Get_Bool (Offset => 999) then
      Put_Line (" - Failed, Expected True, found False at offset 999");
      return Failed;
   end if;

   Container.Set_Bool (Offset => 0,
                       Value  => False);

   Container.Set_Bool (Offset => 24,
                       Value  => False);

   Container.Set_Bool (Offset => 999,
                       Value  => False);

   if Container.Get_Bool (Offset => 0) then
      Put_Line (" - Failed, Expected False, found True at offset 0");
      return Failed;
   end if;

   if Container.Get_Bool (Offset => 24) then
      Put_Line (" - Failed, Expected False, found True at offset 24");
      return Failed;
   end if;

      if Container.Get_Bool (Offset => 999) then
      Put_Line (" - Failed, Expected False, found True at offset 999");
      return Failed;
   end if;

   Container.Set_Bool (Offset => 0, Value => True);
   Container.Set_Bool (Offset => 1, Value => False);

   declare
      subtype Comparable is Array_Of_Unsigned_8 (1 .. 2);
      Actual: Comparable;
      Expected: Comparable := (1, 0);
   begin
      Container.Test_Support_Get_Bytes (Start => 0, Dest => Actual);
      if Actual /= Expected then
         Put_Line (" - Failed, Expected: " & Put_As_Line (Expected) & ", Found: " & Put_As_Line (Actual));
         return Failed;
      end if;
   end;

   return Passed;

end Bool_Access;

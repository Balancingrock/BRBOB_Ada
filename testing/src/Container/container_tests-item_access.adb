separate (Container_Tests)

function Item_Access (Count: in out Integer) return Test_Result is

   Container: Instance := Store_Factory (Buffer'Access , Machine_Endianness);

   Opt: BR_Item_Options := To_BR_Item_Options (16#45#);
   Flg: BR_Item_Flags := To_BR_Item_Flags (16#3E#);

begin

   if Container.Get_Unsigned_8 (0) /= 0 then
      Put_Line (" - Failed, container not empty");
      return Failed;
   end if;

   if Container.Valid_Item_Type (0) then
      Put_Line (" - Failed, 0 is not a valid BR_Item_Type");
      return Failed;
   end if;

   Container.Set_Item_Type (0, BR_Binary);

   if not Container.Valid_Item_Type (0) then
      Put_Line (" - Failed, expected a valid BR_Item_Type");
      return Failed;
   end if;

   if Container.Get_Item_Type (0) /= BR_Binary then
      Put_Line (" - Failed, expected BR_Binary");
      return Failed;
   end if;

   declare
      Inval: Unsigned_8 := To_Unsigned_8 (BR_Item_Type'Last) + 1;
      Inv: BR_Item_Type;
   begin
      Container.Set_Unsigned_8 (0, Unsigned_8 (Inval));
      if Container.Valid_Item_Type (0) then
         Put_Line (" - Failed, invalid item expected");
         return Failed;
      end if;
      Inv := Container.Get_Item_Type (0);
      Put_Line (" - Failed, expected an exception");
      return Failed;
   exception
      when BRBON.Illegal_Item_Type =>
         null;
   end;



   Container.Set_Item_Options (0, Opt);
   Container.Set_Item_Flags (1, Flg);

   if Container.Get_Item_Options (0) /= Opt then
      Put_Line (" - Failed, options do not match");
      return Failed;
   end if;

   if Container.Get_Item_Flags (1) /= Flg then
      Put_Line (" - Failed, flags do not match");
      return Failed;
   end if;

   return Verify_Small_Bytes (Container, 0, (16#45#, 16#3E#));

end Item_Access;

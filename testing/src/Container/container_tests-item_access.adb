separate (Container_Tests)

function Item_Access (Count: in out Integer) return Test_Result is

   Container: Instance := Factory (Buffer'Access , Machine_Endianness);

   Opt: Item_Options := To_Item_Options (16#45#);
   Flg: Item_Flags := To_Item_Flags (16#3E#);

begin

   if Container.Get_Unsigned_8 (0) /= 0 then
      Put_Line (" - Failed, container not empty");
      return Failed;
   end if;



   return Verify_Small_Bytes (Container, 0, (16#45#, 16#3E#));

end Item_Access;

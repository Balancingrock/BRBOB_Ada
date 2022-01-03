package body BRBON is





   -----------------------------------------------------------------------------
   
   procedure Set_Data_Byte_Order (B: Store; Value: Byte_Storage_Order) is
   
   begin
   
      B.Swap := Value /= BRBON.Machine_Byte_Storage_Order;
   
   end Set_Data_Byte_Order;

end BRBON;

with BRBON.Item;
with BRBON.Configure;

package body BRBON.Portal is

   Null_Portal_Instance: Instance;

   function Factory
     (
      For_Container: Container.Instance;
      Using_Item_Offset: Unsigned_32;
      Of_Type: Instance_Type := Normal;
      Element_Index: Unsigned_32 := 0;
      Column_Index: Unsigned_32 := 0
      ) return Instance is
   begin
      return (For_Container, Using_Item_Offset, Of_Type, Element_Index, Column_Index);
   end Factory;


   function Null_Portal return Instance is
   begin
      return Null_Portal_Instance;
   end Null_Portal;


   function Small_Value_Offset (P: Instance) return Unsigned_32 is
   begin
      return P.Item_Offset + Item.Small_Value_Offset;
   end Small_Value_Offset;


   function Value_Offset (P: Instance) return Unsigned_32 is
   begin
      return Item.Value_Offset (P.Container, P.Item_Offset);
   end Value_Offset;


   function Is_Null_Portal (P: Instance) return Boolean is
   begin
      raise Implementation;
      return False;
   end Is_Null_Portal;


   Ptr: Array_Of_Unsigned_8_Ptr;
   Con: Container.Instance;

begin

   Ptr := new Array_Of_Unsigned_8 (1 ..1);
   Con := BRBON.Container.Factory (Ptr, BRBON.Configure.Machine_Endianness);
   Null_Portal_Instance := (Con, 0, Normal, 0, 0);

end BRBON.Portal;

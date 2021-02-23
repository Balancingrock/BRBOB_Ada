with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with Interfaces.C;
with Interfaces; use Interfaces;

with BRBON; use BRBON;


package Pointer_Math is


   package Ptr_Math is new Interfaces.C.Pointers (Index              => Unsigned_32,
                                                  Element            => Unsigned_8,
                                                  Element_Array      => Array_Of_Unsigned_8,
                                                  Default_Terminator => 0);
   use Ptr_Math;

   function To_Unsigned_8_Ptr is new Ada.Unchecked_Conversion (Ptr_Math.Pointer, Unsigned_8_Ptr);
   function To_Ptr_Math_Pointer is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, Ptr_Math.Pointer);

   function "+" (Left: in Unsigned_8_Ptr; Right: in Unsigned_32) return Unsigned_8_Ptr is (To_Unsigned_8_Ptr (To_Ptr_Math_Pointer(Left) + Interfaces.C.ptrdiff_t(Right)));
   pragma Inline("+");

end Pointer_Math;

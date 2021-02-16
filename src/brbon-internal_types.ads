with Interfaces.C.Pointers;
with Interfaces.C;
with Brbon_Basic_Types; use Brbon_Basic_Types;
with Ada.Unchecked_Conversion;

package BRBON.Internal_Types is


   -- Flags associated with stored items (currently unused)
   --
   type Item_Flags is new Bits_8;



   -- Supporting pointer arithmetic

   type Array_For_Ptr_Math is array (Unsigned_32 range <>) of aliased Unsigned_8;

   package PtrIf is new Interfaces.C.Pointers (Index              => Unsigned_32,
                                               Element            => Unsigned_8,
                                               Element_Array      => Array_For_Ptr_Math,
                                               Default_Terminator => 0);
   use PtrIf;

   function To_Unsigned_8_Ptr is new Ada.Unchecked_Conversion (PtrIf.Pointer, Unsigned_8_Ptr);
   function To_PtrIf_Pointer is new Ada.Unchecked_Conversion (Unsigned_8_Ptr, PtrIf.Pointer);

   function "+" (Left: in Unsigned_8_Ptr; Right: in Unsigned_32) return Unsigned_8_Ptr is (To_Unsigned_8_Ptr (To_PtrIf_Pointer(Left) + Interfaces.C.ptrdiff_t(Right)));
   pragma Inline("+");


end BRBON.Internal_Types;

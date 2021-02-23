with Interfaces; use Interfaces;
with Ada.Strings.Bounded;

package Font_Package is

   package Bounded_String_256 is new Ada.Strings.Bounded.Generic_Bounded_Length (256);

   type Font is
      record
         Points: IEEE_Float_32;
         Family: Bounded_String_256.Bounded_String;
         Name: Bounded_String_256.Bounded_String;
      end record;

   Default_Family: Bounded_String_256.Bounded_String := Bounded_String_256.To_Bounded_String("Courier");
   Default_Name: Bounded_String_256.Bounded_String := Bounded_String_256.To_Bounded_String("Courier");
   Default_Font: constant Font := (10.0, Default_Family, Default_Name);

end Font_Package;

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;


package Color_Package is

   type Color is private;

   function Color_Factory (Red: Unsigned_8; Green: Unsigned_8; Blue: Unsigned_8; Alpha: Unsigned_8) return Color;

   function Get_Red_Component (C: Color) return Unsigned_8;
   function Get_Green_Component (C: Color) return Unsigned_8;
   function Get_Blue_Component (C: Color) return Unsigned_8;
   function Get_Alpha_Component (C: Color) return Unsigned_8;

   function Image (C: Color) return String;

   function None return Color;    -- 00000000
   function Black return Color;   -- 000000FF
   function Red return Color;     -- FF0000FF
   function Green return Color;   -- 00FF00FF
   function Blue return Color;    -- 0000FFFF
   function Yellow return Color;  -- FFFF00FF
   function Magenta return Color; -- FF00FFFF
   function Cyan return Color;    -- 00FFFFFF
   function White return Color;   -- FFFFFFFF

   function "=" (lhs, rhs: Color) return Boolean;

private

   type Color is
      record
         Red: Unsigned_8;
         Green: Unsigned_8;
         Blue: Unsigned_8;
         Alpha: Unsigned_8;
      end record;
   --
   for Color use
      record
         Red   at 0 range 0..7;
         Green at 1 range 0..7;
         Blue  at 2 range 0..7;
         Alpha at 3 range 0..7;
      end record;
   --
   for Color'Size use 32;
   --
   function To_Unsigned_32 is new Ada.Unchecked_Conversion (Color, Unsigned_32);
   function To_Color is new Ada.Unchecked_Conversion (Unsigned_32, Color);

   Color_None:     constant Color := (16#00#, 16#00#, 16#00#, 16#00#);
   Color_Black:    constant Color := (16#00#, 16#00#, 16#00#, 16#FF#);
   Color_Red  :    constant Color := (16#FF#, 16#00#, 16#00#, 16#FF#);
   Color_Green:    constant Color := (16#00#, 16#FF#, 16#00#, 16#FF#);
   Color_Blue :    constant Color := (16#00#, 16#00#, 16#FF#, 16#FF#);
   Color_Yellow:   constant Color := (16#FF#, 16#FF#, 16#00#, 16#FF#);
   Color_Magenta:  constant Color := (16#00#, 16#FF#, 16#FF#, 16#FF#);
   Color_Cyan:     constant Color := (16#FF#, 16#FF#, 16#FF#, 16#FF#);
   Color_White:    constant Color := (16#FF#, 16#FF#, 16#FF#, 16#FF#);

end Color_Package;

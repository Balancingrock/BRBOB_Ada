package body Color_Package is

function Factory (Red: Unsigned_8; Green: Unsigned_8; Blue: Unsigned_8; Alpha: Unsigned_8) return Color is
begin
   return (Red, Green, Blue, Alpha);
end Factory;

function Get_Red_Component (C: Color) return Unsigned_8 is begin return C.Red; end Get_Red_Component;
function Get_Green_Component (C: Color) return Unsigned_8 is begin return C.Green; end Get_Green_Component;
function Get_Blue_Component (C: Color) return Unsigned_8 is begin return C.Blue; end Get_Blue_Component;
function Get_Alpha_Component (C: Color) return Unsigned_8 is begin return C.Alpha; end Get_Alpha_Component;

function Black return Color is begin return Color_Black; end Black;
function Red return Color is begin return Color_Red; end Red;
function Green return Color is begin return Color_Green; end Green;
function Blue return Color is begin return Color_Blue; end Blue;
function Yellow return Color is begin return Color_Yellow; end Yellow;
function Magenta return Color is begin return Color_Magenta; end Magenta;
function Cyan return Color is begin return Color_Cyan; end Cyan;
function White return Color is begin return Color_White; end White;

end Color_Package;

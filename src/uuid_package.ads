with BRBON.Types; use BRBON.Types;


package UUID_Package is

   type UUID is new Array_Of_Unsigned_8 (1 .. 16);

   Null_UUID: constant UUID := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

end UUID_Package;

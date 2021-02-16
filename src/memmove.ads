with System; use System;
with Interfaces.C; use Interfaces.C;

function memmove (dest: Address; src: Address; num: size_t) return Address
  with
    Import     => True,
    Convention => C;

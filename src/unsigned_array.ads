--------------------------------------------------------------------------------
-- FILE   : unsigned_array.ads
-- SUBJECT: Specification of a package for [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;

package Unsigned_Array is

   -- A packed array of eight bit values.
   type Unsigned_8_Array is array(Natural range <>) of Unsigned_8
     with Component_Size => 8;

   -- A packed array of 32 bit values.
   type Unsigned_32_Array is array(Natural range <>) of Unsigned_32
     with Component_Size => 32;

end Unsigned_Array;

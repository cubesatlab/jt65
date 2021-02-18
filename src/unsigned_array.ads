--------------------------------------------------------------------------------
-- FILE   : unsigned_array.ads
-- SUBJECT: Specification of a package for [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;

package Unsigned_Array is

   type Unsigned_32_Array is array (Natural range <>) of Unsigned_32;
   type Unsigned_8_Array  is array (Natural range <>) of Unsigned_8;

end Unsigned_Array;

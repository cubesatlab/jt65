--------------------------------------------------------------------------------
-- FILE   : init_rs.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;

package Init_Rs is

   type Unsigned_Array is
     array(Natural range 0 .. 63) of Unsigned_8;
   type Unsigned_Array_2 is
     array(Natural range 0 .. 51) of Unsigned_8;

   type Rs is
      record
         Mm : Unsigned_8 := 0;
         Nn : Unsigned_8 := 0;
         Alpha_To : Unsigned_Array := (others => 0);
         Index_Of : Unsigned_Array := (others => 0);
         Genpoly : Unsigned_Array_2 := (others => 0);
         Nroots : Unsigned_8 := 0;
         Fcr : Unsigned_8 := 0;
         Prim : Unsigned_8 := 0;
         Iprim : Unsigned_8 := 0;
         Pad : Unsigned_8 := 0;
      end record;

   type Rs_Access is
     access Rs;

   Reed_S : Rs_access;

   function Init_Rs_Int ( Symsize : in Unsigned_8;
                          Gfpoly : in Unsigned_8;
                          Fcr : in Unsigned_8;
                          Prim : in Unsigned_8;
                          Nroots : in Unsigned_8;
                           Pad : in Unsigned_8) return Rs_Access
     with
       Volatile_Function,
       Pre => Fcr >= 0 and Prim >= 0 and Nroots >= 0 and Pad >= 0 and Gfpoly > 0
               and Symsize >= 0 and Prim <= 255 and Fcr <= 255 and Nroots <= 255
                                      and Gfpoly /= 0 and Gfpoly <= 255 and Symsize <= 6;

   function Modnn ( Rs : in Rs_Access;
                    A : in Unsigned_8) return Integer
     with
       Pre =>  Rs /= null
       and then (Rs.Mm >= 0 and Rs.Nn >= 0 and Rs.Nroots >= 0 and Rs.Fcr >= 0
                 and Rs.Prim >= 0 and Rs.Iprim >= 0 and Rs.Pad >= 0
                 and Rs.Nn <= 255 and A <= 255);



end Init_Rs;


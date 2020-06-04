--------------------------------------------------------------------------------
-- FILE   : init_rs.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with System.Memory; use System.Memory;

package Init_Rs is

   type Unsigned_Array is
     array(Natural range 0 .. 63) of Integer;
   type Unsigned_Array_2 is
     array(Natural range 0 .. 51) of Integer;

   type Rs is
      record
         Mm : Integer := 0;
         Nn : Integer := 0;
         Alpha_To : Unsigned_Array := (others => 0);
         Index_Of : Unsigned_Array := (others => 0);
         Genpoly : Unsigned_Array_2 := (others => 0);
         Nroots : Integer := 0;
         Fcr : Integer := 0;
         Prim : Integer := 0;
         Iprim : Integer := 0;
         Pad : Integer := 0;
      end record;

   type Rs_Access is
     access Rs;

   function Init_Rs_Int ( Symsize : in Integer;
                          Gfpoly : in Integer;
                          Fcr : in Integer;
                          Prim : in Integer;
                          Nroots : in Integer;
                          Pad : in Integer) return Rs_Access
     with Pre => Fcr >= 0 and Prim >= 0 and Nroots >= 0 and Pad >= 0 and Gfpoly > 0 and Symsize >= 0;

   function Modnn ( Rs : in Rs_Access;
                    A : in Integer) return Integer;

end Init_Rs;


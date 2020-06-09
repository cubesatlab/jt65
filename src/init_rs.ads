--------------------------------------------------------------------------------
-- FILE   : init_rs.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package Init_Rs is

   type Int_Array is
     array(Natural range 0 .. 63) of Integer;
   type Int_Array_2 is
     array(Natural range 0 .. 51) of Integer;

   type Rs is
      record
         Mm : Integer := 0;
         Nn : Integer := 0;
         Alpha_To : Int_Array := (others => 0);
         Index_Of : Int_Array := (others => 0);
         Genpoly : Int_Array_2 := (others => 0);
         Nroots : Integer := 0;
         Fcr : Integer := 0;
         Prim : Integer := 0;
         Iprim : Integer := 0;
         Pad : Integer := 0;
      end record;

   type Rs_Access is
     access Rs;

   Reed_S : Rs_access;

   function Init_Rs_Int ( Symsize : in Integer;
                          Gfpoly : in Integer;
                          Fcr : in Integer;
                          Prim : in Integer;
                          Nroots : in Integer;
                           Pad : in Integer) return Rs_Access
     with
       Volatile_Function,
       Pre => Fcr >= 0 and Prim >= 0 and Nroots >= 0 and Pad >= 0 and Gfpoly > 0
               and Symsize >= 0 and Prim <= 255 and Fcr <= 255 and Nroots <= 255
                                      and Gfpoly /= 0 and Gfpoly <= 255 and Symsize <= 6;

   function Modnn ( Rs : in Rs_Access;
                    A : in Integer) return Integer
     with
       Pre =>  Rs /= null
       and then (Rs.Mm >= 0 and Rs.Nn >= 0 and Rs.Nroots >= 0 and Rs.Fcr >= 0
                 and Rs.Prim >= 0 and Rs.Iprim >= 0 and Rs.Pad >= 0
                 and Rs.Nn < 256 and A < 256);



end Init_Rs;


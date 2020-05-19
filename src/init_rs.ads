--------------------------------------------------------------------------------
-- FILE   : init_rs.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Unchecked_Deallocation;

package Init_Rs is

   type Data_T is new Integer;
   type Data_T_Access is access Data_T;

   type Unsigned_Array is array(Natural range 0 .. 63) of Integer;
   type Unsigned_Array_2 is array(Natural range 0 .. 51) of Integer;

   type Rs is
      record
         Mm : Integer;
         Nn : Integer;
         Alpha_To : Unsigned_Array;
         Index_Of : Unsigned_Array;
         Genpoly : Unsigned_Array_2;
         Nroots : Integer;
         Fcr : Integer;
         Prim : Integer;
         Iprim : Integer;
         Pad : Integer;
      end record;

   type Rs_Access is access Rs;


   function Init_Rs_Int ( Symsize : in out Integer; Gfpoly : in out Integer; Fcr : in out Integer;
                          Prim : in out Integer; Nroots : in out Integer; Pad : in out Integer)
                         return Rs_Access;

   function Modnn ( Rs : in Rs_Access; X : in out Integer) return Integer;

end Init_Rs;

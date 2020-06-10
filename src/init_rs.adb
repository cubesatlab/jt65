--------------------------------------------------------------------------------
-- FILE   : init_rs.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

--with Interfaces; use Interfaces;

package body Init_Rs is

   function Init_Rs_Int ( Symsize : in Unsigned_32;
                          Gfpoly : in Unsigned_32;
                          Fcr : in Unsigned_32;
                          Prim : in Unsigned_32;
                          Nroots : in Unsigned_32;
                          Pad : in Unsigned_32) return Rs_Access
   is

      Rs_Ptr : constant Rs_Access := new Rs;
      X, Sr, Root, Iprim : Unsigned_32;
      Prop : Boolean;

      Rs_Null : constant Rs_Access := new Rs;

      Placeholder : Unsigned_32;
      One : constant Unsigned_32 := 1;

   begin

      Rs_Ptr.Mm := Symsize;
      Placeholder := Shift_Left(1,Integer(Symsize))-1;
      Rs_Ptr.Nn := Placeholder;
      Rs_Ptr.Pad := Pad;

      Rs_Ptr.Index_Of(0) := Rs_Ptr.Nn;
      Rs_Ptr.Alpha_To(Integer(Rs_Ptr.Nn)) := 0;

      Sr := 1;
      Prop := True;
      for I in 0 .. Rs_Ptr.Nn-1 loop
         --if Sr /= 0  and I <= Unsigned_32'Last and Sr <= Unsigned_32'Last then
            Rs_Ptr.Index_Of(Integer(Sr)) := I;
            Rs_Ptr.Alpha_To(Integer(I)) := Sr;
            Sr := Shift_Left(Sr,1);
            if (Sr and Shift_Left(One,Integer(Symsize))) /= 0 then
               Sr := Sr xor Gfpoly;
            end if;
            Sr := Sr and Rs_Ptr.Nn;
         --end if;
         pragma Loop_Invariant(if Sr > 0 then Prop);
      end loop;

      if Sr /= 1 then
         return Rs_Null;
      else

         Rs_Ptr.Fcr := Fcr;
         Rs_Ptr.Prim := Prim;
         Rs_Ptr.Nroots := Nroots;

         Iprim := 1;
         Prop := True;
         loop
            if Iprim mod Prim = 0 then
               exit;
            else
               Iprim := Iprim + Rs_Ptr.Nn;
            end if;
            pragma Loop_Invariant(if Sr > 0 then Prop);
         end loop;

         Rs_Ptr.Iprim := Iprim / Prim;
         Rs_Ptr.Genpoly(0) := 1;
         Root := Fcr * Prim;

         for I in 0 .. 50 loop
            Rs_Ptr.Genpoly(I+1) := 1;
            for J in reverse 0 .. I loop
               if J > 0 then
                  if Rs_Ptr.Genpoly(J) /= 0 then
                     X := Rs_Ptr.Index_Of(Integer(Rs_Ptr.Genpoly(J))) + Root;
                     Rs_Ptr.Genpoly(J) := Rs_Ptr.Genpoly(J-1) xor
                                              Rs_Ptr.Alpha_To(Modnn(Rs_Ptr,X));
                  else
                     Rs_Ptr.Genpoly(J) := Rs_Ptr.Genpoly(J-1);
                  end if;
               end if;
            end loop;
            X := Rs_Ptr.Index_Of(Integer(Rs_Ptr.Genpoly(0))) + Root;
            if X < 256 then
               Rs_Ptr.Genpoly(0) := Rs_Ptr.Alpha_To(Modnn(Rs_Ptr,X));
            end if;
            Root := Root + Prim;
         end loop;
         Prop := True;
         for I in 0 .. Nroots loop
            Rs_Ptr.Genpoly(Integer(I)) := Rs_Ptr.Index_Of(Integer(Rs_Ptr.Genpoly(Integer(I))));
            pragma Loop_Invariant(if Sr > 0 then Prop);
         end loop;
      end if;
      return Rs_Ptr;
   end Init_Rs_Int;

   function Modnn ( Rs : in Rs_Access;
                    A : in Unsigned_32) return Integer
   is
      X : Unsigned_32 := A;
   begin
      while X >= Rs.Nn loop
         X := X - Rs.Nn;
         X := Shift_Right(X, Integer(Rs.Mm))
                      + (X and Rs.Nn);
      end loop;
      return Integer(X);
   end Modnn;

end Init_Rs;

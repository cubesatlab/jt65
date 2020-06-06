--------------------------------------------------------------------------------
-- FILE   : init_rs.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;

package body Init_Rs is

   function Init_Rs_Int ( Symsize : in Integer;
                          Gfpoly : in Integer;
                          Fcr : in Integer;
                          Prim : in Integer;
                          Nroots : in Integer;
                          Pad : in Integer) return Rs_Access
   is

      Rs_Ptr :  constant Rs_Access := new Rs;
      X, Sr, Root, Iprim : Integer;

      Rs_Null_One,
      Rs_Null_Two,
      Rs_Null_Three,
      Rs_Null_Four,
      Rs_Null_Five,
      Rs_Null_Six : constant Rs_Access := new Rs;

      Placeholder : Unsigned_8;
      One : constant Unsigned_8 := 1;

   begin
      if Symsize < 0 or Symsize > Integer(8*Unsigned_8'Size) then
         return Rs_Null_One;
      end if;

      if Fcr < 0 or Unsigned_8(Fcr) >= Shift_Left(1,Symsize) then
           return Rs_Null_Two;
      end if;

      if Prim <= 0 or Unsigned_8(Prim) >= Shift_Left(1,Symsize) then
         return Rs_Null_Three;
      end if;

      if Nroots < 0 or Unsigned_8(Nroots) >= Shift_Left(1,Symsize) then
         return Rs_Null_Four;
      end if;

      if Pad < 0 or Unsigned_8(Pad) >=
        Shift_Left(1,Symsize) - 1 - Unsigned_8(Nroots)  then
         return Rs_Null_Five;
      end if;

      Rs_Ptr.Mm := Symsize;
      Placeholder := Shift_Left(1,Symsize)-1;
      Rs_Ptr.Nn := Integer(Placeholder);
      Rs_Ptr.Pad := Pad;

      Rs_Ptr.Index_Of(0) := Rs_Ptr.Nn;
      Rs_Ptr.Alpha_To(Rs_Ptr.Nn) := 0;

      Sr := 1;
      for I in 0 .. Rs_Ptr.Nn-1 loop
         Rs_Ptr.Index_Of(Sr) := I;
         Rs_Ptr.Alpha_To(I) := Sr;
         Sr := Integer(Shift_Left(Unsigned_8(Sr),1));
         if (Unsigned_8(Sr) and Shift_Left(One,Symsize)) /= 0 then
            Sr := Integer(Unsigned_8(Sr) xor Unsigned_8(Gfpoly));
         end if;
         Sr := Integer(Unsigned_8(Sr) and Unsigned_8(Rs_Ptr.Nn));
      end loop;

      if Sr /= 1 then
         Return Rs_Null_Six;
      end if;

      Rs_Ptr.Fcr := Fcr;
      Rs_Ptr.Prim := Prim;
      Rs_Ptr.Nroots := Nroots;

      Iprim := 1;
      loop
         if Iprim mod Prim = 0 then
            exit;
         end if;
         Iprim := Iprim + Rs_Ptr.Nn;
      end loop;

      Rs_Ptr.Iprim := Iprim / Prim;
      Rs_Ptr.Genpoly(0) := 1;
      Root := Fcr * Prim;

      for I in 0 .. 50 loop
         Rs_Ptr.Genpoly(I+1) := 1;
         for J in reverse 0 .. I loop
            if J > 0 then
               if Rs_Ptr.Genpoly(J) /= 0 then
                 X := Rs_Ptr.Index_Of(Rs_Ptr.Genpoly(J)) + Root;
               Rs_Ptr.Genpoly(J) := Integer(Unsigned_8(Rs_Ptr.Genpoly(J-1)) xor
                                              Unsigned_8(Rs_Ptr.Alpha_To(Modnn(Rs_Ptr,X))));
               else
               Rs_Ptr.Genpoly(J) := Rs_Ptr.Genpoly(J-1);
               end if;
            end if;
         end loop;
         X := Rs_Ptr.Index_Of(Rs_Ptr.Genpoly(0)) + Root;
         Rs_Ptr.Genpoly(0) := Rs_Ptr.Alpha_To(Modnn(Rs_Ptr,X));
         Root := Root + Prim;
      end loop;
      for I in 0 .. Nroots loop
         Rs_Ptr.Genpoly(I) := Rs_Ptr.Index_Of(Rs_Ptr.Genpoly(I));
      end loop;
      return Rs_Ptr;
   end Init_Rs_Int;

   function Modnn ( Rs : in Rs_Access;
                    A : in Integer) return Integer
   is
      X : Integer := A;
   begin
      while X >= Rs.Nn loop
         X := X - Rs.Nn;
         X := Integer(Shift_Right(Unsigned_8(X), Rs.Mm)
                      + (Unsigned_8(X) and Unsigned_8(Rs.Nn)));
      end loop;
      return X;
   end Modnn;

end Init_Rs;

--------------------------------------------------------------------------------
-- FILE   : init_rs.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body Init_Rs is

   function Init_Rs_Int ( Symsize : in out Integer; Gfpoly : in out Integer; Fcr : in out Integer;
                          Prim : in out Integer; Nroots : in out Integer; Pad : in out Integer)
                         return Rs_Access is

      Rs_Ptr :  constant Rs_Access := new Rs;
      I, J, Sr, Root, Iprim : Integer;
      Rs_Null : constant Rs_Access := null;
      Symsize_8 : Unsigned_8;
      Sr_8 : Unsigned_8;
      Placeholder : Unsigned_8;

   begin
      --Init_Rs_Int(6, 16#43#, 3, 1, 51, 0);
      Symsize_8 := Unsigned_8(Symsize);

      if Symsize < 0 or Symsize > Integer(8*Unsigned_8'Size) then
         return Rs_Null;
      end if;

      if Fcr < 0 then
         if Unsigned_8(Fcr) >= Shift_Left(1,Symsize) then
            return Rs_Null;
         end if;
         return Rs_Null;
      end if;

      if Prim <= 0 then
         if Unsigned_8(Prim) >= Shift_Left(1,Symsize) then
            return Rs_Null;
         end if;
         return Rs_Null;
      end if;

      if Nroots <= 0 then
         if Unsigned_8(Nroots) >= Shift_Left(1,Symsize) then
            return Rs_Null;
         end if;
         return Rs_Null;
      end if;

      if Pad <= 0 then
         if Unsigned_8(Pad) >= Shift_Left(1,Symsize) - 1 - Unsigned_8(Nroots) then
            return Rs_Null;
         end if;
         return Rs_Null;
      end if;

      Rs_Ptr.Mm := Symsize;
      Placeholder := Shift_Left(1,Symsize)-1;
      Rs_Ptr.Nn := Integer(Placeholder);
      Rs_Ptr.Pad := Pad;

      Rs_Ptr.Index_Of(0) := Rs_Ptr.Nn;
      Rs_Ptr.Alpha_To(Rs_Ptr.Nn) := 0;

      Sr := 1;

      for I in 1 .. Rs_Ptr.Nn loop
         Rs_Ptr.Index_Of(Sr) := I;
         Rs_Ptr.Alpha_To(I) := Sr;
         --Placeholder := Unsigned_8(Sr);
         Sr := Integer(Shift_Left(Symsize_8,1));
         Sr_8 := Unsigned_8(Sr);
         if Unsigned_64(Unsigned_64(Sr) and Shift_Left(1,Symsize)) then
            Sr := Sr xor Gfpoly;
         end if;
         Sr := Sr and Rs_Ptr.Nn;
      end loop;







      return Rs_Ptr;
   end Init_Rs_Int;

   function Modnn ( Rs : in Rs_Access; X : in out Integer) return Integer is
      X_Convert : Unsigned_8;
      Nn_Convert : Unsigned_8;
   begin
      Nn_Convert := Rs.Nn;
      X_Convert := X;
      while X >= Rs.Nn loop
         X_Convert := X_Convert - Nn_Convert;
         X_Convert := Shift_Right( X_Convert, Nn_Convert);
         null;
      end loop;
      return X;
   end Modnn;

end Init_Rs;

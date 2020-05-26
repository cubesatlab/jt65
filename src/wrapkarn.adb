--------------------------------------------------------------------------------
-- FILE   : wrapkarn.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
--pragma SPARK_Mode(On);

with Init_Rs; use Init_Rs;
with Ada.Text_IO; use Ada.Text_IO;

package body Wrapkarn is


   procedure Encode_Rs_Int( Rs : in out Rs_Access; Data : in out Integer_Array; Bb : in out Integer_Array_Aliased ) is

      Feedback, X : Integer;
      Nroots : Integer := 51;
      Rs_Null : constant Rs_Access := null;
   begin
      if Rs /= Rs_Null then

         Bb(0 .. 50) := (others => 0);
         for I in 0 .. 11 loop
            Feedback := Rs.Index_Of(Integer(Unsigned_8(Data(I)) xor Unsigned_8(Bb(0))));
            if Feedback /= Rs.Nn then
               for J in 1 .. 50 loop
                  X := Feedback + Rs.Genpoly(Nroots-J);
                  Bb(J) := Integer(Unsigned_8(Bb(J)) xor Unsigned_8(Rs.Alpha_To(Modnn(Rs, X))));
               end loop;
            end if;
            Bb(0 .. Nroots - 2) := Bb(1 .. Nroots - 1);
            if Feedback /= Rs.Nn then
               X := Feedback + Rs.Genpoly(0);
               Bb(Rs.Nroots-1) := Rs.Alpha_To(Modnn(Rs, X));
            else
               Bb(51-1) := 0;
            end if;
         end loop;
      end if;
   end Encode_Rs_Int;


   function Decode_Rs_Int( Rs : in out Rs_Access; Data : in out Integer_Array;
                           Eras_Pos : in out Eras_Pos_Access; No_Eras : in out Integer )
                          return Integer is

      procedure Finish( Eras_Pos1 : in out Eras_Pos_Access; Counter : in Integer;
                        Loc1 : in Integer_Array) is
         Eras_Pos_Null : constant Eras_Pos_Access := null;
      begin
         if Eras_Pos1 /= Eras_Pos_Null then
            for I in 0 .. Counter - 1 loop
               Eras_Pos1(I) := Loc1(I);
            end loop;
         end if;
      end Finish;

      Nroots : Integer := 51;
      Deg_Lambda, El, Deg_Omega : Integer;
      --I, J, R, K : Integer;
      U, Q, Tmp, Num1, Num2, Den, Discr_R, X : Integer;

      -- Err + Eras Locator Poly and syndrome poly
      Lambda : Integer_Array(0 .. Nroots + 1);
      S : Integer_Array(0 .. Nroots - 1);

      B, T, Omega, Reg : Integer_Array(0 .. Nroots + 1);
      Root, Loc : Integer_Array(0 .. Nroots);
      Syn_Error, Count : Integer;
      Rs_Null : constant Rs_Access := null;
   begin
      if Rs /= Rs_Null then
         for I in 0 .. Nroots - 1 loop
            S(I) := Data(0);
         end loop;

         for J in 1 .. Rs.Nn - Rs.Pad-1 loop
            for I in 0 .. Nroots-1 loop
               if S(I) = 0 then
                  S(I) := Data(J);
               else
                  X := Rs.Index_Of(S(I)) + ( Rs.Fcr + I)* Rs.Prim;
                  S(I) := Integer(Unsigned_8(Data(J)) xor Unsigned_8(Rs.Alpha_To(Modnn(Rs, X))));
               end if;
            end loop;
         end loop;

         -- Convert syndromes to index form, checking for nonzero condition
         Syn_Error := 0;
         for I in 0 .. Nroots - 1 loop
            Syn_Error := Integer(Unsigned_8(Syn_Error) or Unsigned_8(S(I)));
            S(I) := Rs.Index_Of(S(I));
         end loop;

         if Syn_Error = 0 then
            -- If syndrome is zero, data() is a codeword and there are no errors
            -- to corret. Return Data() unmodified.
            Count := 0;
            Finish(Eras_Pos, Count, Loc);
            return Count;
         end if;
         Lambda(0 .. 50) := (others=>0);
         Lambda(0) := 1;
         return 1;
      end if;
      return 0;
   end Decode_Rs_Int;


   procedure Rs_Encode( Dgen : in out Integer_Array; Sent : in out Integer_Array ) is

      Dat1 : Integer_Array(0..11);
      B : Integer_Array_Aliased(0 .. 50);
      Symsize : Integer := 6;
      Gfpoly : Integer := 16#43#;
      Fcr : Integer := 3;
      Prim : Integer  := 1;
      Nroots : Integer := 51;
      Pad : Integer := 0;
   begin

      if(First) then
         -- Initialize the JT65 codec
         Reed_S := Init_Rs_Int(Symsize, Gfpoly, Fcr, Prim, Nroots, Pad);
         First := False;
      end if;

      -- Reverse data order for the Karn codec
      for I in 0 .. 11 loop
         Dat1(I) := Dgen(11-I);
      end loop;

      -- Compute the parity symbols
      Encode_Rs_Int(Reed_S, Dat1, B);

      -- Move aprity symbols and data into sent array, in reverse order.
      for I in 0 .. 50 loop
         Sent(50 - I) := B(I);
      end loop;

      for I in 0 .. 11 loop
         Sent(51 + I) := Dat1(11 - I);
      end loop;

   end Rs_Encode;

   procedure Rs_Decode( Recd0 : in out Integer_Array; Era : in out Integer_Array;
                       Num : in Integer; Decoded : in out Integer_Array; Nerr : in out Integer  ) is
   -- Decode JT65 received data recd0[63], producing decoded[12].
   -- Erasures are indicated in era0[Numera].  The number of corrected
   -- errors is Nerr.  If the data are uncorrectable, Nerr=-1 is returned.
      Numera : Integer;
      Era_Pos : Eras_Pos_Access(0 .. 49);
      Recd : Integer_Array(0 .. 62);
      Symsize : Integer := 6;
      Gfpoly : Integer := 16#43#;
      Fcr : Integer := 3;
      Prim : Integer  := 1;
      Nroots : Integer := 51;
      Pad : Integer := 0;
   begin

      if( First ) then
         Reed_S := Init_Rs_Int(Symsize, Gfpoly, Fcr, Prim, Nroots, Pad);
         First := False;
      end if;
      Numera := Num;
      for I in 0 .. 11 loop
         Recd(I) := Recd0(62 - I);
      end loop;
      for I in 0 .. 50 loop
         Recd(12 + I) := Recd0(50 - I);
      end loop;

      --TO DO: proper handling of Era_Pos
      --if( Numera /= 0) then
         --for I in 1 .. Numera loop
            --Era_Pos(I) := Era(I);
         --end loop;
      --end if;
      Nerr := Decode_Rs_Int( Reed_S, Recd, Era_Pos, Numera );
      for I in 0 .. 11 loop
         Decoded(I) := Recd(11 - I);
      end loop;
   end Rs_Decode;

end Wrapkarn;

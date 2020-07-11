--------------------------------------------------------------------------------
-- FILE   : wrapkarn.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Wrapkarn is

   procedure Rs_Encode
     (Dgen : in Unsigned_8_array;
      Sent : in out Unsigned_8_array)
   is

      Dat1 : Unsigned_8_array(0 .. 11);
      B : Unsigned_8_array(0 .. 50);
      Symsize : constant Unsigned_8 := 6;
      Gfpoly : constant Unsigned_8 := 16#43#;
      Fcr : constant Unsigned_8 := 3;
      Prim : constant Unsigned_8  := 1;
      Nroots : constant Unsigned_8 := 51;
      Pad : constant Unsigned_8 := 0;

   begin
      if First then
         -- Initialize the JT65 codec
         Init_Rs_Int( Reed_S, Symsize, Gfpoly, Fcr, Prim, Nroots, Pad);
         First := False;
      end if;

      -- Reverse data order for the Karn codec
      for I in 0 .. 11 loop
         Dat1(I) := Dgen(11-I);
      end loop;

      if Reed_S.Nroots <= 51 then
         -- Compute the parity symbols
         Encode_Rs_Int(Reed_S, Dat1, B);
         -- Move aprity symbols and data into sent array, in reverse order.
         for I in 0 .. 50 loop
            Sent(50 - I) := B(I);
         end loop;

         for I in 0 .. 11 loop
            Sent(51 + I) := Dat1(11 - I);
         end loop;
      end if;
   end Rs_Encode;

   procedure Rs_Decode
     (Recd0 : in Unsigned_8_array;
      Era : in Unsigned_8_array;
      Num : in Unsigned_8;
      Decoded : out Unsigned_8_array;
      Dat_Packed : in Unsigned_8_Array)
   is
   -- Decode JT65 received data recd0[63], producing decoded[12].
   -- Erasures are indicated in era0[Numera].  The number of corrected
   -- errors is Nerr.  If the data are uncorrectable, Nerr=-1 is returned.
      Numera : Unsigned_8;
      Era_Pos : Unsigned_8_array(0 .. 49) := (others => 0);
      Recd, Recd_Out: Unsigned_8_array(0 .. 62);
      Symsize : constant Unsigned_8 := 6;
      Gfpoly : constant Unsigned_8 := 16#43#;
      Fcr : constant Unsigned_8 := 3;
      Prim : constant Unsigned_8  := 1;
      Nroots : constant Unsigned_8 := 51;
      Pad : constant Unsigned_8 := 0;
      Recd_Reverse : Unsigned_8_Array(0 .. 11) := (others => 0);
      Count : Integer := 0;

   begin

      Decoded := (others => 0);
      --Era_Pos := (others => 0);
      --Recd_Reverse :=
      Numera := Num;
      for I in Recd'Range loop
         Recd(I) := Recd0(62 - I);
      end loop;
      for I in 0 .. 50 loop
         Recd(12 + I) := Recd0(50 - I);
      end loop;
      for I in reverse Recd_Reverse'Range loop
         Recd_Reverse(Count) := Recd(I);
         Count := Count + 1;
      end loop;

      -- If the packed bits do not match, call the decoder
      if Dat_Packed(0 .. 11) /= Recd_Reverse(0 .. 11) then
         if First then
            Init_Rs_Int(Reed_S, Symsize, Gfpoly, Fcr, Prim, Nroots, Pad);
            First := False;
         end if;
         if Numera /= 0 then
            for I in 1 .. Numera loop
               Era_Pos(Integer(I)) := Era(Integer(I));
            end loop;
            --for I in Era_Pos'Range loop
            --   Era_Pos(I) := 0;
            --end loop;
         end if;
         -- No longer returning the number of errors to meet
         -- SPARK flow analysis requirements
         if Reed_S.Nn <= 63 then
            Recd_Out := Decode_Rs_Int( Reed_S, Recd, Era_Pos, Numera );
            for I in 0 .. 11 loop
               Decoded(I) := Recd_Out(11 - I);
            end loop;
         end if;
         -- Introduce an out error code here?

      -- The packed bits matched.
      -- Return the packed bits for packed-bits->message decoding
      else
         for I in 0 .. 11 loop
            Decoded(I) := Recd_Reverse(I);
         end loop;
      end if;

   end Rs_Decode;

   procedure Init_Rs_Int
     (Reed_S : in out Rs;
      Symsize : in Unsigned_8;
      Gfpoly : in Unsigned_8;
      Fcr : in Unsigned_8;
      Prim : in Unsigned_8;
      Nroots : in Unsigned_8;
      Pad : in Unsigned_8)
   is
      X, Sr, Root, Iprim, Placeholder : Unsigned_8;
      One : constant Unsigned_8 := 1;
   begin

      Reed_S.Mm := Symsize;
      Placeholder := Shift_Left(1,Integer(Symsize))-1;
      Reed_S.Nn := Placeholder;
      Reed_S.Pad := Pad;

      Reed_S.Index_Of(0) := Reed_S.Nn;
      Reed_S.Alpha_To(Integer(Reed_S.Nn)) := 0;

      Sr := 1;
      for I in 0 .. Reed_S.Nn-1 loop
         --if Sr /= 0  and I <= Unsigned_8'Last and Sr <= Unsigned_8'Last then
         if Sr <= Placeholder and I <= Placeholder then
            Reed_S.Index_Of(Integer(Sr)) := I;
            Reed_S.Alpha_To(Integer(I)) := Sr;
            Sr := Shift_Left(Sr,1);
            if (Sr and Shift_Left(One,Integer(Symsize))) /= 0 then
               Sr := Sr xor Gfpoly;
            end if;
            Sr := Sr and Reed_S.Nn;
         end if;
      end loop;

      if Sr = 1 then
         Reed_S.Fcr := Fcr;
         Reed_S.Prim := Prim;
         Reed_S.Nroots := Nroots;

         Iprim := 1;
         loop
            if (Iprim mod Prim) = 0 then
               exit;
            else
               Iprim := Iprim + Reed_S.Nn;
            end if;
         end loop;

         Reed_S.Iprim := Iprim / Prim;
         Reed_S.Genpoly(0) := 1;
         Root := Fcr * Prim;

         for I in 0 .. 50 loop
            Reed_S.Genpoly(I+1) := 1;
            for J in reverse 0 .. I loop
               if J > 0 then
                  if Reed_S.Genpoly(J) > 0 and Reed_S.Genpoly(J) <= Placeholder then
                     X := Reed_S.Index_Of(Integer(Reed_S.Genpoly(J))) + Root;
                     Reed_S.Genpoly(J) := Reed_S.Genpoly(J-1) xor
                                              Reed_S.Alpha_To(Modnn(Reed_S,X));
                  else
                     Reed_S.Genpoly(J) := Reed_S.Genpoly(J-1);
                  end if;
               end if;
            end loop;
            if Reed_S.Genpoly(0) <= 63 then
               X := Reed_S.Index_Of(Integer(Reed_S.Genpoly(0))) + Root;
               if X < 255 then
                  Reed_S.Genpoly(0) := Reed_S.Alpha_To(Modnn(Reed_S,X));
               end if;
            end if;
            Root := Root + Prim;
         end loop;
         for I in 0 .. Nroots loop
            if Nroots = 51 and I <= 51 then
               if Reed_S.Genpoly(Integer(I)) <= 63 then
               Reed_S.Genpoly(Integer(I)) := Reed_S.Index_Of(Integer(Reed_S.Genpoly(Integer(I))));
               end if;
            end if;
         end loop;
      end if;
   end Init_Rs_Int;

   function Modnn
     (Reed_S : in Rs;
      A : in Unsigned_8) return Integer
   is
      X : Unsigned_8 := A;
   begin
      while X >= Reed_S.Nn loop
         X := X - Reed_S.Nn;
         X := Shift_Right(X, Integer(Reed_S.Mm))
                      + (X and Reed_S.Nn);
      end loop;
      return Integer(X);
   end Modnn;

   procedure Encode_Rs_Int
     (Reed_S : in Rs;
      Data : in Unsigned_8_array;
      Bb : out Unsigned_8_array )
   is
      Feedback, X : Unsigned_8;
      Nroots : constant Unsigned_8 := 51;
      --Rs_Null : constant Rs := null;
   begin
      for I in Bb'Range loop
         Bb(I) := 0;
      end loop;

      if Reed_S.Nn <= 63 then
      for I in 0 .. 11 loop
         if Integer(Data(I) xor Bb(0)) <= 63 then
            Feedback := Reed_S.Index_Of(Integer(Data(I) xor
                                          (Bb(0))));
            if Feedback /= Reed_S.Nn then
               for J in 1 .. 50 loop
                  X := Feedback + Reed_S.Genpoly(Integer(Nroots-Unsigned_8(J)));
                  Bb(J) := Bb(J) xor Reed_S.Alpha_To(Modnn(Reed_S, X));
               end loop;
            end if;
            Bb(0 .. Integer(Nroots) - 2) := Bb(1 .. Integer(Nroots) - 1);
            if Feedback /= Reed_S.Nn then
               X := Feedback + Reed_S.Genpoly(0);
               Bb(50) := Reed_S.Alpha_To(Modnn(Reed_S, X));
            else
               Bb(50) := 0;
            end if;
         end if;
         end loop;
      end if;
   end Encode_Rs_Int;

   function Decode_Rs_Int
     (Reed_S : in Rs;
      Data_In : in  Unsigned_8_array;
      Eras_Pos_In : in Unsigned_8_array;
      No_Eras : in Unsigned_8 ) return Unsigned_8_array
   is

      --procedure Finish( Eras_Pos1 : in out Unsigned_8_array;
      --                  Counter : in Integer;
      --                  Loc1 : in Unsigned_8_array)
      --is
         --Eras_Pos_Null : constant Eras_Pos_Access := null;
      --begin
         --if Eras_Pos1 /= Eras_Pos_Null then
         --   for I in 0 .. Counter - 1 loop
         --      Eras_Pos1(I) := Loc1(I);
         --   end loop;
         --end if;
      --end Finish;

      Nroots : constant Integer := 51;
      Deg_Lambda, Deg_Omega : Integer;
      --I, J, R,
      Q, Num1, Num2 : Integer;
      X, Y, Z, U, K, R, Tmp, Den, El, Modnn_Tmp, Syn_Error, Discr_R, Modnn_Tmp2, Modnn_Tmp3 : Unsigned_8;

      -- Err + Eras Locator Poly and syndrome poly
      Lambda : Unsigned_8_array(0 .. 51) := (others => 0);
      S : Unsigned_8_array(0 .. 50);

      B, T, Omega, Reg : Unsigned_8_array(0 .. 51) := (others => 0);
      Root, Loc : Unsigned_8_array(0 .. 51) := (others => 0);
      Count : Integer;

     --Rs_Null : constant Rs := null;
      Data : Unsigned_8_array(0 .. 62);
      Eras_Pos : Unsigned_8_array(0 .. 49);
   begin
      -- Initialization
      for I in Eras_Pos'Range loop
         Eras_Pos(I) := Eras_Pos_In(I);
      end loop;
      for I in Data'Range loop
         Data(I) := Data_In(I);
      end loop;

      for I in 0 .. Nroots - 1 loop
         S(I) := Data(0);
      end loop;
      for J in 1 .. Reed_S.Nn - Reed_S.Pad-1 loop
         for I in 0 .. Nroots-1 loop
            if S(I) = 0 and Integer(S(I)) <= 63 and Integer(J) <= 62 then
                  S(I) := Data(Integer(J));
            else
               if Integer(S(I)) <= 63 and Integer(J) <= 62 then
                  X := Reed_S.Index_Of(Integer(S(I))) + (Reed_S.Fcr + Unsigned_8(I))* (Reed_S.Prim);
                  if Modnn(Reed_S,X) <= 63 then
                     S(I) := Data(Integer(J)) xor
                       Reed_S.Alpha_To(Modnn(Reed_S, X));
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      -- Convert syndromes to index form, checking for nonzero condition
      Syn_Error := 0;
      for I in 0 .. Nroots - 1 loop
         Syn_Error := Syn_Error or S(I);
         if S(I) <= 63 then
            S(I) := Reed_S.Index_Of(Integer(S(I)));
         end if;
      end loop;

      if Syn_Error = 0 then
         -- If syndrome is zero, data() is a codeword and there are no errors
         -- to corret. Return Data() unmodified.
            return Data; -- Always returning here, have never found an error.
                         -- This needs looking into, to ensure the deep-search decoder
                         -- is working properly. Need to figure out proper testing method.
      else
         Lambda(0 .. 50) := (others=>0);
         Lambda(0) := 1;
         if No_Eras > 0 then
            Y := Reed_S.Prim * (Reed_S.Nn - 1 - Eras_Pos(0));
            Lambda(1) := Reed_S.Alpha_To(Modnn(Reed_S, Y));
            for I in 1 .. No_Eras - 1 loop
               Z := Reed_S.Prim * (Reed_S.Nn - 1 - Eras_Pos(Integer(I)));
               U := Unsigned_8(Modnn(Reed_S,Z));
               for J in reverse 1 .. I + 1 loop
                  Tmp := Reed_S.Index_Of(Integer(Lambda(Integer(J) - 1)));
                  if Tmp /= Reed_S.Nn then
                    Modnn_Tmp := U + Tmp;
                     Lambda(Integer(J)) :=
                       Lambda(Integer(J)) xor Reed_S.Alpha_To(Modnn(Reed_S, Modnn_Tmp));
                  end if;
               end loop;
            end loop;
         end if;

         -- Skipped over DEBUG 1 and DEBUG 2

         for I in 0 .. 50 loop
            B(I) := Reed_S.Index_Of(Integer(Lambda(I)));
            --Put_Line("B("&I'Image&") = "&Integer'Image(B(I)));
            --Put_Line("Reed_S.Index_Of(Lambda("&I'Image&") = "&Integer'Image(Reed_S.Index_Of(Lambda(I))));
         end loop;

         R := No_Eras;
         El := No_Eras;

         while Integer(R) + 1 <= Nroots loop
            Discr_R := 0;
            for I in 0 .. R - 1 loop
               if Integer(I) <= 51 and R > 0 then
                  if Lambda(Integer(I)) /= 0 and S(Integer(R - I) - 1) /= Reed_S.Nn then
                     Modnn_Tmp := Reed_S.Index_Of(Integer(Lambda(Integer(I)))) + S(Integer(R-I)-1);
                     Discr_R := Discr_R xor Reed_S.Alpha_To(Modnn(Reed_S,Modnn_Tmp));
                  end if;
               end if;
            end loop;
            if Integer(Discr_R) <= 63 then
               Discr_R := Reed_S.Index_Of(Integer(Discr_R));
            end if;

            if Discr_R = Reed_S.Nn then
               B(1) := B(0);
               B(0) := Reed_S.Nn;
            else
               T(0) := Lambda(0);
               for I in 0 .. Nroots - 1 loop
                  if B(I) /= Reed_S.Nn then
                     Modnn_Tmp2 := Discr_R + B(I);
                     T(I + 1) := Lambda(I + 1) xor
                                           Reed_S.Alpha_To(Modnn(Reed_S,Modnn_Tmp2));
                  else
                     T(I + 1) := Lambda(I + 1);
                  end if;
               end loop;
               if 2 * El <= R + No_Eras - 1 then
                  El := R + No_Eras - El;
                  for I in 0 .. Nroots loop
                     if Integer(Lambda(I) - Discr_R + Reed_S.Nn) <= 63 then
                        Modnn_Tmp3 := Reed_S.Index_Of(Integer(Lambda(I) - Discr_R + Reed_S.Nn));
                        B(I) := (if Lambda(I) = 0 then Reed_S.Nn else Unsigned_8(Modnn(Reed_S,Modnn_Tmp3)));
                     end if;
                  end loop;
               else
                  -- TODO: Maybe error here?
                  -- 2 lines below: B(x) <-- inv(discr_r_) * lambda(x)
                  B(1) := B(0);
                  B(0) := Reed_S.Nn;
               end if;
               Lambda := T;
            end if;
         end loop;

         -- Convert lambda to index forma and compute deg(lambda(x))
         Deg_Lambda := 0;
         for I in 0 .. Nroots loop
            Lambda(I) := Reed_S.Index_Of(Integer(Lambda(I)));
            if Lambda(I) /= Reed_S.Nn then
               Deg_Lambda := I;
            end if;
         end loop;

         -- Find roots of the eror+ersure locator polynomial by Chien search
         Reg(1) := Lambda(1);
         Count := 0;

         K := Reed_S.Iprim-1;
         for I in 1 .. Reed_S.Nn loop
            Q := 1; -- Lambda(0) is always 0
            for J in reverse 1 .. Deg_Lambda loop
               if Reg(J) /= Reed_S.Nn then
                  Y := Reg(J) + Unsigned_8(J);
                  Reg(J) := Unsigned_8(Modnn(Reed_S,Y));
                  Q := Integer(Unsigned_8(Q) xor Reed_S.Alpha_To(Integer(Reg(J))));
               end if;
            end loop;

            if Q /= 0 then
               null;
            else
               Root(Count) := I;
               Loc(Count) := K;
               -- If we've already found the max possible roots.
               -- Abort the search to save time
               Count := Count + 1;
               if Count = Deg_Lambda then
                  exit;
               end if;
            end if;
            X := K + Reed_S.Iprim;
            K := Unsigned_8(Modnn(Reed_S,X));
         end loop;

         if Deg_Lambda /= Count then
            -- Deg(Lambda unequal to number of roots => uncorrectable
            -- error detected
            -- Count := -1;
            -- Finish(Eras_Pos, Count, Loc);
            return Data;
         end if;
         -- Compute err+eras evaluator poly omega(x) = s(x)*lambda(x) (modulo
         -- x **Nroots). In index form. Also find deg(omega).

         Deg_Omega := Deg_Lambda - 1;
         for I in 0 .. Deg_Omega loop
            Tmp := 0;
            for J in reverse 0 .. I loop
               if S(I - J) /= Reed_S.Nn and Lambda(J) /= Reed_S.Nn then
                  X := S(I - J) + Lambda(J);
                  Tmp := Tmp xor Reed_S.Alpha_To(Modnn(Reed_S,X));
               end if;
            end loop;
            Omega(I) := Reed_S.Index_Of(Integer(Tmp));
         end loop;

         -- Compute error values in poly-form. Num1 = Omega(Inv(X(L))), Num2 =
         -- Inv(X(L))**(Reed_S.Fcr-1) and Den = Lambda_Pr(Inv(X(L))) all in poly-form
         for J in 0 .. Count - 1 loop
            Num1 := 0;
            for I in 0 .. Deg_Omega loop
               if Omega(I) /= Reed_S.Nn then
                  X := Omega(I) + Unsigned_8(I) * Root(J);
                  Num1 := Integer(Unsigned_8(Num1) xor Reed_S.Alpha_To(Modnn(Reed_S,X)));
               end if;
            end loop;
            Y := Root(J) * (Reed_S.Fcr - 1) + Reed_S.Nn;
            Num2 := Integer(Reed_S.Alpha_To(Modnn(Reed_S,Y)));
            Den := 0;

            -- Lamda(I + 1) for I even is the formal dericative lambda_pr of lambda(I)
            -- Bitwise not operation in ada?
            -- Tmp := Integer(Unsigned_8((if Deg_Lambda < Nroots - 1 then Deg_Lambda else Nroots - 1)) and Unsigned_8((not 1)));
            Tmp := 1;
            for I in reverse 0 .. Tmp loop
               if Lambda(Integer(I) + 1) /= Reed_S.Nn then
                  Z := Lambda(Integer(I) + 1) + I * Root(J);
                  Den := Den xor Reed_S.Alpha_To(Modnn(Reed_S,Z));
               end if;

               if Num1 /= 0 and Loc(J) >= Reed_S.Pad then
                  R := Reed_S.Index_Of(Num1) + Reed_S.Index_Of(Num2) + Reed_S.Nn - Reed_S.Index_Of(Integer(Den));
                  Data(Integer(Loc(J) - Reed_S.Pad)) := Data(Integer(Loc(J) - Reed_S.Pad)) xor Reed_S.Alpha_To(Modnn(Reed_S,R));
               end if;
               --Tmp := Tmp - 2;
            end loop;
         end loop;
            return Data;
      end if;
   end Decode_Rs_Int;

end Wrapkarn;


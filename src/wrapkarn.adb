--------------------------------------------------------------------------------
-- FILE   : wrapkarn.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Wrapkarn is

   procedure Encode_Rs_Int( Rs : in Rs_Access;
                            Data : in Unsigned_32_Array;
                            Bb : out Unsigned_32_Array ) is

      Feedback, X : Unsigned_32;
      Nroots : constant Unsigned_32 := 51;
      Rs_Null : constant Rs_Access := null;
   begin
      for I in Bb'Range loop
         Bb(I) := 0;
      end loop;

      if Rs /= Rs_Null then
         for I in 0 .. 11 loop
            Feedback := Rs.Index_Of(Integer(Data(I) xor
                               (Bb(0))));
            if Feedback /= Rs.Nn then
               for J in 1 .. 50 loop
                  X := Feedback + Rs.Genpoly(Integer(Nroots-Unsigned_32(J)));
                  Bb(J) := Bb(J) xor Rs.Alpha_To(Modnn(Rs, X));
               end loop;
            end if;
            Bb(0 .. Integer(Nroots) - 2) := Bb(1 .. Integer(Nroots) - 1);
            if Feedback /= Rs.Nn then
               X := Feedback + Rs.Genpoly(0);
               Bb(Integer(Rs.Nroots)-1) := Rs.Alpha_To(Modnn(Rs, X));
            else
               Bb(51-1) := 0;
            end if;
         end loop;
      end if;
   end Encode_Rs_Int;

   function Decode_Rs_Int( Rs : in Rs_Access;
                           Data_In : in  Unsigned_32_Array;
                           Eras_Pos_In : in Unsigned_32_Array;
                           No_Eras : in Unsigned_32 ) return Unsigned_32_Array
   is

      --procedure Finish( Eras_Pos1 : in out Unsigned_32_Array;
      --                  Counter : in Integer;
      --                  Loc1 : in Unsigned_32_Array)
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
      X, Y, Z, U, K, R, Tmp, Den, El, Modnn_Tmp, Syn_Error, Discr_R, Modnn_Tmp2, Modnn_Tmp3 : Unsigned_32;

      -- Err + Eras Locator Poly and syndrome poly
      Lambda : Unsigned_32_Array(0 .. 51) := (others => 0);
      S : Unsigned_32_Array(0 .. 50);

      B, T, Omega, Reg : Unsigned_32_Array(0 .. 51) := (others => 0);
      Root, Loc : Unsigned_32_Array(0 .. 51) := (others => 0);
      Count : Integer;

      Rs_Null : constant Rs_Access := null;
      Data : Unsigned_32_Array(0 .. 62);
      Eras_Pos : Unsigned_32_Array(0 .. 49);
   begin
      -- Initialization
      for I in Eras_Pos'Range loop
         Eras_Pos(I) := Eras_Pos_In(I);
      end loop;
      for I in Data'Range loop
         Data(I) := Data_In(I);
      end loop;

      if Rs /= Rs_Null then
         for I in 0 .. Nroots - 1 loop
            S(I) := Data(0);
         end loop;
         for J in 1 .. Rs.Nn - Rs.Pad-1 loop
            for I in 0 .. Nroots-1 loop
               if S(I) = 0 then
                  S(I) := Data(Integer(J));
               else
                  X := Rs.Index_Of(Integer(S(I))) + (Rs.Fcr + Unsigned_32(I))* (Rs.Prim);
                  S(I) := Data(Integer(J)) xor
                                    Rs.Alpha_To(Modnn(Rs, X));
               end if;
            end loop;
         end loop;

         -- Convert syndromes to index form, checking for nonzero condition
         Syn_Error := 0;
         for I in 0 .. Nroots - 1 loop
            Syn_Error := Syn_Error or S(I);
            S(I) := Rs.Index_Of(Integer(S(I)));
         end loop;

         if Syn_Error = 0 then
            -- If syndrome is zero, data() is a codeword and there are no errors
            -- to corret. Return Data() unmodified.
            --Count := 0;
            --Finish(Eras_Pos, Count, Loc);
            return Data; -- Always returning here, have never found an error.
                         -- This needs looking into, to ensure the deep-search decoder
                         -- is working properly.
         end if;
         Lambda(0 .. 50) := (others=>0);
         Lambda(0) := 1;
         if No_Eras > 0 then
            Y := Rs.Prim * (Rs.Nn - 1 - Eras_Pos(0));
            Lambda(1) := Rs.Alpha_To(Modnn(Rs, Y));
            for I in 1 .. No_Eras - 1 loop
               Z := Rs.Prim * (Rs.Nn - 1 - Eras_Pos(Integer(I)));
               U := Unsigned_32(Modnn(Rs,Z));
               for J in reverse 1 .. I + 1 loop
                  Tmp := Rs.Index_Of(Integer(Lambda(Integer(J) - 1)));
                  if Tmp /= Rs.Nn then
                    Modnn_Tmp := U + Tmp;
                     Lambda(Integer(J)) :=
                       Lambda(Integer(J)) xor Rs.Alpha_To(Modnn(Rs, Modnn_Tmp));
                  end if;
               end loop;
            end loop;
         end if;

         -- Skipped over DEBUG 1 and DEBUG 2

         for I in 0 .. 50 loop
            B(I) := Rs.Index_Of(Integer(Lambda(I)));
            --Put_Line("B("&I'Image&") = "&Integer'Image(B(I)));
            --Put_Line("Rs.Index_Of(Lambda("&I'Image&") = "&Integer'Image(Rs.Index_Of(Lambda(I))));
         end loop;

         R := No_Eras;
         El := No_Eras;

         while Integer(R) + 1 <= Nroots loop
            Discr_R := 0;
            for I in 0 .. R - 1 loop
               if Lambda(Integer(I)) /= 0 and S(Integer(R - I) - 1) /= Rs.Nn then
                  Modnn_Tmp := Rs.Index_Of(Integer(Lambda(Integer(I)))) + S(Integer(R-I)-1);
                  Discr_R := Discr_R xor Rs.Alpha_To(Modnn(Rs,Modnn_Tmp));
               end if;
            end loop;
            Discr_R := Rs.Index_Of(Integer(Discr_R));

            if Discr_R = Rs.Nn then
               B(1) := B(0);
               B(0) := Rs.Nn;
            else
               T(0) := Lambda(0);
               for I in 0 .. Nroots - 1 loop
                  if B(I) /= Rs.Nn then
                     Modnn_Tmp2 := Discr_R + B(I);
                     T(I + 1) := Lambda(I + 1) xor
                                           Rs.Alpha_To(Modnn(Rs,Modnn_Tmp2));
                  else
                     T(I + 1) := Lambda(I + 1);
                  end if;
               end loop;
               if 2 * El <= R + No_Eras - 1 then
                  El := R + No_Eras - El;
                  for I in 0 .. Nroots loop
                     Modnn_Tmp3 := Rs.Index_Of(Integer(Lambda(I) - Discr_R + Rs.Nn));
                     B(I) := (if Lambda(I) = 0 then Rs.Nn else Unsigned_32(Modnn(Rs,Modnn_Tmp3)));
                  end loop;
               else
                  -- TODO: Maybe error here?
                  -- 2 lines below: B(x) <-- inv(discr_r_) * lambda(x)
                  B(1) := B(0);
                  B(0) := Rs.Nn;
               end if;
               Lambda := T;
            end if;
         end loop;

         -- Convert lambda to index forma and compute deg(lambda(x))
         Deg_Lambda := 0;
         for I in 0 .. Nroots loop
            Lambda(I) := Rs.Index_Of(Integer(Lambda(I)));
            if Lambda(I) /= Rs.Nn then
               Deg_Lambda := I;
            end if;
         end loop;

         -- Find roots of the eror+ersure locator polynomial by Chien search
         Reg(1) := Lambda(1);
         Count := 0;

         K := Rs.Iprim-1;
         for I in 1 .. Rs.Nn loop
            Q := 1; -- Lambda(0) is always 0
            for J in reverse 1 .. Deg_Lambda loop
               if Reg(J) /= Rs.Nn then
                  Y := Reg(J) + Unsigned_32(J);
                  Reg(J) := Unsigned_32(Modnn(Rs,Y));
                  Q := Integer(Unsigned_32(Q) xor Rs.Alpha_To(Integer(Reg(J))));
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
            X := K + Rs.Iprim;
            K := Unsigned_32(Modnn(Rs,X));
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
               if S(I - J) /= Rs.Nn and Lambda(J) /= Rs.Nn then
                  X := S(I - J) + Lambda(J);
                  Tmp := Tmp xor Rs.Alpha_To(Modnn(Rs,X));
               end if;
            end loop;
            Omega(I) := Rs.Index_Of(Integer(Tmp));
         end loop;

         -- Compute error values in poly-form. Num1 = Omega(Inv(X(L))), Num2 =
         -- Inv(X(L))**(Rs.Fcr-1) and Den = Lambda_Pr(Inv(X(L))) all in poly-form
         for J in 0 .. Count - 1 loop
            Num1 := 0;
            for I in 0 .. Deg_Omega loop
               if Omega(I) /= Rs.Nn then
                  X := Omega(I) + Unsigned_32(I) * Root(J);
                  Num1 := Integer(Unsigned_32(Num1) xor Rs.Alpha_To(Modnn(Rs,X)));
               end if;
            end loop;
            Y := Root(J) * (Rs.Fcr - 1) + Rs.Nn;
            Num2 := Integer(Rs.Alpha_To(Modnn(Rs,Y)));
            Den := 0;

            -- Lamda(I + 1) for I even is the formal dericative lambda_pr of lambda(I)
            -- Bitwise not operation in ada?
            -- Tmp := Integer(Unsigned_32((if Deg_Lambda < Nroots - 1 then Deg_Lambda else Nroots - 1)) and Unsigned_32((not 1)));
            Tmp := 1;
            for I in reverse 0 .. Tmp loop
               if Lambda(Integer(I) + 1) /= Rs.Nn then
                  Z := Lambda(Integer(I) + 1) + I * Root(J);
                  Den := Den xor Rs.Alpha_To(Modnn(Rs,Z));
               end if;

               if Num1 /= 0 and Loc(J) >= Rs.Pad then
                  R := Rs.Index_Of(Num1) + Rs.Index_Of(Num2) + Rs.Nn - Rs.Index_Of(Integer(Den));
                  Data(Integer(Loc(J) - Rs.Pad)) := Data(Integer(Loc(J) - Rs.Pad)) xor Rs.Alpha_To(Modnn(Rs,R));
               end if;
               --Tmp := Tmp - 2;
            end loop;
         end loop;
         --Finish(Eras_Pos, Count, Loc);
         return Data;
      end if;
      return Data;
   end Decode_Rs_Int;

   procedure Rs_Encode( Dgen : in Unsigned_32_Array;
                        Sent : in out Unsigned_32_Array )
   is

      Dat1 : Unsigned_32_Array(0 .. 11);
      B : Unsigned_32_Array(0 .. 50);
      Symsize : constant Unsigned_32 := 6;
      Gfpoly : constant Unsigned_32 := 16#43#;
      Fcr : constant Unsigned_32 := 3;
      Prim : constant Unsigned_32  := 1;
      Nroots : constant Unsigned_32 := 51;
      Pad : constant Unsigned_32 := 0;

   begin
      if First then
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

   procedure Rs_Decode( Recd0 : in Unsigned_32_Array;
                        Era : in Unsigned_32_Array;
                        Num : in Unsigned_32;
                        Decoded : out Unsigned_32_Array )
   is
   -- Decode JT65 received data recd0[63], producing decoded[12].
   -- Erasures are indicated in era0[Numera].  The number of corrected
   -- errors is Nerr.  If the data are uncorrectable, Nerr=-1 is returned.
      Numera : Unsigned_32;
      Era_Pos : Unsigned_32_Array(0 .. 49);
      Recd, Recd_Out: Unsigned_32_Array(0 .. 62);
      Symsize : constant Unsigned_32 := 6;
      Gfpoly : constant Unsigned_32 := 16#43#;
      Fcr : constant Unsigned_32 := 3;
      Prim : constant Unsigned_32  := 1;
      Nroots : constant Unsigned_32 := 51;
      Pad : constant Unsigned_32 := 0;

   begin

      if First then
         Reed_S := Init_Rs_Int(Symsize, Gfpoly, Fcr, Prim, Nroots, Pad);
         First := False;
      end if;
      Numera := Num;
      for I in Recd'Range loop
         Recd(I) := Recd0(62 - I);
      end loop;
      for I in 0 .. 50 loop
         Recd(12 + I) := Recd0(50 - I);
      end loop;

      if Numera /= 0 then
         for I in 1 .. Numera loop
            Era_Pos(Integer(I)) := Era(Integer(I));
         end loop;
      else
      for I in Era_Pos'Range loop
            Era_Pos(I) := 0;
      end loop;
      end if;

      -- No longer returning the number of errors to meet
      -- SPARK flow analysis requirements
      Recd_Out := Decode_Rs_Int( Reed_S, Recd, Era_Pos, Numera );
      for I in Decoded'Range loop
         Decoded(I) := 0;
      end loop;

      for I in 0 .. 11 loop
         Decoded(I) := Recd_Out(11 - I);
      end loop;
   end Rs_Decode;

end Wrapkarn;


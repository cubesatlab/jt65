--------------------------------------------------------------------------------
-- FILE   : wrapkarn.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package body Wrapkarn is

   procedure Rs_Encode ( Dgen : in out Integer_Array; Sent : in out Integer_Array ) is
      Dat1 : Integer_Array(1..12);
      B : Integer_Array(1 .. 51);
      I : Integer;
   begin

      if(First) then
         -- Initialize the JT65 codec
         --Rs := Init_Rs_Int(6, 16#43#, 3, 1, 51, 0);
         First := False;
      end if;

      -- Reverse data order for the Karn codec
      for I in 1 .. 12 loop
         Dat1(I) := Dgen(12-I);
      end loop;

      -- Compute the parity symbols
      --Encode_Rs_Int(Rs, Dat1, B);

      -- Move aprity symbols and data into sent array, in reverse order.
      for I in 1 .. 51 loop
         Sent(50-I) := B(I);
      end loop;

      for I in 1 .. 12 loop
         Sent(51 + I) := Dat1(12 - I);
      end loop;

   end Rs_Encode;

   procedure Rs_Decode ( Recd0 : in out Integer_Array; Era : in out Integer_Array;
                       Num : in Integer; Decoded : in out Integer_Array; Nerr : in out Integer  ) is
   -- Decode JT65 received data recd0[63], producing decoded[12].
   -- Erasures are indicated in era0[Numera].  The number of corrected
   -- errors is Nerr.  If the data are uncorrectable, Nerr=-1 is returned.
      Numera : Integer;
      I : Integer;
      Era_Pos : Integer_Array(1 .. 50);
      Recd : Integer_Array(1 .. 63);
   begin

      if( First ) then
         --Rs := Init_Rs_Int(6, 16#43#, 3, 1, 51, 0);
         First := False;
      end if;

      Numera := Num;
      for I in 1 .. 12 loop
         Recd(I) := Recd0(62 - I);
      end loop;

      for I in 1 .. 51 loop
         Recd(12 + I) := Recd0(50 - I);
      end loop;

      if( Numera > 0) then
         for I in 1 .. Numera loop
            Era_Pos(I) := Era(I);
         end loop;
      end if;
      --Nerr := Decode_Rs_Int( Rs, Recd, Era_Pos, Numera );

      for I in 1 .. 12 loop
         Decoded(I) := Recd(12 - I);
      end loop;

   end Rs_Decode;

end Wrapkarn;

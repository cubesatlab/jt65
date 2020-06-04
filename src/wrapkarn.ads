--------------------------------------------------------------------------------
-- FILE   : wrapkarn.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Pack_JT;
with Interfaces; use Interfaces;
with General_JT; use General_JT;
with Init_Rs; use Init_Rs;

package Wrapkarn is

   Reed_S : Rs_access;
   First : Boolean := True;

   procedure Encode_Rs_Int( Rs : in Rs_Access;
                            Data : in Integer_Array;
                            Bb : out Integer_Array);

   function Decode_Rs_Int( Rs : in Rs_Access;
                           Data_In : in Integer_Array;
                           Eras_Pos_In : in Integer_Array;
                           No_Eras : in  Integer ) return Integer_Array;

   procedure Rs_Encode( Dgen : in Integer_Array;
                        Sent : in out Integer_Array )
     with Global => (In_Out => (Reed_S, First));

   procedure Rs_Decode( Recd0 : in Integer_Array;
                        Era : in Integer_Array;
                        Num : in Integer;
                        Decoded : out Integer_Array )
     with Global => (In_Out => (Reed_S, First));

end Wrapkarn;


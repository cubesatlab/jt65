--------------------------------------------------------------------------------
-- FILE   : wrapkarn.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Pack_JT; use Pack_JT;
with Interfaces; use Interfaces;
with General_JT; use General_JT;
with Init_Rs; use Init_Rs;

package Wrapkarn is

   Reed_S : Rs_access;

   type Eras_Pos_Access is access Integer_Array;

   First : Boolean := True;

   type Integer_Array_Aliased is array(Natural range <>) of aliased Integer;

   procedure Rs_Encode( Dgen : in out Integer_Array; Sent : in out Integer_Array );

   procedure Rs_Decode( Recd0 : in out Integer_Array; Era : in out Integer_Array;
                        Num : in Integer; Decoded : in out Integer_Array; Nerr : in out Integer );

   procedure Encode_Rs_Int( Rs : in out Rs_Access; Data : in out Integer_Array;
                            Bb : in out Integer_Array_Aliased );

   function Decode_Rs_Int( Rs : in out Rs_Access; Data : in out Integer_Array;
                            Eras_Pos : in out Eras_Pos_Access; No_Eras : in out Integer ) return Integer;

end Wrapkarn;

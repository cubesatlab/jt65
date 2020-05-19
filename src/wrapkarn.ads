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

package Wrapkarn is

   type Rs is tagged null record;

   First : Boolean := True;

   procedure Rs_Encode ( Dgen : in out Integer_Array; Sent : in out Integer_Array );

   procedure Rs_Decode ( Recd0 : in out Integer_Array; Era : in out Integer_Array;
                       Num : in Integer; Decoded : in out Integer_Array; Nerr : in out Integer );

end Wrapkarn;

--------------------------------------------------------------------------------
-- FILE   : general_jt.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Pack_JT; use Pack_JT;
with Interfaces; use Interfaces;

package General_JT is

   subtype Range_7 is Natural range 0 .. 6;
   subtype Range_9 is Natural range 0 .. 8;
   type Unsigned_Array_Sent is array (Range_7, Range_9) of Unsigned_8;
   type Unsigned_Array_Holder is array (Range_9, Range_7) of Unsigned_8;

   type Unsigned_Array is array(Natural range <>) of Unsigned_8;

   -- Formats a message by converting all letters to upper case and collapsing multiple blanks into one
   function Fmtmsg (Msg : in String) return String;

   -- Converted subroutines from References to be added here
   procedure Chkmsg (Msg : in out String; Nspecial : in out Integer)
     with
       Pre => Msg'Length <= 22 and Msg'Length >= 1;

   procedure Rs_Encode ( Dgen : in out Integer_Array; Sent : in out Unsigned_Array); -- Not yet implemented

   -- Performs Interleave / De-interleave
   procedure Interleave63 ( Sent : in out Unsigned_Array; Num : in Integer; Holder : in out Unsigned_Array);

   -- Performs Graycode and applys the graycode to Output
   procedure Graycode ( Sent : in Unsigned_Array; Num : in Integer; Dir : in Integer; Output : in out Unsigned_Array);

end General_JT;

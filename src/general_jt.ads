--------------------------------------------------------------------------------
-- FILE   : general_jt.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);


with Interfaces; use Interfaces;

package General_JT is

   subtype Range_7 is Natural range 0 .. 6;
   subtype Range_9 is Natural range 0 .. 8;
   type Unsigned_Array_Sent is array ( Range_7, Range_9 ) of Integer;
   type Unsigned_Array_Holder is array (Range_9, Range_7 ) of Integer;
   type Integer_Array is array(Positive range <>) of Integer;

   -- Formats a message by converting all letters to upper case and
   -- collapsing multiple blanks into one
   procedure Fmtmsg( Msg : in out String )
     with
       Pre => Msg'Length >= 1 and Msg'Length <= 22;

   -- Converted subroutines from References to be added here
   procedure Chkmsg( Msg : in out String; Cok : in out String; Nspecial : out Integer )
     with
       Pre => Msg'Length >= 1 and Msg'Length <= 22;

   --procedure Rs_Encode( Dgen : in out Integer_Array;
   --                     Sent : in out Unsigned_Array ); -- Not yet implemented

   -- Performs Interleave / De-interleave
   procedure Interleave63( Sent : in out Integer_Array; Num : in Integer;
                           Holder : in out Integer_Array )
     with
       Pre => Sent'Length >= 1 and Sent'Length <= 63;

   -- Performs Graycode and applys the graycode to Output
   procedure Graycode( Sent : in Integer_Array; Num : in Integer;
                       Dir : in Integer; Output : out Integer_Array )
     with
       Pre => Sent'Length >= 1 and Sent'Length <= 63 and Output'Length >= 1 and
       Output'Length <= 63;

end General_JT;

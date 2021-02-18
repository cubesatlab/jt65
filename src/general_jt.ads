--------------------------------------------------------------------------------
-- FILE   : general_jt.ads
-- SUBJECT: Specification of a package for [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Unsigned_Array; use Unsigned_Array;

package General_JT is

   -- Checks message for OOO report
   procedure Chkmsg(Msg : in out String; Cok : out String; Nspecial : out Integer)
     with
       Pre => Cok'First = 1 and Cok'Last = 3 and
              Msg'First = 1 and Msg'Last = 22;

   -- Performs Interleave / De-interleave
   procedure Interleave63(Sent : in out Unsigned_8_array; Num : in Integer)
     with
       Pre => (Num = -1 or Num = 1) and (Sent'First = 0 and Sent'Last = 62);

   -- Performs Graycode and applys the graycode to Output
   procedure Graycode(Sent : in out Unsigned_8_array; Dir : in Integer)
     with
       Pre => (Dir = -1 or Dir = 1) and (Sent'First = 0 and Sent'Last = 62);

   -- Collapse blank lines in Msg
   procedure Collapse_Blanks(Msg : in out String)
     with
       Pre => Msg'First = 1 and Msg'Last = 22;

end General_JT;


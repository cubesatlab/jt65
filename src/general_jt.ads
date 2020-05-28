--------------------------------------------------------------------------------
-- FILE   : general_jt.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package General_JT is

   type Integer_Array is array(Natural range <>) of Integer;

   -- Converted subroutines from References to be added here
   procedure Chkmsg(Msg : in out String; Cok : out String; NSpecial : out Integer )
     with Pre => Msg'Length >= 1 and Msg'Length <= 22 and Cok'Length = 3;

   -- Performs Interleave / De-interleave
   procedure Interleave63(Sent : in out Integer_Array; IDir : in Integer)
     with Pre => Sent'First = 0 and Sent'Last = 62;

   -- Performs Graycode and applys the graycode to Output
   procedure Graycode(Sent : in out Integer_Array; Dir : in Integer);

end General_JT;

--------------------------------------------------------------------------------
-- FILE   : general_jt.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Unsigned_Array; use Unsigned_Array;

package General_JT is

   Array_Out_Of_Bounds : exception;

   -- Checks message for OOO report
   procedure Chkmsg
     (Msg : in out String;
      Cok : out String;
      Nspecial : out Integer)
     with Pre => (Cok'First = 1 and Cok'Last = 3 and Cok'Length = 3)
     and then ((Msg'First = 1 and Msg'Last = 22 and Msg'Length = 22)
               or else raise Array_Out_Of_Bounds);

   -- Performs Interleave / De-interleave
   procedure Interleave63
     (Sent : in out Unsigned_8_array;
      Num : in Integer)
     with Pre => (Num = -1 or Num = 1)
     and then ((Sent'Length = 63 and Sent'First = 0 and Sent'Last = 62)
               or else raise Array_Out_Of_Bounds);

   -- Performs Graycode and applys the graycode to Output
   procedure Graycode
     (Sent : in out Unsigned_8_array;
      Dir : in Integer)
     with Pre => (Dir = -1 or Dir = 1)
     and then ((Sent'Length = 63 and Sent'First = 0 and Sent'Last = 62)
               or else raise Array_Out_Of_Bounds);

   -- Collapse blank lines in Msg
   procedure Collapse_Blanks
     (Msg : in out String)
     with
       Pre => (Msg'First >= 1 and Msg'Last <= 22 and Msg'Length = 22)
     or else raise Array_Out_Of_Bounds;


end General_JT;

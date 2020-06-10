--------------------------------------------------------------------------------
-- FILE   : wrapkarn.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

--with Pack_JT;
with Interfaces; use Interfaces;
--with General_JT; use General_JT;
with Init_Rs; use Init_Rs;
with Unsigned_Array; use Unsigned_Array;

package Wrapkarn is

   First : Boolean := True;
   Array_Out_Of_Bounds : exception;

   procedure Encode_Rs_Int( Rs : in Rs_Access;
                            Data : in Unsigned_32_Array;
                            Bb : out Unsigned_32_Array)
     with
          Pre => (Bb'First = 0 and Bb'Last = 50 and Bb'Length = 51
                  and Data'First = 0 and Data'Last = 11 and Data'Length = 12)
        or else raise Array_Out_Of_Bounds;

   function Decode_Rs_Int( Rs : in Rs_Access;
                           Data_In : in Unsigned_32_Array;
                           Eras_Pos_In : in Unsigned_32_Array;
                           No_Eras : in  Unsigned_32 ) return Unsigned_32_Array
     with Pre => No_Eras = 0 and then
     ((Data_In'First = 0 and Data_In'Last = 62 and Data_In'Length = 63 and
           Eras_Pos_In'First = 0 and Eras_Pos_In'Last = 49 and Eras_Pos_In'Length = 50)
      or else raise Array_Out_Of_Bounds);

   procedure Rs_Encode( Dgen : in Unsigned_32_Array;
                        Sent : in out Unsigned_32_Array )
     with Global => (In_Out => (Reed_S, First)),
     Pre => ((Dgen'First = 0 and Dgen'Last = 11 and Dgen'Length = 12
             and Sent'First = 0 and Sent'Last = 62 and Sent'Length = 63)
             or else raise Array_Out_Of_Bounds);

   procedure Rs_Decode( Recd0 : in Unsigned_32_Array;
                        Era : in Unsigned_32_Array;
                        Num : in Unsigned_32;
                        Decoded : out Unsigned_32_Array )
     with Global => (In_Out => (Reed_S, First)),
     Pre => Num = 0 and then
     ((Recd0'First = 0 and Recd0'Last = 62 and Recd0'Length = 63
       and Era'First = 0 and Era'Last = 50 and Era'Length = 51
       and Decoded'First = 0 and Decoded'Last = 11 and Decoded'Length = 12)
      or else raise Array_Out_Of_Bounds);

end Wrapkarn;


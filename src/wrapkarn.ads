--------------------------------------------------------------------------------
-- FILE   : wrapkarn.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;
with Unsigned_Array; use Unsigned_Array;

package Wrapkarn is

   First : Boolean := True;
   Array_Out_Of_Bounds : exception;

   procedure Rs_Encode( Dgen : in Unsigned_8_array;
                        Sent : in out Unsigned_8_array )
     with
       Pre => ((Dgen'First = 0 and Dgen'Last = 11 and Dgen'Length = 12
               and Sent'First = 0 and Sent'Last = 62 and Sent'Length = 63)
               or else raise Array_Out_Of_Bounds);

   procedure Rs_Decode( Recd0 : in Unsigned_8_array;
                        Era : in Unsigned_8_array;
                        Num : in Unsigned_8;
                        Decoded : out Unsigned_8_array;
                        Dat_Packed : in Unsigned_8_Array )
     with
       Pre => Num = 0 and then
       ((Recd0'First = 0 and Recd0'Last = 62 and Recd0'Length = 63
         and Era'First = 0 and Era'Last = 50 and Era'Length = 51
         and Decoded'First = 0 and Decoded'Last = 11 and Decoded'Length = 12
         and Dat_Packed'First = 0 and Dat_Packed'Last = 11 and Dat_Packed'Length = 12)
        or else raise Array_Out_Of_Bounds),
       Post => (Decoded'Length = 12 and Decoded'First = 0 and Decoded'Last = 11)
       or else raise Array_Out_Of_Bounds;

private

   type Unsigned_Array is
     array(Natural range 0 .. 63) of Unsigned_8;
   type Unsigned_Array_2 is
     array(Natural range 0 .. 51) of Unsigned_8;

   type Rs is limited
      record
         Mm : Unsigned_8 := 0;
         Nn : Unsigned_8 := 0;
         Alpha_To : Unsigned_Array := (others => 0);
         Index_Of : Unsigned_Array := (others => 0);
         Genpoly : Unsigned_Array_2 := (others => 0);
         Nroots : Unsigned_8 := 0;
         Fcr : Unsigned_8 := 0;
         Prim : Unsigned_8 := 0;
         Iprim : Unsigned_8 := 0;
         Pad : Unsigned_8 := 0;
      end record;

   Reed_S : Rs;

   procedure Init_Rs_Int ( Reed_S : in out Rs;
                           Symsize : in Unsigned_8;
                           Gfpoly : in Unsigned_8;
                           Fcr : in Unsigned_8;
                           Prim : in Unsigned_8;
                           Nroots : in Unsigned_8;
                           Pad : in Unsigned_8)
     with
       Global => null,
       Pre =>  Gfpoly > 0 and Symsize <= 6 and Prim > 0,
       Post => Reed_S.Nn <= 63;

   function Modnn ( Reed_S : in Rs;
                    A : in Unsigned_8) return Integer
     with
       Global => null,
       Pre => Reed_S.Nn <= 63,
       Post => Modnn'Result <= 63 and Modnn'Result >= 0;

   procedure Encode_Rs_Int( Reed_S : in Rs;
                            Data : in Unsigned_8_array;
                            Bb : out Unsigned_8_array)
     with
       Global => null,
       Pre => Reed_S.Nroots <= 51 and then ((Bb'First = 0 and Bb'Last = 50 and Bb'Length = 51
               and Data'First = 0 and Data'Last = 11 and Data'Length = 12)
               or else raise Array_Out_Of_Bounds);

   function Decode_Rs_Int( Reed_S : in Rs;
                           Data_In : in Unsigned_8_array;
                           Eras_Pos_In : in Unsigned_8_array;
                           No_Eras : in  Unsigned_8 ) return Unsigned_8_array
     with
       Global => null,
       Pre => (No_Eras = 0 and Reed_S.Nn <= 63) and then
       ((Data_In'First = 0 and Data_In'Last = 62 and Data_In'Length = 63 and
               Eras_Pos_In'First = 0 and Eras_Pos_In'Last = 49 and Eras_Pos_In'Length = 50)
        or else raise Array_Out_Of_Bounds),
       Post => Decode_Rs_Int'Result'Length = 63 and
       Decode_Rs_Int'Result'First = 0 and
       Decode_Rs_Int'Result'Last = 62;

end Wrapkarn;


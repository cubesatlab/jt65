--------------------------------------------------------------------------------
-- FILE   : general_jt.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Ada.Text_IO; use Ada.Text_IO;

package body General_JT is

   procedure Chkmsg( Msg : in out String; Cok : in out String; Nspecial : out Integer ) is
      Flip : Float;
   begin
      Nspecial := 0;
      Flip := 1.0;
      Cok := "   ";

      for I in reverse Msg'Range loop
         if Msg(I) /= ' ' then
            if I > 11 then
               if Msg(I-3 .. I) = " 000" or Msg(Msg'First + 19 .. Msg'First + 21) = " 00" then
                  Cok := "000";
                  Flip :=-1.0;
                  if (Msg(Msg'First + 19 .. Msg'First + 21) = " 00") then
                     Msg := Msg(Msg'First .. Msg'First + 18);
                  else
                     Msg := Msg(Msg'First .. I-4);
                  end if;
               end if;
            end if;
         else
         if (Msg = "RO                    ") then Nspecial := 2; end if;
         if (Msg = "RRR                   ") then Nspecial := 3; end if;
         if (Msg = "73                    ") then Nspecial := 4; end if;
         end if;
      end loop;
   end ChkMsg;

   procedure Interleave63( Sent : in out Integer_Array; Num : in Integer;
                           Holder : in out Integer_Array ) is

      Index: Integer;

      Sent_Tmp, Sent_Tmp2 : Integer_Array(0 .. 62);

      type Interleave_Array is array(0 .. 6, 0 .. 8) of Integer;
      type Interleave_Array_Tmp is array(0 .. 8, 0 .. 6) of Integer;

      D1 : Interleave_Array;
      D2 : Interleave_Array_Tmp;

   begin
      Sent_Tmp(0 .. 62) := Sent(1 .. 63);
      Sent_Tmp2(0 .. 62) := Sent(1 .. 63);
      if Num > 0 then
         for I in D1'Range(1) loop
            for J in D1'Range(2) loop
               D1(I, J) := Sent_Tmp(I + 7*J);
            end loop;
         end loop;
         for I in D1'Range(1) loop
            for J in D1'Range(2) loop
               D2(J, I) := D1(I, J);
            end loop;
         end loop;
          for I in D1'Range(1) loop
            for J in D1'Range(2) loop
               Sent_Tmp2(I + 7*J) := D1(I,J);
            end loop;
         end loop;
         Index := 1;
         for I in D1'Range(1) loop
            for J in D1'Range(2) loop
               Sent(Index) := Sent_Tmp2(I + 7*J);
               Index := Index + 1;
            end loop;
         end loop;
      else
         Index := 0;
          for I in D2'Range(1) loop
            for J in D2'Range(2) loop
               Sent_Tmp2(Index) := Sent_Tmp(I + 9*J);
                Index := Index + 1;
            end loop;
         end loop;
         Sent(1 .. 63) := Sent_Tmp2(0 .. 62);
      end if;
   end Interleave63;

   procedure Graycode( Sent : in out Integer_Array; Num : in Integer;
                        Dir : in Integer) is
      To_Output : Integer;
      Tmp_Array : Integer_Array(1 .. 63);

      function Igray( Num : in out Integer; Dir : in Integer ) return Integer is
         Num_8: Unsigned_8;
         Sh  : Unsigned_8;
         Nn  : Unsigned_8;
      begin
         Num_8 := Interfaces.Unsigned_8(Num);
         if Dir > 0 then return Integer(Num_8 xor Shift_Right( Num_8, 1 )); end if;
         Sh := 1;
         Nn := Interfaces.Shift_Right(Num_8, 1);
           While Nn > 0 loop
              Num_8 := Num_8 xor Nn;
              Sh := Shift_Left( Sh, 1 );
              Nn := Shift_Right( Num_8, Integer(Sh) );
           end loop;
         Num := Integer(Num_8);
         return Num;
      end Igray;

   begin

      for I in 1 .. Num loop
         To_Output := Igray ( Sent(I), Dir );
         Tmp_Array(I) := To_Output;
      end loop;
      for I in 1 .. Num loop
         Sent(I) := Tmp_Array(I);
      end loop;
   end Graycode;
end General_JT;

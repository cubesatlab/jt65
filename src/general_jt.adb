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

package body General_JT is

   procedure Fmtmsg ( Msg : in out String ) is
   begin
      for I in Msg'Range loop
         if Msg(I) = ' ' then
            if I = Msg'Length then
               exit;
            else
               if Msg(I+1) = ' ' then
                  Msg(I) := Character'Val (127);
               end if;
            end if;
         else
            null;
         end if;
      end loop;
      Msg := To_Upper(Msg);
   end Fmtmsg;

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

      procedure Move( First : in Integer_Array; Second : in out Integer_Array;
                      Num : Integer ) is
      begin
         for I in 1 .. Num loop
             Second(I) := First(I);
         end loop;
      end Move;

      procedure Sent_Into_Matrix( Sent_Matrix : out Unsigned_Array_Sent;
                                  Sent : in Integer_Array ) is
      Index : Integer := 1;
      begin
         for M in 0 .. 6 loop
            for N in 0 .. 8 loop
            Sent_Matrix(M,N) := Sent(Index);
            Index := Index + 1;
            end loop;
         end loop;
      end Sent_Into_Matrix;

      procedure Matrix_Into_Sent( Sent : in out Integer_Array;
                                  Sent_Matrix : in Unsigned_Array_Sent ) is
      Index : Integer := 1;
      begin
          for M in 0 .. 6 loop
            for N in 0 .. 8 loop
               Sent(Index) := Sent_Matrix(M,N);
               Index := Index + 1;
            end loop;
         end loop;
      end Matrix_Into_Sent;

      Sent_Matrix   : Unsigned_Array_Sent;
      Holder_Matrix : Unsigned_Array_Holder;
      Index         : Integer;

   begin

      if Num > 0 then

         Sent_Into_Matrix(Sent_Matrix, Sent);
         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               Holder_Matrix(J, I) := Sent_Matrix(I, J);
            end loop;
         end loop;

         Index := 1;
         for C in 0 .. 8 loop
            for D in 0 .. 6 loop
               Holder(Index) := Holder_Matrix(C,D);
               Index := Index + 1;
            end loop;
         end loop;
         Move(Holder, Sent, 63);
      else

         Index := 1;
         for M in 0 .. 8 loop
            for N in 0 .. 6 loop
            Holder_Matrix(M,N) := Holder(Index);
            Index := Index + 1;
            end loop;
         end loop;

         Move(Sent, Holder, 63);
         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               Sent_Matrix(I, J) := Holder_Matrix(J, I);
            end loop;
         end loop;
         Matrix_Into_Sent(Sent, Sent_Matrix);
      end if;

   end Interleave63;

   procedure Graycode( Sent : in Integer_Array; Num : in Integer;
                        Dir : in  Integer; Output : out Integer_Array ) is
      My_Num : Unsigned_8;
      To_Output : Unsigned_8;

      function Igray( Num : in Unsigned_8; Dir : in Integer ) return Unsigned_8 is
         New_Num : Unsigned_8 := Num;
         Sh  : Unsigned_8;
         Nn  : Unsigned_8;
         Tmp : Natural;

      begin
         if Dir > 0 then return New_Num or Shift_Right( New_Num, 1 ); end if;
         Sh := 1;
         Nn := Interfaces.Shift_Right(New_Num, 1);
         While Nn > 0 loop
            New_Num := New_Num or Nn;
            Sh := Shift_Left( Sh, 1 );
            Tmp := Integer( Sh );
            Nn := Shift_Right( New_Num, Tmp );
         end loop;
         return New_Num;
      end Igray;

   begin
      for I in 1 .. Num loop
         My_Num := Unsigned_8(Sent(I));
         To_Output := Igray ( My_Num, Dir );
         Output(I) := Integer(To_Output);
      end loop;
   end Graycode;

end General_JT;

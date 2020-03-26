with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;
with Interfaces; use Interfaces;
with Ada.Numerics.Generic_Real_Arrays;

package body General_JT is

   function Fmtmsg (Msg : in out String) return String is
      Formatted_Msg : String := Msg;
   begin

      for I in Formatted_Msg'Range loop

         if Formatted_Msg(I) = ' ' then
            if I = Formatted_Msg'Length then
               exit;
            else
               if Formatted_Msg(I+1) = ' ' then
                  Formatted_Msg(I) := Character'Val (127);
                  Formatted_Msg := Formatted_Msg;
               end if;
            end if;

         else
            null;
         end if;

      end loop;

      Formatted_Msg := To_Upper(Formatted_Msg);

      return Formatted_Msg;

   end Fmtmsg;

   procedure Chkmsg (Msg : in out String; Nspecial : in out Integer) is
      Cok : String (1 .. 3);
      Flip : Float;
      --Nspecial : Integer;

   begin
      Nspecial := 0;
      Flip := 1.0;
      Cok := "   ";

      for I in reverse Msg'Range loop

         if ( Msg(I) /= ' ') then

            if (I > 11) then

               if (Msg(I-3 .. I) = " 000" or Msg(Msg'First + 19 .. Msg'First + 21) = " 00") then

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

   procedure Rs_Encode ( Dgen : in out Integer_Array; Sent : in out Unsigned_Array)
   is
   begin
      null;
   end Rs_Encode;


   procedure Interleave63 ( Sent : in out Unsigned_Array; Num : in Integer; Holder : in out Unsigned_Array) is

      procedure Move (First : in out Unsigned_Array; Second : in out Unsigned_Array; Num : Integer) is
      begin
         for I in 1 .. Num loop
             Second(I) := First(I);
         end loop;
      end Move;

      procedure Sent_Into_Matrix ( Sent_Matrix : in out Unsigned_Array_Sent; Sent : in out Unsigned_Array) is
      Index : Integer := 1;
      begin
         for M in 0 .. 6 loop
            for N in 0 .. 8 loop
            Sent_Matrix(M,N) := Sent(Index);
            Index := Index + 1;
            end loop;
         end loop;
      end Sent_Into_Matrix;

      procedure Matrix_Into_Sent ( Sent : in out Unsigned_Array; Sent_Matrix : in out Unsigned_Array_sent) is
      Index : Integer := 1;
      begin
          for M in 0 .. 6 loop
            for N in 0 .. 8 loop
               Sent(Index) := Sent_Matrix(M,N);
               Index := Index + 1;
            end loop;
         end loop;
      end Matrix_Into_Sent;

      Sent_Matrix : Unsigned_Array_Sent;
      Holder_Matrix : Unsigned_Array_Holder;
      Index : Integer;

   begin

      if (Num > 0) then

         Sent_Into_Matrix(Sent_Matrix, Sent);

         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               Holder_Matrix(J, I) := Sent_Matrix(I, J);
            end loop;
         end loop;

         Matrix_Into_Sent(Sent, Sent_Matrix);

         Index := 1;
         for C in 0 .. 8 loop
            for D in 0 .. 6 loop
               Holder(Index) := Holder_Matrix(C,D);
               Index := Index + 1;
            end loop;
         end loop;

         Move(Holder, Sent, 63);

      else

        Sent_Into_Matrix(Sent_Matrix, Sent);

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

   procedure Graycode ( Sent : in out Unsigned_Array; Num : in Integer; Dir : in  Integer; Output : in out Unsigned_Array) is

      function Igray (Num : in out Unsigned_8; Dir : in Integer) return Unsigned_8 is
         Sh : Unsigned_8;
         Nn : Unsigned_8;
         Tmp : Natural;
      begin

         if (Dir > 0) then return Num or (Shift_Right (Num, 1)); end if;

         Sh := 1;
         Nn := Interfaces.Shift_Right(Num, 1);

         While (Nn > 0) loop
            Num := Num or Nn;
            Sh := Shift_Left(Sh, 1);
            Tmp := Integer(Sh);
            Nn := Shift_Right(Num, Tmp);
         end loop;

         return Num;

      end Igray;

   begin

      for I in 1 .. Num loop
         Output(I) := Igray(Sent(I), Dir);
      end loop;

   end Graycode;

end General_JT;

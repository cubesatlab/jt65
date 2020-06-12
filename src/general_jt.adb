--------------------------------------------------------------------------------
-- FILE   : general_jt.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;

package body General_JT is

   procedure Chkmsg(Msg : in out String; Cok : out String; NSpecial : out Integer) is
   begin
      NSpecial := 0;
      Cok := "   ";

      for I in reverse Msg'Range loop
         if Msg(I) /= ' ' then
            if I > 11 then
               if Msg(I-3 .. I) = " 000" or Msg(Msg'First + 19 .. Msg'First + 21) = " 00" then
                  Cok := "000";
                  if (Msg(Msg'First + 19 .. Msg'Last) = " 00" and Msg(Msg'First .. Msg'First + 10) /= "           " )then
                     Msg(20 .. 22) := "   ";
                  else
                     for J in 1 .. 4 loop
                        Msg(I - J) := ' ';
                     end loop;
                  end if;
               end if;
            end if;
         else
         if (Msg = "RO                    ") then NSpecial := 2; end if;
         if (Msg = "RRR                   ") then NSpecial := 3; end if;
         if (Msg = "73                    ") then NSpecial := 4; end if;
         end if;
      end loop;
   end ChkMsg;

   procedure Interleave63( Sent : in out Unsigned_8_array;
                           Num : in Integer ) is

      type Interleave_Array_7by9 is array(0 .. 6, 0 .. 8) of Unsigned_8;
      type Interleave_Array_9by7 is array(0 .. 8, 0 .. 6) of Unsigned_8;

      D1 : Interleave_Array_7by9;
      D2 : Interleave_Array_9by7;

   begin
      if Num > 0 then
         -- Copy Sent into D1 (column major order).
         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               D1(I, J) := Sent(I + 7*J);
            end loop;
         end loop;

         -- Do the interleave
         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               D2(J, I) := D1(I, J);
            end loop;
         end loop;

         -- Copy D2 back into Sent (column major order).
         for I in 0 .. 8 loop
            for J in 0 .. 6 loop
               Sent(I + 9*J) := D2(I, J);
            end loop;
         end loop;
      else
         -- Copy Sent into D2 (column major order).
         for I in 0 .. 8 loop
            for J in 0 .. 6 loop
               D2(I, J) := Sent(I + 9*J);
            end loop;
         end loop;

         -- Do the de-interleave
         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               D1(I, J) := D2(J, I);
            end loop;
         end loop;

         -- Copy D1 back into Sent (column major order).
         for I in 0 .. 6 loop
            for J in 0 .. 8 loop
               Sent(I + 7*J) := D1(I, J);
            end loop;
         end loop;
      end if;
   end Interleave63;

   procedure Graycode(Sent : in out Unsigned_8_array;
                      Dir : in Integer)
   is
      Temporary_Array : Unsigned_8_array(Sent'Range);

      function IGray(Num : in Unsigned_8; Dir : in Integer) return Unsigned_8
      is
         Num_8: Unsigned_8;
         Sh   : Unsigned_8;
         Nn   : Unsigned_8;
      begin
         Num_8 := Num;
         if Dir > 0 then return Num_8 xor Shift_Right(Num_8, 1); end if;
         Sh := 1;
         Nn := Interfaces.Shift_Right(Num_8, 1);
           While Nn > 0 loop
              Num_8 := Num_8 xor Nn;
              Sh := Shift_Left(Sh, 1);
              Nn := Shift_Right(Num_8, Integer(Sh));
           end loop;
         return Num_8;
      end IGray;

   begin
      for I in Sent'Range loop
         Temporary_Array(I) := IGray(Sent(I), Dir);
      end loop;
      Sent := Temporary_Array;
   end Graycode;
end General_JT;

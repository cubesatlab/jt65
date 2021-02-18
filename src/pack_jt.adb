--------------------------------------------------------------------------------
-- FILE   : pack_jt.adb
-- SUBJECT: Specification of a package for [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Pfx;
use  Pfx;

package body Pack_JT is

   -----------------------------------------
   -- Implementations of Visible Subprograms
   -----------------------------------------

   -- Might need to fix the index for the arrays.
   procedure Pack_Bits
     (DBits : in     Unsigned_8_Array;
      NSymd : in     Integer;
      M0    : in     Integer;
      Sym   :    out Unsigned_8_Array)
   is
      -- Might need to change the types for n and m.
      K : Integer := 0;
      N : Unsigned_8;
      M : Unsigned_8;
   begin
      Sym := (others => 0);
      for I in 0 .. NSymd-1 loop
         N := 0;
         for J in 0 .. M0-1 loop
            M := DBits(K);
            K := K + 1;
            N := Shift_Left(N,1) or M;
            pragma Loop_Invariant(K in Dbits'Range);
         end loop;
         Sym(I) := N;
      end loop;
   end Pack_Bits;


   -- Might need to fix the index for the arrays.
   procedure Unpack_Bits
     (Sym   : in     Unsigned_8_Array;
      NSymd : in     Integer;
      M0    : in     Integer;
      DBits :    out Unsigned_8_Array)
   is
      K : Integer := 0;
      Mask : Unsigned_8;
   begin
      Dbits := (others => 0);
      for I in 0 .. NSymd - 1 loop
         Mask := Shift_Left(1, M0 - 1);
         for J in 0 .. M0 - 1 loop
            K := K + 1;
            DBits(K) := 0;
            if (Mask and Sym(I)) /= 0 then
               DBits(K) := 1;
            end if;
            Mask := Shift_Right(Mask, 1);
         end loop;
      end loop;
   end Unpack_Bits;


   --Packs a valid callsign into a 28-bit integer
   procedure Pack_Call(Call : in out String; NCall : in out Unsigned_32; Text : out Boolean) is
      NBASE : constant Unsigned_32 := 37*36*10*27*27*27;
      C     : Character;
      TMP   : String(1 .. 6);
      N1    : Integer;
      N2    : Integer;
      N3    : Integer;
      N4    : Integer;
      N5    : Integer;
      N6    : Integer;
      NFreq : Unsigned_32;
   begin
      Text := False;
      if Call(Call'First .. Call'First + 3) = "3DA0" then
         Call := "3D0" & Call(Call'First + 4 .. Call'First + 5) & ' ';
      end if;
      if Call(Call'First .. Call'First + 1) = "3X" and Call(Call'First + 2) >= 'A' and Call(Call'First + 2) <= 'Z' then
         Call := 'Q' & Call(Call'First + 2 .. Call'First + 5) & ' ';
      end if;
      if Call(Call'First .. Call'First + 2) = "CQ " then
         NCall := NBASE + 1;
         if Call(Call'First + 3) >= '0' and Call(Call'First + 3) <= '9' and Call(Call'First + 4) >= '0' and Call(Call'First + 4) <= '9' and Call(Call'First + 5) >= '0' and Call(Call'First + 5) <= '9' then
            NFreq := Unsigned_32'Value(Call(Call'First + 3 .. Call'First + 5));
            NCall := NBASE + 3 + NFreq;
         end if;
         return;
      elsif Call(Call'First .. Call'First + 3) = "QRZ " then
         NCall := NBASE + 2;
         return;
      elsif Call(Call'First .. Call'First + 2) = "DE " then
         NCall := 267796945;
         return;
      end if;
      if Call(Call'First + 2) >= '0' and Call(Call'First + 2) <= '9' then
         --TMP := Call;
         Move(Call,TMP, Right, Left, Space);
      elsif Call(Call'First + 1) >= '0' and Call (Call'First + 1) <= '9' then
         if Call(Call'First + 5) /= ' ' then
            Text := True;
            return;
         end if;
         TMP := ' ' & Call(Call'First .. Call'First + 4);
      else
         Text := True;
         return;
      end if;

      for I in 1 .. 6 loop
         C := TMP(I);
         if C >= 'a' and C <= 'z' then
            TMP(I) := Character'Val(Character'Pos(C)-Character'Pos('a') + Character'Pos('A'));
         end if;
      end loop;

      N1 := 0;
      if (TMP(1) >= 'A' and TMP(1) <= 'Z') or TMP(1) = ' ' then N1 := 1; end if;
      if TMP(1) >= '0' and TMP(1) <= '9' then N1 := 1; end if;
      N2 := 0;
      if TMP(2) >= 'A' and TMP(2) <= 'Z' then N2 := 1; end if;
      if TMP(2) >= '0' and TMP(2) <= '9' then N2 := 1; end if;
      N3 := 0;
      if TMP(3) >= '0' and TMP(3) <= '9' then N3 := 1; end if;
      N4 := 0;
      if (TMP(4) >= 'A' and TMP(4) <= 'Z') or TMP(4) = ' ' then N4 := 1; end if;
      N5 := 0;
      if (TMP(5) >= 'A' and TMP(5) <= 'Z') or TMP(5) = ' ' then N5 := 1; end if;
      N6 := 0;
      if (TMP(6) >= 'A' and TMP(6) <= 'Z') or TMP(6) = ' ' then N6 := 1; end if;

      if N1 + N2 + N3 + N4 + N5 + N6 /= 6 then
         text := True;
         return;
      end if;

      NCall := Unsigned_32(NChar(TMP(1)));
      NCall := 36 * NCall + Unsigned_32(NChar(TMP(2)));
      NCall := 10 * NCall + Unsigned_32(NChar(TMP(3)));
      NCall := 27 * NCall + Unsigned_32(NChar(TMP(4))) - 10;
      NCall := 27 * NCall + Unsigned_32(NChar(TMP(5))) - 10;
      NCall := 27 * NCall + Unsigned_32(NChar(TMP(6))) - 10;
   end Pack_Call;


   procedure Unpack_Call
     (NCall : in     Unsigned_32;
      Word  :    out String;
      Iv2   :    out Integer;
      Psfx  :    out String)
   is
      --NBASE : constant Integer := 37 * 36 * 10 * 27 * 27 * 27;
      C : constant String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
      N : Integer := Integer(NCall);
      I : Integer;
   begin

      --Word := "......";
      Move("......", Word, Right, Left, Space);
      --Psfx := "    ";
      Move("    ", Psfx, Right, Left, Space);
      Iv2 := 0;

      if N >= 262177560 then
         if N >= 267796946 then
            if Word(Word'First .. Word'First + 2) = "3D0" then
               Word := "3DA0" & Word(Word'First + 3 .. Word'Last - 1);
            end if;
            if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
               Word := "3X" & Word(Word'First + 1 .. Word'Last - 1);
            end if;
            return;
         end if;

         if N >= 262178563 and N <= 264002071 then
            Iv2 := 1;
            N := N - 262178563;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 3) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 2) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 1) := C(I);
            N := N / 37;

            I := N + 1;
            Psfx(Psfx'First) := C(I);

         elsif N >= 264002072 and N <= 265825580 then
            Iv2 := 2;
            N := N - 264002072;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 3) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 2) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 1) := C(I);
            N := N / 37;

            I := N + 1;
            Psfx(Psfx'First) := C(I);

         elsif N >= 265825581 and N <= 267649089 then
            Iv2 := 3;
            N := N - 265825581;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 3) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 2) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 1) := C(I);
            N := N / 37;

            I := N + 1;
            Psfx(Psfx'First) := C(I);

         elsif N >= 267649090 and N <= 267698374 then
            Iv2 := 4;
            N := N - 267649090;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 2) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 1) := C(I);
            N := N / 37;

            I := N + 1;
            Psfx(Psfx'First) := C(I);

         elsif N >= 267698375 and N <= 267747659 then
            Iv2 := 5;
            N := N - 267698375;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 2) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 1) := C(I);
            N := N / 37;

            I := N + 1;
            Psfx(Psfx'First) := C(I);

         elsif N >= 267747660 and N <= 267796944 then
            Iv2 := 6;
            N := N - 267747660;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 2) := C(I);
            N := N / 37;

            I := (N mod 37) + 1;
            Psfx(Psfx'First + 1) := C(I);
            N := N / 37;

            I := N + 1;
            Psfx(Psfx'First) := C(I);

         elsif N = 267796945 then
            Iv2 := 7;
            Psfx(1 .. 4) := "    ";

         end if;
         if Word(Word'First .. Word'First + 2) = "3D0" then
            Word := "3DA0" & Word(Word'First + 3 .. Word'Last - 1);
         end if;
         if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
            Word := "3X" & Word(Word'First + 1 .. Word'Last - 1);
         end if;
         return;
      end if;

      I := (N mod 27) + 11;
      Word(Word'First + 5) := C(I);
      N := N / 27;

      I := (N mod 27) + 11;
      Word(Word'First + 4) := C(I);
      N := N / 27;

      I := (N mod 27) + 11;
      Word(Word'First + 3) := C(I);
      N := n / 27;

      I := (N mod 10) + 1;
      Word(Word'First + 2) := C(I);
      N := N / 10;

      I := (N mod 36) + 1;
      Word(Word'First + 1) := C(I);
      N := N / 36;

      I := N + 1;
      Word(Word'First) := C(I);

      for X in 1 .. 4 loop
         if Word(Word'First + X - 1) /= ' ' then
            --Word := Word(X .. Word'Last);
            Collapse_Blanks_12(Word);
            --Move(Word(X .. Word'Last), Word, Right, Left, Space);
            --Word(Word'First .. Word(X .. Word'Last)'Length) := Word(X .. Word'Last);
            if Word(Word'First .. Word'First + 2) = "3D0" then
               Word := "3DA0" & Word(Word'First + 3 .. Word'Last - 1);
            end if;
            if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
               Word := "3X" & Word(Word'First + 1 .. Word'Last - 1);
            end if;
            return;
         end if;
      end loop;
      if Word(Word'First .. Word'First + 2) = "3D0" then
         Word := "3DA0" & Word(Word'First + 3 .. Word'Last - 1);
      end if;
      if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
         Word := "3X" & Word(Word'First + 1 .. Word'Last - 1);
      end if;
   end Unpack_Call;

   --  procedure Unpack_Call
   --    (NCall : in     Unsigned_32;
   --     Word  :    out String;
   --     Iv2   :    out Integer;
   --     Psfx  :    out String)
   --  is
   --     --NBASE : constant Integer := 37 * 36 * 10 * 27 * 27 * 27;
   --     C : constant String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
   --     N : Integer := Integer(NCall);
   --     I : Integer := 0;
   --
   --     procedure Nine_Nine_Nine is
   --     begin
   --        if Word(Word'First .. Word'First + 2) = "3D0" then
   --           Word := "3DA0" & Word(Word'First + 3 .. Word'Last - 1);
   --        end if;
   --        if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
   --           Word := "3X" & Word(Word'First + 1 .. Word'Last - 1);
   --        end if;
   --     end Nine_Nine_Nine;
   --
   --
   --     procedure Twenty
   --       with
   --         Pre =>
   --           Word'First = 1 and Word'Last = 12 and
   --           Psfx'First = 1 and Psfx'Last = 4
   --     is
   --     begin
   --        if N >= 267796946 then
   --           Nine_Nine_Nine;
   --           return;
   --        end if;
   --
   --        if N >= 262178563 and N <= 264002071 then
   --           Iv2 := 1;
   --           N := N - 262178563;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 3) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 2) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 1) := C(I);
   --           N := N / 37;
   --
   --           I := N + 1;
   --           Psfx(Psfx'First) := C(I);
   --
   --        elsif N >= 264002072 and N <= 265825580 then
   --           Iv2 := 2;
   --           N := N - 264002072;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 3) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 2) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 1) := C(I);
   --           N := N / 37;
   --
   --           I := N + 1;
   --           Psfx(Psfx'First) := C(I);
   --
   --        elsif N >= 265825581 and N <= 267649089 then
   --           Iv2 := 3;
   --           N := N - 265825581;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 3) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 2) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 1) := C(I);
   --           N := N / 37;
   --
   --           I := N + 1;
   --           Psfx(Psfx'First) := C(I);
   --
   --        elsif N >= 267649090 and N <= 267698374 then
   --           Iv2 := 4;
   --           N := N - 267649090;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 2) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 1) := C(I);
   --           N := N / 37;
   --
   --           I := N + 1;
   --           Psfx(Psfx'First) := C(I);
   --
   --        elsif N >= 267698375 and N <= 267747659 then
   --           Iv2 := 5;
   --           N := N - 267698375;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 2) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 1) := C(I);
   --           N := N / 37;
   --
   --           I := N + 1;
   --           Psfx(Psfx'First) := C(I);
   --
   --        elsif N >= 267747660 and N <= 267796944 then
   --           Iv2 := 6;
   --           N := N - 267747660;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 2) := C(I);
   --           N := N / 37;
   --
   --           I := (N mod 37) + 1;
   --           Psfx(Psfx'First + 1) := C(I);
   --           N := N / 37;
   --
   --           I := N + 1;
   --           Psfx(Psfx'First) := C(I);
   --
   --        elsif N = 267796945 then
   --           Iv2 := 7;
   --           Psfx := "    ";
   --
   --        end if;
   --        Nine_Nine_Nine;
   --     end Twenty;
   --
   --  begin  -- Unpack_Call
   --     --Word := "......";
   --     Move("......", Word, Right, Left, Space);
   --     --Psfx := "    ";
   --     Move("    ", Psfx, Right, Left, Space);
   --     Iv2 := 0;
   --
   --     if N >= 262177560 then
   --        Twenty;
   --        return;
   --     end if;
   --
   --     I := (N mod 27) + 11;
   --     Word(Word'First + 5) := C(I);
   --     N := N / 27;
   --
   --     I := (N mod 27) + 11;
   --     Word(Word'First + 4) := C(I);
   --     N := N / 27;
   --
   --     I := (N mod 27) + 11;
   --     Word(Word'First + 3) := C(I);
   --     N := n / 27;
   --
   --     I := (N mod 10) + 1;
   --     Word(Word'First + 2) := C(I);
   --     N := N / 10;
   --
   --     I := (N mod 36) + 1;
   --     Word(Word'First + 1) := C(I);
   --     N := N / 36;
   --
   --     I := N + 1;
   --     Word(Word'First) := C(I);
   --
   --     for X in 1 .. 4 loop
   --        if Word(Word'First + X - 1) /= ' ' then
   --           --Word := Word(X .. Word'Last);
   --           Collapse_Blanks_12(Word); --
   --           --Move(Word(X .. Word'Last), Word, Right, Left, Space);
   --           --Word(Word'First .. Word(X .. Word'Last)'Length) := Word(X .. Word'Last);
   --           Nine_Nine_Nine;
   --           return;
   --        end if;
   --     end loop;
   --     Nine_Nine_Nine;
   --  end Unpack_Call;


   procedure Pack_Grid(Grid : in out String; NG : in out Unsigned_32; Text : out Boolean) is
      NGBASE : constant Unsigned_32 := 180 * 180;
      C1     : Character;
      N      : Integer;
      Long   : Integer;
      Lat    : Integer;
      DLong  : Float;
      DLat   : Float;
   begin
      Text := False;
      if Grid /= "    " then
         if Grid(Grid'First) = '-' then
            if Grid(Grid'First + 1 .. Grid'First + 2) = "  " then
               Text := True;
               return;
            end if;
            for I in Grid(Grid'First + 1 .. Grid'First + 2)'range loop
               if ((Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ') then
                  Text := True;
                  return;
               end if;
            end loop;
            N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 2));
            if N >= 1 and N <= 30 then
               NG := NGBASE + 1 + Unsigned_32(N);
               return;
            end if;
            -- USE THIS SECTION FOR GO TO 10
            N := 99;
            C1 := Grid(Grid'First);
            for I in 1 .. 4 loop
               if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
                  for I in 2 .. 4 loop
                     if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
                        if N >= -50 and N <= 49 then
                           if C1 = 'R' then
                              --write(grid,1002) n+50
                              --format('LA',i2.2)
                              --Grid := "LA" & Integer'Image(N + 50);
                              Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                           else
                              --write(grid,1003) n+50
                              --format('KA',i2.2)
                              --Grid := "KA" & Integer'Image(N + 50);
                              Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                           end if;
                           Grid2Deg(Grid & "mm", DLong, DLat);
                           Long := Integer(Float'Floor(DLong));
                           Lat := Integer(Float'Floor(DLat + 90.0));
                           NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                           return;
                        end if;
                        if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
                        if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
                        if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
                        if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
                        if Text then return; end if;
                        Grid2Deg(Grid & "mm", DLong, DLat);
                        Long := Integer(Float'Floor(DLong));
                        Lat := Integer(Float'Floor(DLat + 90.0));
                        NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                        return;
                     end if;
                  end loop;
                  N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 3));
                  if N >= -50 and N <= 49 then
                     if C1 = 'R' then
                        --write(grid,1002) n+50
                        --format('LA',i2.2)
                        --Grid := "LA" & Integer'Image(N + 50);
                        Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                     else
                        --write(grid,1003) n+50
                        --format('KA',i2.2)
                        --Grid := "KA" & Integer'Image(N + 50);
                        Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                     end if;
                     Grid2Deg(Grid & "mm", DLong, DLat);
                     Long := Integer(Float'Floor(DLong));
                     Lat := Integer(Float'Floor(DLat + 90.0));
                     NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                     return;
                  end if;
                  if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
                  if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
                  if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
                  if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
                  if Text then return; end if;
                  Grid2Deg(Grid & "mm", DLong, DLat);
                  Long := Integer(Float'Floor(DLong));
                  Lat := Integer(Float'Floor(DLat + 90.0));
                  NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                  return;
               end if;
            end loop;
            N := Integer'Value(Grid);
            if N >= -50 and N <= 49 then
               if C1 = 'R' then
                  --write(grid,1002) n+50
                  --format('LA',i2.2)
                  --Grid := "LA" & Integer'Image(N + 50);
                  Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
               else
                  --write(grid,1003) n+50
                  --format('KA',i2.2)
                  --Grid := "KA" & Integer'Image(N + 50);
                  Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
               end if;
               Grid2Deg(Grid & "mm", DLong, DLat);
               Long := Integer(Float'Floor(DLong));
               Lat := Integer(Float'Floor(DLat + 90.0));
               NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
               return;
            end if;
            if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
            if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
            if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
            if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
            if Text then return; end if;
            Grid2Deg(Grid & "mm", DLong, DLat);
            Long := Integer(Float'Floor(DLong));
            Lat := Integer(Float'Floor(DLat + 90.0));
            NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
            return;
         elsif Grid(Grid'First .. Grid'First + 1) = "R-" then -- second else if
            if Grid(Grid'First + 2 .. Grid'First + 3) = "  " then
               Text := True;
               return;
            end if;
            for I in Grid(Grid'First + 2 .. Grid'First + 3)'range loop
               if ((Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ') then
                  Text := True;
                  return;
               end if;
            end loop;
            N := Integer'Value(Grid(Grid'First + 2 .. Grid'First + 3));
            if N >= 1 and N <= 30 then
            NG := NGBASE + 31 + Unsigned_32(N);
               return;
            end if;
            N := 99;
            C1 := Grid(Grid'First);
            for I in 1 .. 4 loop
               if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
                  for I in 2 .. 4 loop
                     if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
                        if N >= -50 and N <= 49 then
                           if C1 = 'R' then
                              --write(grid,1002) n+50
                              --format('LA',i2.2)
                              --Grid := "LA" & Integer'Image(N + 50);
                              Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                           else
                              --write(grid,1003) n+50
                              --format('KA',i2.2)
                              --Grid := "KA" & Integer'Image(N + 50);
                              Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                           end if;
                           Grid2Deg(Grid & "mm", DLong, DLat);
                           Long := Integer(Float'Floor(DLong));
                           Lat := Integer(Float'Floor(DLat + 90.0));
                           NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                           return;
                        end if;
                        if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
                        if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
                        if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
                     if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
                        if Text then return; end if;
                        Grid2Deg(Grid & "mm", DLong, DLat);
                        Long := Integer(Float'Floor(DLong));
                        Lat := Integer(Float'Floor(DLat + 90.0));
                        NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                        return;
                  end if;
                  end loop;
                  N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 3));
                  if N >= -50 and N <= 49 then
                     if C1 = 'R' then
                        --write(grid,1002) n+50
                        --format('LA',i2.2)
                        --Grid := "LA" & Integer'Image(N + 50);
                        Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                     else
                        --write(grid,1003) n+50
                        --format('KA',i2.2)
                        --Grid := "KA" & Integer'Image(N + 50);
                        Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                     end if;
                     Grid2Deg(Grid & "mm", DLong, DLat);
                     Long := Integer(Float'Floor(DLong));
                     Lat := Integer(Float'Floor(DLat + 90.0));
                     NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                     return;
                  end if;
                  if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
                  if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
                  if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
                  if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
                  if Text then return; end if;
                  Grid2Deg(Grid & "mm", DLong, DLat);
                  Long := Integer(Float'Floor(DLong));
                  Lat := Integer(Float'Floor(DLat + 90.0));
                  NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                  return;
               end if;
            end loop;
            N := Integer'Value(Grid);
            if N >= -50 and N <= 49 then
               if C1 = 'R' then
                  --write(grid,1002) n+50
                  --format('LA',i2.2)
                  --Grid := "LA" & Integer'Image(N + 50);
                  Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
               else
                  --write(grid,1003) n+50
                  --format('KA',i2.2)
                  --Grid := "KA" & Integer'Image(N + 50);
                  Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
               end if;
               Grid2Deg(Grid & "mm", DLong, DLat);
               Long := Integer(Float'Floor(DLong));
               Lat := Integer(Float'Floor(DLat + 90.0));
               NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
               return;
            end if;
            if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
            if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
            if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
            if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
            if Text then return; end if;
            Grid2Deg(Grid & "mm", DLong, DLat);
            Long := Integer(Float'Floor(DLong));
            Lat := Integer(Float'Floor(DLat + 90.0));
            NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
            return;
         elsif Grid(Grid'First .. Grid'First + 3) = "RO  " then
            NG := NGBASE + 62;
            return;
         elsif Grid(Grid'First .. Grid'First + 3) = "RRR " then
            NG := NGBASE + 63;
            return;
         elsif Grid(Grid'First .. Grid'First + 3) = "73  " then
            NG := NGBASE + 64;
            return;
         end if;
         N := 99;
         C1 := Grid(Grid'First);
         for I in 1 .. 4 loop
            if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
               for I in 2 .. 4 loop
                  if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
                     if N >= -50 and N <= 49 then
                        if C1 = 'R' then
                           --write(grid,1002) n+50
                           --format('LA',i2.2)
                           --Grid := "LA" & Integer'Image(N + 50);
                           Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                        else
                           --write(grid,1003) n+50
                           --format('KA',i2.2)
                           --Grid := "KA" & Integer'Image(N + 50);
                           Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                        end if;
                        Grid2Deg(Grid & "mm", DLong, DLat);
                        Long := Integer(Float'Floor(DLong));
                        Lat := Integer(Float'Floor(DLat + 90.0));
                        NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                        return;
                     end if;
                     if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
                     if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
                     if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
                     if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
                     if Text then return; end if;
                     Grid2Deg(Grid & "mm", DLong, DLat);
                     Long := Integer(Float'Floor(DLong));
                     Lat := Integer(Float'Floor(DLat + 90.0));
                     NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                     return;
                  end if;
               end loop;
               N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 3));
               if N >= -50 and N <= 49 then
                  if C1 = 'R' then
                     --write(grid,1002) n+50
                     --format('LA',i2.2)
                     --Grid := "LA" & Integer'Image(N + 50);
                     Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                  else
                     --write(grid,1003) n+50
                     --format('KA',i2.2)
                     --Grid := "KA" & Integer'Image(N + 50);
                     Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
                  end if;
                  Grid2Deg(Grid & "mm", DLong, DLat);
                  Long := Integer(Float'Floor(DLong));
                  Lat := Integer(Float'Floor(DLat + 90.0));
                  NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
                  return;
               end if;
               if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
               if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
               if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
               if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
               if Text then return; end if;
               Grid2Deg(Grid & "mm", DLong, DLat);
               Long := Integer(Float'Floor(DLong));
               Lat := Integer(Float'Floor(DLat + 90.0));
               NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
               return;
            end if;
         end loop;
         N := Integer'Value(Grid(1 .. 4));
         if N >= -50 and N <= 49 then
            if C1 = 'R' then
               --write(grid,1002) n+50
               --format('LA',i2.2)
               --Grid := "LA" & Integer'Image(N + 50);
               Move("LA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
            else
               --write(grid,1003) n+50
               --format('KA',i2.2)
               --Grid := "KA" & Integer'Image(N + 50);
               Move("KA" & Trim(Integer'Image(N + 50), Both), Grid, Right, Left, Space);
            end if;
            Grid2Deg(Grid & "mm", DLong, DLat);
            Long := Integer(Float'Floor(DLong));
            Lat := Integer(Float'Floor(DLat + 90.0));
            NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
            return;
      end if;
         if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
         if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
         if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
         if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
         if Text then return; end if;
         Grid2Deg(Grid & "mm", DLong, DLat);
         Long := Integer(Float'Floor(DLong));
         Lat := Integer(Float'Floor(DLat + 90.0));
         NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
         return;
      end if;
      Ng := NGBASE + 1;
      return;
   end Pack_Grid;


   procedure Unpack_Grid(Ng : Integer; Grid : out String) is
      subtype Integer_Value is Integer range -50 .. 33000;

      NGBASE    : constant Integer := 180 * 180;
      Grid6     : String(1 .. 6);
      DLat      : Float;
      DLong     : Float;
      N         : Integer_Value;
      Grid_Temp : String(1 .. 3);
      N_Temp    : String(1 .. 2);

      --  function Prove_Grid(Grid : in String) return Boolean is
      --     Flag : Boolean := True;
      --  begin
      --     for I in Grid'Range loop
      --        if Flag then
      --           if Grid(I) in Callsign_Type then
      --              Flag := True;
      --           else
      --              return False;
      --           end if;
      --        end if;
      --     end loop;
      --     return True;
      --  end Prove_Grid;

      --  function Prove_Valid_Char(Char : in Character) return Boolean is
      --  begin
      --     if Char in Callsign_Type then
      --        return True;
      --     else
      --        return False;
      --     end if;
      --  end Prove_Valid_Char;

      procedure Ten is
      begin
         N := Ng - NGBASE - 1;

         if N >= 1 and N <= 30 then
            --Grid := Integer'Image(-N);
            Move(Integer'Image(-N), Grid, Right, Left, Space);
         elsif N > 31 and N <= 60 then
            if N > 31 and N <= 39 then
               N := N - 30;
               --N_Temp(1 .. 2) := Integer'Image(N);
               Move(Integer'Image(N), N_Temp, Right, Left, Space);
               if N_Temp(1) = ' ' then
                  N_Temp(1) := '0';
                  Move("R-" & N_Temp, Grid, Right, Left, Space);
               else
                  N_Temp(1) := '+';
                  Move("R-" & N_Temp, Grid, Right, Left, Space);
               end if;
            else
               N := N - 30;
               --Grid := "R" & Integer'Image(-N);
               -- PERFORM THIS CHEK FOR 31 .. 60?
               Move("R" & Integer'Image(-N), Grid, Right, Left, Space);
            end if;
         elsif N = 61 then
            Grid := "RO  ";
         elsif N = 62 then
            Grid := "RRR ";
         elsif N = 63 then
            Grid := "73  ";
         end if;
      end Ten;

   begin -- Unpack_Grid
      Grid := "    ";
      --Grid6 := "      ";
      if Ng >= 32400 then
         Ten;
         return;
      end if;
      DLat := Float((Ng mod 180) - 90);
      DLong := Float((Ng / 180) * 2 - 180 + 2);
      Deg2Grid(DLong, DLat, Grid6);
      Grid := Grid6(Grid6'First .. Grid6'First + 3);
      --Prove_Grid;
      -- Handling -31 through -49.
      --N := (if Grid(Grid'First + 2) in JT65_Callsign_Character then Integer'Value(Grid(Grid'First + 2 .. Grid'Last)) else 999);


      -- First two checks ("KA" and "LA") never being accessed
      if Grid(Grid'First .. Grid'First + 1) = "KA" then
         --N := Integer'Value(Grid(Grid'First + 2 .. Grid'First + 3));
         N := (if Grid(Grid'First + 2) in JT65_Callsign_Character and Grid(Grid'Last) in JT65_Callsign_Character then Integer'Value(Grid(Grid'First + 2 .. Grid'Last)) else 0);
         if N >= 50 then
            N := N - 50;
            if N > 0 and N <= 49 then
               --if N <= 9 then
               --Grid_Temp(3 .. 3) := Trim(Integer'Image(N), Both);
               Move(Integer'Image(N), Grid_Temp, Right, Left, Space);
               --
               --Grid_Temp(Grid_Temp'First .. Grid_Temp'First + 2) := Integer'Image(N);
               --end if;
               Grid(Grid'First .. Grid'First + 1) := "  ";
               Grid(Grid'First + 1) := '-';
               Grid(Grid'First + 2 .. Grid'First + 3) := Grid_Temp(2 .. 3);
               -- Now collapsing
            elsif N <= 0 then
               Grid(Grid'First .. Grid'First + 3) := "    ";
               Grid(Grid'First + 1) := '-';
               Grid(Grid'First + 2 .. Grid'First + 3) := "00";
            end if;
         end if;
      elsif Grid(Grid'First + 2 .. Grid'First + 3) = "LA" then
         N := (if Grid(3 .. 4) in JT65_String then Integer'Value(Grid(3 .. 4)) else 0);
         if N >= 50 then
            N := N - 50;
            if N > 0 then
               Move("R" & Trim(Integer'Image(N), Both), Grid, Right, Left, Space);
               --if N <= 9 then
               --   Grid := "R   " & Trim(Integer'Image(N), Both);
               --else
               --   Grid := "R  " & Trim(Integer'Image(N), Both);
               --end if;
               if Grid(Grid'First + 1) = ' ' then
                  Grid(Grid'First + 1) := '+';
               end if;
            end if;
         end if;
      elsif Grid(Grid'First .. Grid'First + 1) = "K " then
         if Ng >= 13320 and Ng <= 13329 then -- 0 - 9
            N := Ng - 13320;
            --Grid(Grid'First + 2 .. Grid'Last) := Integer'Image(N);
            --Grid(Grid'First + 2) := '0';
            Move("0" & Trim(Integer'Image(N), Both), Grid, Right, Left, Space);
         elsif Ng >= 13140 and Ng <= 13149 then -- 10 - 19
            N := Ng - 13130;
            --Grid(Grid'First + 1 .. Grid'Last) := Integer'Image(N);
            Move(Trim(Integer'Image(N), Both), Grid, Right, Left, Space);
         elsif Ng >= 12960 and Ng <= 12969 then -- 20 - 29
            N := Ng - 12940;
            --Grid(Grid'First + 1 .. Grid'Last) := Integer'Image(N);
            Move(Trim(Integer'Image(N), Both), Grid, Right, Left, Space);
         elsif Ng >= 12780 and Ng <= 12789 then -- 30 - 39
            N := Ng - 12750;
            --Grid(Grid'First + 1 .. Grid'Last) := Integer'Image(N);
            Move(Trim(Integer'Image(N), Both), Grid, Right, Left, Space);
         elsif Ng >= 12600 and Ng <= 12609 then -- 40 - 49
            N:= Ng - 12560;
            --Grid(Grid'First + 1 .. Grid'Last) := Integer'Image(N);
            Move(Trim(Integer'Image(N), Both), Grid, Right, Left, Space);
         end if;
      end if;
   end Unpack_Grid;


   procedure Pack_Msg(Msg0 : String; Dat : out Unsigned_8_Array; IType : out Integer) is
      subtype Msg_Indeces is Integer range 1 .. 22;
      Msg      : String(1 .. 22);
      C1       : String(1 .. 12);
      C2       : String(1 .. 12);
      C3       : String(1 ..  4);
      Grid6    : String(1 ..  6) := (others => ' ');
      Text1    : Boolean := True;
      Text2    : Boolean := True;
      Text3    : Boolean := True;
      I_Start  : Integer := 1;
      Ia       : Integer;
      Ib       : Msg_Indeces;
      Ic       : Integer;
      Nv2a     : Integer := 0;
      Nv2b     : Integer := 0;
      Nc1      : Unsigned_32 := 0;
      Nc2      : Unsigned_32 := 0;
      Ng       : Unsigned_32 := 0;
      Skip_Ten : Boolean := False;

      procedure Three
        with
          Pre =>
            Msg'First = 1 and Msg'Last = 22 and C1'First = 1 and C1'Last = 12 and C2'First = 1 and C2'Last = 12
      is
         subtype Constrained_Integer is Integer range Integer'First .. 2000000;
         K  : Integer;
         K1 : Constrained_Integer;
         K2 : Constrained_Integer;
      begin
         Ic := I_Start;
         C3 := "    ";
         if Ic >= Ib + 1 then
            --C3 := Msg(ib + 1 .. Ic);
            Move(Msg(ib + 1 .. Ic), C3, Right, Left, Space);
         end if;
         if C3 = "OOO " then
            C3 := "    ";
         end if;
         Get_Pfx1(C1, K1, Nv2a);
         if Nv2a >= 4 then
            --Ten;
            return;
         end if;
         Pack_Call(C1(1 .. 6), Nc1, Text1);
         if Text1 then
           --Ten;
            return;
         end if;
         Get_Pfx1(C2, K2, Nv2b);
         Pack_Call(C2(1 .. 6), Nc2, text2);
         if Text2 then
            --Ten;
            return;
         end if;
         --Put("K1 = ");Put_Line(Integer'Image(K1));
        -- Put("K2 = ");Put_Line(Integer'Image(K2));
         if Nv2a = 2 or Nv2a = 3 or Nv2b = 2 or Nv2b = 3 then
            if K1 < 0 or K2 < 0 or K1*K2 /= 0 then
               --Ten;
               return;
            end if;
            if K2 > 0 and K2 < 1999550 then
               K2 := K2 + 450;
            end if;
            K := Integer'Max(K1, K2);
            if K > 0 then
               K2Grid(K, Grid6);
               C3 := Grid6(1 .. 4);
            end if;
         end if;
         Pack_Grid(C3, Ng, Text3);
         --Nv2b := 4;
         if Nv2a < 4 and Nv2b < 4 and not Text1 and not Text3 then
            Skip_Ten := True;
            return;
         end if;
         Nc1 := 0;
         if Nv2b = 4 and K2 >= 0 then
           if C1(1 .. 3) = "CQ " and not Text3 then
               Nc1 := 262178563 + Unsigned_32(K2);
            end if;
            if C1(1 .. 4) = "QRZ " and not Text3 then
               Nc1 := 264002072 + Unsigned_32(K2);
            end if;
            if C1(1 ..3) = "DE " and not Text3 then
               Nc1 := 265825581 + Unsigned_32(K2);
           end if;
         elsif Nv2b = 5 and K2 >= 0 then
            if C1(1 .. 3) = "CQ " and not Text3 then
               Nc1 := 267649090 + Unsigned_32(K2);
            end if;
            if C1(1 .. 4) = "QRZ " and not Text3 then
               Nc1 := 267698375 + Unsigned_32(K2);
            end if;
            if C1(1 ..3) = "DE " and not Text3 then
              Nc1 := 267747660 + Unsigned_32(K2);
            end if;
         end if;
         if Nc1 /= 0 then
            Skip_Ten := True;
         end if;
      end Three;

      procedure Two is
      begin
         ib := I_Start;
         --C2 := Msg(ia+1 .. ib-1);
         Move(Msg(ia+1 .. ib-1), C2, Right, Left, Space);

         I_Start := ib + 1;
         for I in I_Start .. 22 loop
           I_Start := I;
            if Msg(I) = ' ' then
               Three;
               exit;
            end if;
         end loop;
      end Two;

      -- Gets the second blank space.
      procedure One is
      begin
         Ia := I_Start;
         --C1 := Msg(1 .. Ia - 1);
         Move(Msg(1 .. Ia - 1), C1, Right, Left, Space);
         I_Start := Ia + 1;
         for I in I_Start .. 22 loop
            I_Start := I;
            if Msg(I) = ' ' then
               Two;
               exit;
            end if;
         end loop;
      end One;

      procedure Ten is
      begin
         IType := 6;
         Pack_Text(Msg, Nc1, Nc2, Ng);
         Ng := Ng + 32768;
      end Ten;

   begin -- Pack_Msg
      IType := 1;
      Dat := (others => 0);
      --Msg := Msg0;
      Move(Msg0, Msg, Right, Left, Space);
      --Fmtmsg(Msg); --This Needs to be fixed for now I am just using To_Upper
      Msg := To_Upper(Msg);
      if Msg(1 .. 3) = "CQ " and Msg(4) >= '0' and Msg(4) <= '9' and Msg(5) = ' ' then
         Msg := "CQ 00" & Msg(4..Msg'Last - 2); --The Last character of the Msg is cut off, I dont think it matters but it might.
      end if;
      if Msg(1 .. 6) = "CQ DX " then
         Msg(3) := '9';
      end if;
      if Msg(1 .. 3) = "CQ " and msg(4) >= 'A' and Msg(4) <= 'Z' and Msg(5) >= 'A' and Msg(5) <= 'Z' and Msg(6) = ' ' then
         --Msg := "E9" & Msg(4..Msg'Last);
         Move("E9" & Msg(4..Msg'Last), Msg, Right, Left, Space);
      end if;
      --Check if it"s a CQ message
      if Msg(1 .. 3) = "CQ " then
         I_Start := 3;
         if Msg(4) >= '0' and Msg(4) <= '9' and Msg(5) >= '0' and Msg(5) <= '9' and Msg(6) >= '0' and Msg(6) <= '9' then
            I_Start := 7;
         end if;
         One;
      else
         --Gets the first blank space
         for I in I_Start .. 22 loop
            I_Start := I;
            if I_Start in Msg'Range then
               if Msg(I) = ' ' then
                  One;
                  exit;
               end if;
            end if;
         end loop;
      end if;
      if Skip_Ten = False then
         Ten;
      end if;
      if IType /= 6 then
         IType := Integer'Max(Nv2a,Nv2b);
      end if;
      --JT_IType := IType;
      --JT_C1 := c1(1..6);
      --JT_C2 := c2(1..6);
      --JT_C3 := c3;
      --JT_K1 := K1;
      --JT_K2 := K2;
      --JT_Nc1 := Nc1;
      --JT_Nc2 := Nc2;
      --JT_Ng := Ng;
      Dat( 0) := Unsigned_8(Shift_Right(Nc1, 22) and 63);
      Dat( 1) := Unsigned_8(Shift_Right(Nc1, 16) and 63);
      Dat( 2) := Unsigned_8(Shift_Right(Nc1, 10) and 63);
      Dat( 3) := Unsigned_8(Shift_Right(Nc1, 4) and 63);
      Dat( 4) := Unsigned_8(4 * (Nc1 and 15) + (Shift_Right(Nc2, 26) and 3));
      Dat( 5) := Unsigned_8(Shift_Right(Nc2, 20) and 63);
      Dat( 6) := Unsigned_8(Shift_Right(Nc2, 14) and 63);
      Dat( 7) := Unsigned_8(Shift_Right(Nc2, 8) and 63);
      Dat( 8) := Unsigned_8(Shift_Right(Nc2, 2) and 63);
      Dat( 9) := Unsigned_8(16 * (Nc2 and 3) + (Shift_Right(Ng, 12) and 15));
      Dat(10) := Unsigned_8(Shift_Right(Ng, 6) and 63);
      Dat(11) := Unsigned_8(Ng and 63);
   end Pack_Msg;


   -- Need to fix null character check
   procedure Unpack_Msg(Dat0 : Unsigned_8_Array; Msg : out String) is
      NBASE : constant Integer := 37*36*10*27*27*27;
      --NGBASE : Integer := 180*180;
      c1    : String(1 .. 12);
      c2    : String(1 .. 12);
      grid  : String(1 .. 4);
      junk2 : String(1 .. 4);
      psfx  : String(1 .. 4) := (others => ' ');
      grid6 : String(1 .. 6);
      cqnnn : Boolean := False;
     -- subtype Constrained_Unsigned_32 is Unsigned_32 range 0 .. 270000000;
      nc1   : Unsigned_32; -- Constrained_Unsigned_32;
      nc2   : Unsigned_32; -- Constrained_Unsigned_32;
      ng    : Unsigned_32; -- Constrained_Unsigned_32;
      iv2   : Integer;
      NFreq : Integer;
      junk1 : Integer;
      n1    : Integer;
      n2    : Integer;
      K     : Integer;
      J     : Integer;
      Dat   : Unsigned_32_Array (0 .. 11) := (others => 0);
      --Index_Val : Integer;

      function Prove_Valid_String (JT65_String : in String) return Boolean is
         Flag : Boolean := True;
      begin
         for I in JT65_String'Range loop
            if Flag then
               if JT65_String(I) in JT65_Character then
                  Flag := True;
               else
                  return False;
               end if;
            end if;
         end loop;
         return True;
      end Prove_Valid_String;

      procedure One_Hundred is
      begin
         if Msg(Msg'First .. Msg'First + 5) = "CQ9DX " then
            Msg(Msg'First + 2) := ' ';
         end if;
         if Msg(Msg'First .. Msg'First + 1) = "E9" and Msg(Msg'First + 2) >= 'A' and Msg(Msg'First + 2) <= 'Z' and Msg(Msg'First + 3) >= 'A' and Msg(Msg'First + 3) <= 'Z' and Msg(Msg'First + 4) = ' ' then
            --Msg := "CQ " & Msg(Msg'First + 2 .. Msg'Last);
            Move("CQ " & Msg(Msg'First + 2 .. Msg'Last), Msg, Right, Left, Space);
         end if;
         if Msg(Msg'First .. Msg'First + 4) = "CQ 00" and Msg(Msg'First + 5) >= '0' and Msg(Msg'First + 5) <= '9' then
            --Msg := "CQ " & Msg(Msg'First + 5 .. Msg'Last);
            Move("CQ " & Msg(Msg'First + 5 .. Msg'Last), Msg, Right, Left, Space);
         end if;
      end One_Hundred;

      procedure Twenty
        with
          Pre => Msg'First = 1 and Msg'Last = 22 and J > 0 and J <= 21
      is
         Flag : Boolean := False;
      begin
         if K = 0 then
            for I in 1 .. 4 loop
               if J <= 21 then
                  J := J + 1;
                  if I = 1 and grid(I) = '-' then
                     case (grid(I + 1)) is
                     when '0' .. '9' =>
                        case (grid(I + 2)) is
                        when ' ' =>
                           Flag := True;
                        when others =>
                           Flag := False;
                        end case;
                     when others =>
                        Flag := False;
                     end case;
                     if Flag and J <= 20 then
                        Msg(J) := grid(I);
                        Msg(J + 2) := grid(I+1);
                        exit;
                     end if;
                  end if;
                  Msg(J) := grid(I);
               end if;
            end loop;
            if J <= 21 then
               J := J + 1;
            end if;
            Msg(J) := ' ';
            if Flag then
               Msg(J) := '0';
            end if;
         end if;
         One_Hundred;
      end Twenty;

      procedure Ten
        with
          Pre => Msg'First = 1 and Msg'Last = 22 and J > 0 and J <= 21
      is
      begin
         for I in 1 .. 12 loop
            if J < 21 then
               J := J + 1;
            end if;
            Msg(J) := c2(I);
            if c2(I) = ' ' then
               Twenty;
               return;
            end if;
         end loop;
         if J < 21 then
            J := J + 1;
         end if;
         Msg(J) := ' ';
         Twenty;
      end Ten;

   begin -- Unpack_Msg
      for M in Dat0'Range loop
         Dat(M) := Unsigned_32(Dat0(M));
         pragma Loop_Invariant (Dat(M) <= 256);
      end loop;
      Msg := (others => ' ');
      nc1 := Shift_Left(Dat(0), 22) + Shift_Left(Dat(1), 16) + Shift_Left(Dat(2), 10) +
        Shift_Left(Dat(3), 4) + (Shift_Right(Dat(4), 2) and 15); -- Converted from Unsigned_8 to Unsigned_32. How is SPARK seeing a possible range check fail?

      nc2 := Shift_Left((Dat(4) and 3), 26) + Shift_Left(Dat(5), 20) + Shift_Left(Dat(6), 14) +
        Shift_Left(Dat(7), 8) + Shift_Left(Dat(8), 2) + (Shift_Right(Dat(9), 4) and 3); -- Converted from Unsigned_8 to Unsigned_32. How is SPARK seeing a possible range check fail?

      ng := Shift_Left((Dat(9) and 15), 12) + Shift_Left(Dat(10), 6) + Dat(11); -- Converted from Unsigned_8 to Unsigned_32. How is SPARK seeing a possible range check fail?
      if ng >= 32768 then
         Unpack_Text(nc1, nc2, ng, Msg);
         One_Hundred;
         return;
      end if;
      if Prove_Valid_String(psfx) = True and Nc1 <= 2147483647 and Nc2 <= 2147483647 then
         Unpack_Call(nc1, c1, iv2, psfx);

      if iv2 = 0 then
         if Integer(nc1) = NBASE + 1 then
            --c1 := "CQ    ";
            Move("CQ    ", c1, Right, Left, Space);
         end if;
         if Integer(nc1) = NBASE + 2 then
            Move("QRZ   ", c1, Right, Left, Space);
         end if;
         NFreq := Integer(nc1) - NBASE - 3;
         -- ERROR RIGHT HERE
         if NFreq >= 0 and NFreq <= 999 then
            --write(c1,1002) nfreq
            --format('CQ ',i3.3)
            --c1 := "CQ " & Integer'Image(NFreq);
            -- There were 2 leading zeros. This broken, although original program cannot handle two leading zeros.
            if nc1 >= 262177564 and nc1 <= 262177572 then
               Move("CQ 00" & Trim(Integer'Image(NFreq), Both), c1, Right, Left, Space);
               -- There was 1 leading zero
            elsif nc1 >= 262177573 and nc1 <= 262177662 then
               Move("CQ 0" & Trim(Integer'Image(NFreq), Both), c1, Right, Left, Space);
            -- No leading zero
            else
               Move("CQ " & Trim(Integer'Image(NFreq), Both), c1, Right, Left, Space);
            end if;
            cqnnn := True;
         end if;
      end if;

      Unpack_Call(nc2, c2, junk1, junk2);
      Unpack_Grid(Integer(ng), grid);

      if iv2 > 0 then
         for I in 1 .. 4 loop
            if Character'Pos(psfx(I)) = 0 then
               psfx(I) := ' ';
            end if;
         end loop;
         n1 := Trim(psfx, Right)'Length;
         n2 := Trim(c2, Right)'Length;
         if iv2 = 1 then
            --Msg := "CQ " & psfx(psfx'First .. n1) & '/' & c2(c2'First .. n2) & ' ' & grid;
            Move("CQ " & psfx(psfx'First .. n1) & '/' & c2(c2'First .. n2) & ' ' & grid, Msg, Right, Left, Space);
         end if;
         if iv2 = 2 then
            --Msg := "QRZ " & psfx(psfx'First .. n1) & '/' & c2(c2'First .. n2) & ' ' & grid;
            Move("QRZ " & psfx(psfx'First .. n1) & '/' & c2(c2'First .. n2) & ' ' & grid, Msg, Right, Left, Space);
         end if;
         if iv2 = 3 then
            --Msg := "DE " & psfx(psfx'First .. n1) & '/' & c2(c2'First .. n2) & ' ' & grid;
            Move("DE " & psfx(psfx'First .. n1) & '/' & c2(c2'First .. n2) & ' ' & grid, Msg, Right, Left, Space);
         end if;
         if iv2 = 4 then
            --Msg := "CQ " & c2(c2'First .. n2) & '/' & psfx(psfx'First .. n1) & ' ' & grid;
            Move("CQ " & c2(c2'First .. n2) & '/' & psfx(psfx'First .. n1) & ' ' & grid, Msg, Right, Left, Space);
         end if;
         if iv2 = 5 then
            --Msg := "QRZ " & c2(c2'First .. n2) & '/' & psfx(psfx'First .. n1) & ' ' & grid;
            Move("QRZ " & c2(c2'First .. n2) & '/' & psfx(psfx'First .. n1) & ' ' & grid, Msg, Right, Left, Space);
         end if;
         if iv2 = 6 then
            --Msg := "DE " & c2(c2'First .. n2) & '/' & psfx(psfx'First .. n1) & ' ' & grid;
            Move("DE " & c2(c2'First .. n2) & '/' & psfx(psfx'First .. n1) & ' ' & grid, Msg, Right, Left, Space);
         end if;
         if iv2 = 7 then
            grid6 := grid & "ma";
            Grid2k(grid6, K);
            if K >= 451 and K <= 900 then
               Get_Pfx2(K, c2);
               n2 := Trim(c2, Right)'Length;
               --Msg := "DE " & c2(c2'First .. n2);
               Move("DE " & c2(c2'First .. n2), Msg, Right, Left, Space);
            else
               --Msg := "DE " & c2(c2'First .. n2) & ' ' & grid;
               Move("DE " & c2(c2'First .. n2) & ' ' & grid, Msg, Right, Left, Space);
            end if;
         end if;
         if iv2 = 8 then
            Msg := "                      ";
         end if;
         One_Hundred;
         return;
         end if;

         --grid6(Grid6'First .. Grid6'Last) := grid(1 .. 4) & "ma";
         Move(Grid(1 .. 4) & "ma", Grid6, Right, Left, Space);

         Grid2k(grid6, K);
         if K >= 1 and K <= 450 then
            Get_Pfx2(K, c1);
         end if;
         if K >= 451 and K <= 900 then
            Get_Pfx2(K, c2);
         end if;
         Msg := "                      ";
         J := 0;
         if cqnnn then
            Msg := c1 & "          ";
            J := 7;
            Ten;
            return;
         end if;
         for I in 1 .. 12 loop
            J := J + 1;
            Msg(J) := c1(I);
            if c1(I) = ' ' then
               Ten;
               return;
            end if;
         end loop;
         J := J + 1;
         Msg(J) := ' ';
         Ten;
      end if;
   end Unpack_Msg;


   -- I did some limited testing with this and ended up getting the same result as its fortran equivalent
   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : out Unsigned_32) is
      C         : String(1 .. 42);
      Skip_Step : Boolean;
      J_Count   : Unsigned_32;
   begin
      C := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ +-./?";
      Nc1 := 0;
      Nc2 := 0;
      Nc3 := 0;

      for I in 1 .. 5 loop
         Skip_Step := False;
         J_Count := 0;
         for J in 1 .. 42 loop
            J_Count := J_Count + 1;
            if Msg(I) = C(J) then
               Skip_Step := True;
               exit;
            end if;
         end loop;
         if not Skip_Step then
            J_Count := 37; --was 36 for some reason
                           --Skip_Step := False;
         end if;
         J_Count := J_Count - 1;
         Nc1 := 42 * Nc1 + J_Count;
      end loop;
      for I in 6 .. 10 loop
         Skip_Step := False;
         J_Count := 0;
         for J in 1 .. 42 loop
            J_Count := J_Count + 1;
            if Msg(I) = C(J) then
               Skip_Step := True;
               exit;
            end if;
         end loop;
         if not Skip_Step then
            J_Count := 37;
            --Skip_Step := False;
         end if;
         J_Count := J_Count - 1;
         Nc2 := 42 * Nc2 + J_Count;
      end loop;
      for I in 11 .. 13 loop
         Skip_Step := False;
         J_Count := 0;
         for J in 1 .. 42 loop
            J_Count := J_Count + 1;
            if Msg(I) = C(J) then
               Skip_Step := True;
               exit;
            end if;
         end loop;
         if not Skip_Step then
            J_Count := 37;
            --Skip_Step := False;
         end if;
         J_Count := J_Count - 1;
         Nc3 := 42 * Nc3 + J_Count;
      end loop;
      Nc1 := Nc1 + Nc1;
      if (Nc3 and 32768) /= 0 then
         Nc1 := Nc1 + 1;
      end if;
      Nc2 := Nc2 + Nc2;
      if (Nc3 and 65536) /= 0 then
         Nc2 := Nc2 + 1;
      end if;
      Nc3 := Nc3 and 32767;
   end Pack_Text;


   --Parameters might need to be integers not unsigned
   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Unsigned_32; Msg : in out String) is
      Nc1, Nc2, Nc3 : Unsigned_32;
      J : Integer;
      C : constant String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ +-./?";
   begin
      Nc1 := Nc1a;
      Nc2 := Nc2a;
      Nc3 := Nc3a and 32767;

      if (Nc1 and 1) /= 0 then Nc3 := Nc3 + 32768; end if;
      Nc1 := Nc1 / 2;
      if (Nc2 and 1) /= 0 then Nc3 := Nc3 + 65536; end if;
      Nc2 := Nc2 / 2;
      for I in reverse 1 .. 5 loop
         pragma Loop_Invariant(Nc1 <= (2 **31 - 1));
         J := (Integer(Nc1) mod 42) + 1;
         Msg(I) := C(J);
         Nc1 := Nc1 / 42;
      end loop;
      for I in reverse 6 .. 10 loop
         pragma Loop_Invariant(Nc2 <= (2 **31 - 1));
         J := (Integer(Nc2) mod 42) + 1;
         Msg(I) := C(J);
         Nc2 := Nc2 / 42;
      end loop;
      for I in reverse 11 .. 13 loop
         pragma Loop_Invariant(Nc3 <= (2 **31 - 1));
         J := (Integer(Nc3) mod 42) + 1;
         Msg(I) := C(J);
         Nc3 := Nc3 / 42;
         end loop;
      Msg(Msg'First + 13 .. Msg'First + 21) := "         ";
   end Unpack_Text;

   ------------------------------------------
   -- Implementations of Internal Subprograms
   ------------------------------------------

   -- Something is broken in this.
   procedure Get_Pfx1(Callsign : in out String; K : out Integer; Nv2 : out Integer) is
      Suffixes   : sfx_array;
      Prefixes   : pfx_array;
      Callsign0  : String(1 .. 12);
      Temp       : String(1 .. 12) := (others => ' ');
      C          : String(1 ..  8);
      --Iz, Islash : Integer;
      subtype Slash_Loc is Integer range 0 .. 12;
      subtype Iz_Loc is Integer range -1 .. 12;
      Islash : Slash_Loc;
      Iz     : Iz_Loc;

      procedure Ten
        with
          Pre => Callsign0'First = 1 and Callsign0'Last = 12
      is
         llof, lrof, I : Integer;
         t_pfx : String(1 .. 4);
         t_sfx : String(1 .. 3);
         lof, rof : String(1 .. 12);
         --t_pfx : Callsign_String(1 .. 4);
         --t_sfx : Callsign_String(1 .. 3);
         --  lof, rof : Callsign_String(1 .. 12);
         is_pfx, is_sfx, Invalid : Boolean;
         --t_pfx_1, t_pfx_2, t_pfx_3, t_pfx_4 : Callsign_Type;

      begin
         if islash /= 0 and K = 0 then
            --lof := Callsign0(1 .. islash -1);
            Move(Callsign0(1 .. islash -1), lof, Right, Left, Space);
            --rof := Callsign0(islash +1 .. Callsign0'Last);
            Move(Callsign0(islash +1 .. Callsign0'Last), rof, Right, Left, Space);
            llof    := Trim(lof, Right)'Length;
            lrof    := Trim(rof, Right)'Length;
            is_pfx  := llof > 0 and llof <= 4;
            is_sfx  := lrof > 0 and lrof <= 3;
            Invalid := not (is_pfx or is_sfx);

            if is_pfx and is_sfx and islash >= 2 then
               if llof < 3 then is_sfx := False; end if;
               if lrof < 3 then is_pfx := False; end if;
               if is_pfx and is_sfx then
                  I := Character'Pos(Callsign0(islash - 1));
                  if i >= Character'Pos('0') and i <= Character'Pos('9') then
                     is_sfx := False;
                  else
                     is_pfx := False;
                  end if;
               end if;
            end if;
            if invalid then
               K := -1;
            else
               -- Predicate check might fail. Should think about moving to a "Callsign String"?
               if is_pfx then
                  t_pfx := lof(1 .. 4);
                  --pragma Assert(for all I in t_pfx'Range => t_pfx(I) in Callsign_Type);
                  K := NChar(t_pfx(1));
                  K := 37 * K + NChar(t_pfx(2));
                  K := 37 * K + NChar(t_pfx(3));
                  K := 37 * K + NChar(t_pfx(4));
                  Nv2 := 4;
                  I := Index(Callsign0, "/");
                  --Callsign := Callsign0(1 .. I-1);
                  --Move(Callsign0(1 .. I-1), Callsign, Right, Left, Space); Statement has no effect
                  --Callsign := Callsign0(I+1 .. Callsign0'Length);
                  if I >= 1 and I <= 12 then
                     Move(Callsign0(I+1 .. Callsign0'Length), Callsign, Right, Left, Space);
                  end if;
               end if;
               -- Same issue here. Predicate check might fail. Should think about moving to a "Callsign String"?
               if is_sfx then
                  t_sfx := rof(1 .. 3);
                  --pragma Assert(for all I in t_sfx'Range => t_sfx(I) in Callsign_Type);
                  K := NChar(t_sfx(1));
                  K := 37 * K + NChar(t_sfx(2));
                  K := 37 * K + NChar(t_sfx(3));
                  nv2 := 5;
                  I := Index(Callsign0, "/");
                  --Callsign := Callsign0(1 .. I-1);
                  if I >- 1 and I <= 12 then
                     Move(Callsign0(1 .. I-1), Callsign, Right, Left, Space);
                  end if;
               end if;
            end if;
         end if;
      end Ten;

      --Need to find a way to implement common/pfxcom/addpfx im not sure how it effects the behavior of the procedure.
   begin -- Get_Pfx1
      Init_Pfx(Prefixes, Suffixes);
      Callsign0 := Callsign;
      Nv2 := 1;
      iz := Get_Index(Callsign, " ") - 1;
      if iz < 0 then iz := 12; end if;
      islash := Get_Index(Callsign(Callsign'First .. iz), "/");
      K := 0;
      if islash > 0 and islash <= iz - 4 then
         --C := Callsign(1 .. islash - 1);
         Move(Callsign(Callsign'First .. islash - 1), C, Right, Left, Space);
         --Callsign := Callsign(islash + 1 .. iz);
         Temp(islash + 1 .. iz) := Callsign(islash + 1 .. iz);
         Collapse_Blanks_12(Temp);
         Callsign(Callsign'First .. Callsign'Last) := Temp(Temp'First .. Temp'Last);
         --Move(Callsign(islash + 1 .. iz), Callsign, Right, Left, Space);
         --Callsign(Callsign'First .. Callsign(islash + 1 .. iz)'Length) := Callsign(islash + 1 .. iz);
         for I in Prefixes'Range loop
            if Prefixes(I)(1 .. 4) = C(1 .. 4) then
               K := I;
               Nv2 := 2;
               Ten;
               return;
            end if;
         end loop;
         if add_pfx = C then
            K := 449;
            Nv2 := 2;
            Ten;
            return;
         end if;
      elsif islash = iz -1 then
         --C := Callsign(islash + 1 .. iz);
         Move(Callsign(islash + 1 .. iz), C, Right, Left, Space);
         -- Testing --
         -- Callsign := Callsign(1 .. islash - 1);
         Temp(Temp'First .. islash - 1) := Callsign(Callsign'First .. islash - 1);
         Collapse_Blanks_12(Temp);
         Callsign(Callsign'First .. Callsign'Last) := Temp(Temp'First .. Temp'Last);
         --Move(Callsign(Callsign'First .. islash - 1), Callsign, Right, Left, Space);
         for I in Suffixes'Range loop
            if Suffixes(I) = C(1) then
               K := 400 + I;
               Nv2 := 3;
               Ten;
               return;
            end if;
         end loop;
      end if;
      Ten;
   end Get_Pfx1;


   procedure Get_Pfx2(K0 : Integer; Callsign : in out String) is
      subtype Prefix_Size is Integer range -1 .. 12;
      K : Integer := K0;
      --Iz : Integer;
      Suffix : sfx_array;
      Prefix : pfx_array;
      Iz : Prefix_Size;
   begin
      Init_Pfx(Prefix, Suffix);
      if K > 450 then
         K := K - 450;
      end if;
      if K >= 1 and K <= Prefix'Length then
         Iz := Get_Index(Prefix(K), " ") - 1;
         --Callsign := Prefix(K)(1 .. Iz) & '/' & Callsign
         if Iz >= 1 then
            Move(Prefix(K)(1 .. Iz) & '/' & Callsign, Callsign, Right, Left, Space);
         end if;
      elsif K >= 401 and K <= (400 + Suffix'Length) then
         Iz := Get_Index(Callsign, " ") - 1;
         --Callsign := Callsign(1 .. Iz) & '/' & Suffix(K - 400);
         Move(Callsign(Callsign'First .. Iz) & '/' & Suffix(K - 400), Callsign, Right, Left, Space);
      elsif K = 449 then
         Iz := Get_Index(add_pfx, " ") - 1;
         if Iz < 1 then
            Iz := 8;
         end if;
         --Callsign := add_pfx(1 .. Iz) & '/' & Callsign;
         Move(add_pfx(1 .. Iz) & '/' & Callsign, Callsign, Right, Left, Space);
      end if;
   end Get_Pfx2;


   procedure Grid2K(Grid : String; K : out Integer) is
      subtype No_Overflow is Integer range Integer'First + 1 .. Integer'Last - 1;
      NLong, NLat : No_Overflow;
      XLong, XLat : Float;
   begin
      Grid2Deg(Grid, XLong, XLat);
      NLong := Integer(XLong);
      NLat := Integer(XLat);
      K := 0;
      if NLat >= 85 then
         K := 5 * (NLong + 179) / 2 + NLat - 84;
      end if;
   end Grid2k;


   procedure K2Grid(K : Integer; Grid : out String) is
      NLong, NLat : Integer;
      DLong, DLat : Float;
   begin
      NLong := 2 * (((K - 1) / 5) mod 90) - 179;
      if K > 450 then NLong := NLong + 180; end if;
      NLat := ((K - 1) mod 5) + 85;
      DLat := Float(NLat);
      DLong := Float(NLong);
      Deg2Grid(DLong, DLat, Grid);
   end K2Grid;

   --  Grid2N is currently not used
   --  procedure Grid2N(Grid : String; N : out Integer) is
   --     I1, I2, I : Integer;
   --  begin
   --     I1 := Character'Pos(Grid(Grid'First)) - Character'Pos('A');
   --     I2 := Character'Pos(Grid(Grid'First + 2)) - Character'Pos('0');
   --     I := 10 * I1 + I2;
   --     N := -I - 31;
   --  end Grid2N;

   --  N2Grid is currently not used
   --  procedure N2Grid(N : Integer; Grid : in out String) is
   --     I, I1, I2 : Integer;
   --  begin
   --     I := -(N + 31);
   --     I1 := I / 10;
   --     I2 := I mod 10;
   --
   --     Grid(Grid'First) := Character'Val(Character'Pos('A') + I1);
   --     Grid(Grid'First + 1) := 'A';
   --     Grid(Grid'First + 2) := Character'Val(Character'Pos('0') + I2);
   --     Grid(Grid'First + 3) := '0';
   --  end N2Grid;

   --Converts ascii number, letter, or space to 0-36
   function NChar(C : Character) return Numeric_Callsign_Type is
     (case (C) is
         when '0' .. '9' =>
            Character'Pos(C) - Character'Pos('0'),
         when 'A' .. 'Z' =>
            Character'Pos(C) - Character'Pos('A') + 10,
         when 'a' .. 'z' =>
            Character'Pos(C) - Character'Pos('a') + 10,
         when ' ' =>
            36,
         when others =>
            0);
            --when others => raise Program_Error);  -- Why is this necessary? Compiler bug?

   --  Pack50 is currently not used
   --  procedure Pack50(N1, N2 : Unsigned_32; Dat : out Unsigned_32_Array) is
   --  begin
   --     Dat := (others => 0);
   --     Dat(Dat'First +  0) := Shift_Right(N1, 20) and 255;
   --     Dat(Dat'First +  1) := Shift_Right(N1, 12) and 255;
   --     Dat(Dat'First +  2) := Shift_Right(N1, 4) and 255;
   --     Dat(Dat'First +  3) := 16 * (N1 and 15) + (Shift_Right(N2, 18) and 15);
   --     Dat(Dat'First +  4) := Shift_Right(N2, 10) and 255;
   --     Dat(Dat'First +  5) := Shift_Right(N2, 2) and 255;
   --     Dat(Dat'First +  6) := 64 * (N2 and 3);
   --     Dat(Dat'First +  7) := 0;
   --     Dat(Dat'First +  8) := 0;
   --     Dat(Dat'First +  9) := 0;
   --     Dat(Dat'First + 10) := 0;
   --  end Pack50;

   --  Pack_Pfx is currently not used
   --  procedure Pack_Pfx(Call1 : String; N1 : in out Unsigned_32; Ng : in out Integer; Nadd : out Integer) is
   --     Call0 : String (1 .. 12);
   --     Pfx : String (1 .. 3);
   --     Text : Boolean;
   --     N, Nc, I1 : Integer;
   --  begin
   --
   --     I1 := Index(Call1, "/");
   --
   --     if Call1(Call1'First + I1 + 1) = ' ' then
   --
   --        Call0 := Call1(Call1'First .. Call1'First + I1 - 2);
   --        Pack_Call(Call0, N1, Text);
   --        Nadd := 1;
   --        Nc := Character'Pos(Call1(Call1'First + I1));
   --
   --        if Nc >= 48 and Nc <= 57 then
   --           N := Nc - 48;
   --        elsif Nc >= 65 and Nc <= 90 then
   --           N := Nc - 65 + 10;
   --        else
   --           N := 38;
   --        end if;
   --
   --        Ng := 60000 - 32768 + N;
   --
   --     elsif Call1(Call1'First + I1 + 2) = ' ' then
   --
   --        Call0 := Call1(Call1'First .. Call1'First + I1 - 2);
   --        Pack_Call(Call0, N1, Text);
   --        Nadd := 1;
   --        N := 10 * (Character'Pos(Call1(Call1'First + I1)) - 48) + Character'Pos(Call1(Call1'First + I1 + 1)) - 48;
   --        Ng := 60000 + 26 + N;
   --
   --     else
   --
   --        Move(Call1(Call1'First .. Call1'First + I1 - 2), Pfx, Right, Left, Space);
   --        if Pfx(3) = ' ' then
   --           Pfx := ' ' & Pfx(1 .. 2);
   --        end if;
   --
   --        Call0 := Call1(Call1'First + I1 .. Call1'Last);
   --
   --        Pack_Call(Call0, N1, Text);
   --
   --        Ng := 0;
   --
   --        for I in 1 ..3 loop
   --
   --           Nc := Character'Pos(Pfx(I));
   --
   --           if Nc >= 48 and Nc <= 57 then
   --              N := Nc - 48;
   --           elsif Nc >= 65 and nc <= 90 then
   --              N := NC - 65 + 10;
   --           else
   --              N := 36;
   --           end if;
   --
   --           Ng := 37 * Ng + N;
   --
   --        end loop;
   --
   --        Nadd := 0;
   --
   --        if Ng >= 32768 then
   --           Ng := Ng - 32768;
   --           Nadd := 1;
   --        end if;
   --     end if;
   --
   --  end Pack_Pfx;

   procedure Grid2Deg(Grid0 : String; DLong : out Float; DLat : out Float) is
      Grid : String := Grid0;
      G1, G2, G3, G4, G5, G6 : Character;
      I, NLong, NLat, N20d : Integer;
      XMinLong, XMinLat : Float;
   begin
      --Grid := Grid0;
      I := Character'Pos(Grid(5));
      if Grid(5) = ' ' or I <= 64 or I >= 128 then
         Grid(5 .. 6) := "mm";
      end if;
      if Grid(1) >= 'a' and Grid(1) <= 'z' then
         Grid(1) := Character'Val(Character'Pos(Grid(1)) + Character'Pos('A') - Character'Pos('a'));
      end if;
      if Grid(2) >= 'a' and Grid(2) <= 'z' then
         Grid(2) := Character'Val(Character'Pos(Grid(2)) + Character'Pos('A') - Character'Pos('a'));
      end if;
      if Grid(5) >= 'A' and Grid(5) <= 'Z' then
         Grid(5) := Character'Val(Character'Pos(Grid(5)) - Character'Pos('A') + Character'Pos('a'));
      end if;
      if Grid(6) >= 'A' and Grid(6) <= 'Z' then
         Grid(6) := Character'Val(Character'Pos(Grid(6)) - Character'Pos('A') + Character'Pos('a'));
      end if;
      G1 := Grid(1);
      G2 := Grid(2);
      G3 := Grid(3);
      G4 := Grid(4);
      G5 := Grid(5);
      G6 := Grid(6);

      NLong := 180 - 20 * (Character'Pos(G1) - Character'Pos('A'));
      N20d := 2 * (Character'Pos(G3) - Character'Pos('0'));
      XMinLong := 5.0 * (Float(Character'Pos(G5)) - Float(Character'Pos('a')) + 0.5);
      DLong := Float(NLong) - Float(N20d) - XMinLong/60.0;
      NLat := -90 + 10 * (Character'Pos(G2) - Character'Pos('A')) + Character'Pos(G4) - Character'Pos('0');
      XMinLat := 2.5 * (Float(Character'Pos(G6)) - Float(Character'Pos('a')) + 0.5);
      DLat := Float(NLat) + XMinLat/60.0;
   end Grid2Deg;


   procedure Deg2Grid(DLong0 : Float; DLat : Float; Grid : out String) is
      --subtype Add_To_Pos is Integer range 0 .. 22;
      DLong : Float := DLong0;
      NLat, NLong : Integer;
      N1, N2, N3 : Integer;
   begin
      Grid := (others => ' ');
      if DLong < -180.0 then
         DLong := DLong + 360.0;
      end if;
      if DLong > 180.0 then
         DLong := DLong - 360.0;
      end if;
      if DLong >= -2000.0 and DLong <= 2000.0 and DLat >= -1000.0 and DLat <= 1000.0 then
      NLong := Integer(Float'Floor(60.0 * (180.0 - DLong) / 5.0));
      -- Need to check how these are rounded. Floating point numbers are always rounded down. Ex. 17.9 rounds down to 17.
      if NLong >= 0 and NLong <= 5280 then
         N1 := NLong / 240;
         N2 := (NLong - 240 * N1) / 24;
         --if N1 > 0 or N2 > 0 then
            N3 := NLong - 240 * N1 - 24 * N2;
            --Put("NLong = ");Put_Line(Integer'Image(NLong));
            --  Put("N1 = "); Put_Line(Integer'Image(N1));
           -- Put("N2 = "); Put_Line(Integer'Image(N2));
        -- Put("N3 = "); Put_Line(Integer'Image(N3));
         if (N1 >= 0 and N1 <= 22) and (N2 >= 0 and N2 <= 22) and (N1 >= 0 and N1 <= 22) then
            Grid(Grid'First + 0) := Character'Val(Character'Pos('A') + N1);
            Grid(Grid'First + 2) := Character'Val(Character'Pos('0') + N2);
            Grid(Grid'First + 4) := Character'Val(Character'Pos('a') + N3);
         end if;
      end if;
      if DLat > 0.0 then
         NLat := Integer(Float'Floor(60.0 * (DLat + 90.0) / 2.5));
         if NLat >= 0 and NLat <= 5280 then
            N1 := NLat / 240;
            N2 := (NLat - 240 * N1) / 24;
            --if N1 /= 0 or N2 /= 0 then
               N3 := NLat - 240 * N1 - 24 * N2;
               --Put("N1 (2) = ");Put_Line(Integer'Image(N1));
               -- Put("N2 (2) = ");Put_Line(Integer'Image(N2));
               -- Put("N3 (2) = ");Put_Line(Integer'Image(N3));
         if (N1 >= 0 and N1 <= 22) and (N2 >= 0 and N2 <= 22) and (N1 >= 0 and N1 <= 22) then
               Grid(Grid'First + 1) := Character'Val(Character'Pos('A') + N1);
               Grid(Grid'First + 3) := Character'Val(Character'Pos('0') + N2);
               Grid(Grid'First + 5) := Character'Val(Character'Pos('a') + N3);
            end if;
         end if;
         end if;
      end if;
   end Deg2Grid;


   procedure Collapse_Blanks_12(Word : in out String) is
      I : Integer := 1;
      Flag : Boolean := False;
      Counter : Integer;
   begin
      while I <= 12 and I + 1 /= Word'Length loop
         if I >= 1 and I /= Word'Last then
            if (Word(I) = ' ' and Word(I + 1) = ' ') or (I = 1 and Word(I) = ' ') then
               Word(Word'First .. Word'Last) :=
                 Word(Word'First .. I - 1) & Word(I + 1 .. Word'Last) & Word(I);
               Counter := Word'Last - I;
               for X in I .. Word'Last loop
                  if Word(X) = ' ' and Counter >= 0 then
                     Counter := Counter - 1;
                  end if;
                  if Counter = 0 then
                     Flag := True;
                     exit;
                  end if;
               end loop;
               if Flag then
                  exit;
               end if;
            else
               I := I + 1;
            end if;
         end if;
      end loop;
   end Collapse_Blanks_12;


   function Get_Index(JT65_String : in String; Pattern : in String) return Valid_Index is
      Index : Valid_Index := 1;
   begin
      for I in JT65_String'Range loop
         if JT65_String(I .. I) = Pattern then
            return Index;
         else
            if Index = JT65_String'Length then
               return 0;
            else
               if Index < Valid_Index'Last then
                  Index := Index + 1;
               end if;
            end if;
         end if;
      end loop;
      return 0;
   end Get_Index;

end Pack_JT;

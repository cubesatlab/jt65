pragma SPARK_Mode(On);

package body Pack_JT is

   --JT_IType, JT_Nc1, JT_Nc2, JT_Ng, JT_K1, JT_K2 : Integer;
   --JT_C1, JT_C2, JT_C3 : String(1 .. 6);
   add_pfx : String(1 .. 8);


   procedure Pack_Bits(DBits : in out Octet_Array; NSymd : in out Integer; M0 : in out Integer; Sym : in out Integer_Array) is
      -- Might need to change the types for n and m
      k : Integer := 0;
      n : Octet := 0;
      m : Octet;
   begin
      -- these loops might need to be shifted back by one because of how arrays work in Fortran
      for i in 0 .. NSymd-1 loop
         n := 0;
         for j in 0 .. M0-1 loop
            m := DBits(k);
            k := k + 1;
            n := Shift_Left(n,1) or m;
         end loop;
         Sym(i) := Integer(n);
      end loop;

      raise Not_Implemented with "Pack_Bits";
   end Pack_Bits;


   procedure Unpack_Bits(Sym : Integer_Array; NSymd : Integer; M0 : Integer; DBits : Octet_Array) is
   begin
      raise Not_Implemented with "Unpack_Bits";
   end Unpack_Bits;

   --Packs a valid callsign into a 28-bit integer
   procedure Pack_Call(Call : in out String; NCall : in out Unsigned_32; Text : out Boolean) is

      NBASE : constant Unsigned_32 := 37*36*10*27*27*27;
      C : Character;
      TMP : String(1 .. 6);
      N1, N2, N3, N4, N5 , N6, NFreq : Integer;

   begin

      Text := False;

      if Call(Call'First .. Call'First + 3) = "3DA0" then Call := "3D0" & Call(Call'First + 4 .. Call'First + 5); end if;

      if Call(Call'First .. Call'First + 1) = "3X" and Call(Call'First + 2) >= 'A' and Call(Call'First + 2) <= 'Z' then Call := 'Q' & Call(Call'First + 2 .. Call'First + 5); end if;

      if Call(Call'First .. Call'First + 2) = "CQ " then

         NCall := NBASE + 1;

         if Call(Call'First + 3) >= '0' and Call(Call'First + 3) <= '9' and Call(Call'First + 4) >= '0' and Call(Call'First + 4) <= '9' and Call(Call'First + 5) >= '0' and Call(Call'First + 5) <= '9' then
            NFreq := Integer'Value(Call(Call'First + 3 .. Call'First + 5));
            NCall := NBASE + 3 + Unsigned_32(NFreq);
         end if;
         return;
      elsif Call(Call'First .. Call'First + 3) = "QRZ " then
         NCall := NBASE + 2;
         return;
      elsif Call(Call'First .. Call'First + 2) = "DE " then
         NCall := 267796945;
         return;
      end if;

      TMP := "      ";

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


   procedure Unpack_Call(NCall : Unsigned_32; Word : out String; Iv2 : out Integer; Psfx : out String) is
      NBASE : constant Integer := 37 * 36 * 10 * 27 * 27 * 27;
      C : constant String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
      N : Integer := Integer(NCall);
      I : Integer := 0;

      procedure Nine_Nine_Nine is
      begin
         if Word(Word'First .. Word'First + 2) = "3D0" then
            Word := "3DA0" & Word(Word'First + 3 .. Word'Last);
         end if;
         if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
            Word := "3X" & Word(Word'First + 1 .. Word'Last);
         end if;
      end Nine_Nine_Nine;

      procedure Twenty is
      begin
         if N >= 267796946 then
            Nine_Nine_Nine;
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
            Psfx := "    ";

         end if;
         Nine_Nine_Nine;
      end Twenty;

      begin
      --Word := "......";
      Move("......", Word, Right, Left, Space);
      --Psfx := "    ";
      Move("    ", Psfx, Right, Left, Space);
      Iv2 := 0;

      if N >= 262177560 then
         Twenty;
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
         if Word(X) /= ' ' then
            --Word := Word(X .. Word'Last);
            Move(Word(X .. Word'Last), Word, Right, Left, Space);
            Nine_Nine_Nine;
            return;
         end if;
      end loop;

      Nine_Nine_Nine;


--        <<Twenty>>
--        if N >= 267796946 then goto NineNineNine; end if;
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
--
--        <<NineNineNine>>
--        if Word(Word'First .. Word'First + 2) = "3D0" then
--           Word := "3DA0" & Word(Word'First + 3 .. Word'Last);
--        end if;
--        if Word(Word'First) = 'Q' and Word(Word'First + 1) >= 'A' and Word(Word'First + 1) <= 'Z' then
--           Word := "3X" & Word(Word'First + 1 .. Word'Last);
--        end if;

   end Unpack_Call;

   procedure Pack_Grid(Grid : in out String; NG : in out Unsigned_32; Text : in out Boolean) is
      NGBASE : constant Unsigned_32 := 180 * 180;
      C1 : Character;
      N, Long, Lat : Integer := 0;
      DLong, DLat : Float := 0.0;

      procedure Eight_Hundred is
      begin
         Text := True;
      end Eight_Hundred;

      procedure Ninety is
      begin
         NG := NGBASE + 1;
         return;
      end Ninety;

      procedure Forty is
      begin
         Grid2Deg(Grid & "mm", DLong, DLat);
         Long := Integer(Float'Floor(DLong));
         Lat := Integer(Float'Floor(DLat + 90.0));
         NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
         return;
      end Forty;

      procedure Thirty is
      begin
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
            Forty;
            return;
         end if;

         if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
         if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
         if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
         if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
         if Text then return; end if;

         Forty;
      end Thirty;

      procedure Twenty is
      begin

         for I in Grid(Grid'First + 1 .. Grid'First + 3)'range loop
            if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
               Thirty;
               return;
            end if;
         end loop;

         N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 3));
--
--           begin
--              N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 3));
--           exception
--              when E : Constraint_Error =>
--                 Thirty;
--                 return;
--           end;

         Thirty;
      end Twenty;

      procedure Ten is
      begin
         N := 99;
         C1 := Grid(Grid'First);

         for I in Grid'range loop
            if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
               Twenty;
               return;
            end if;
         end loop;

         N := Integer'Value(Grid);

--           begin
--              N := Integer'Value(Grid);
--           exception
--              when E : Constraint_Error =>
--                 Twenty;
--                 return;
--           end;

      Thirty;
      end Ten;

   begin
      Text := False;
      if Grid = "    " then
         Ninety;
         return;
      end if;

      if Grid(Grid'First) = '-' then

         for I in Grid(Grid'First + 1 .. Grid'First + 2)'range loop
            if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
               Eight_Hundred;
               return;
            end if;
         end loop;

         N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 2));

--           begin
--              N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 2));
--           exception
--              when E : Constraint_Error =>
--                 Eight_Hundred;
--                 return;
--           end;

         if N >= 1 and N <= 30 then
            NG := NGBASE + 1 + Unsigned_32(N);
            return;
         end if;

         Ten;
         return;

      elsif Grid(Grid'First .. Grid'First + 1) = "R-" then

         for I in Grid(Grid'First + 2 .. Grid'First + 3)'range loop
            if (Grid(I) < '0' or Grid(I) > '9') and Grid(I) /= ' ' then
               Eight_Hundred;
               return;
            end if;
         end loop;

         N := Integer'Value(Grid(Grid'First + 2 .. Grid'First + 3));

--           begin
--              N := Integer'Value(Grid(Grid'First + 2 .. Grid'First + 3));
--           exception
--              when E : Constraint_Error =>
--                 Eight_Hundred;
--                 return;
--           end;

         if N >= 1 and N <= 30 then
            NG := NGBASE + 31 + Unsigned_32(N);
            return;
         end if;
         Ten;
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

      Ten;

--        <<Ten>>
--        N := 99;
--        C1 := Grid(Grid'First);
--        begin
--           N := Integer'Value(Grid);
--        exception
--           when E : Constraint_Error =>
--              goto Twenty;
--        end;
--
--        goto Thirty;

--        <<Twenty>>
--        begin
--           N := Integer'Value(Grid(Grid'First + 1 .. Grid'First + 3));
--        exception
--           when E : Constraint_Error =>
--              goto Thirty;
--        end;

--        <<Thirty>>
--        if N >= -50 and N <= 49 then
--           if C1 = 'R' then
--              --write(grid,1002) n+50
--              --format('LA',i2.2)
--              Grid := "LA" & Integer'Image(N + 50);
--           else
--              --write(grid,1003) n+50
--              --format('KA',i2.2)
--              Grid := "KA" & Integer'Image(N + 50);
--           end if;
--           goto Forty;
--        end if;
--
--        if Grid(Grid'First) < 'A' or Grid(Grid'First) > 'R' then Text := True; end if;
--        if Grid(Grid'First + 1) < 'A' or Grid(Grid'First + 1) > 'R' then Text := True; end if;
--        if Grid(Grid'First + 2) < '0' or Grid(Grid'First + 2) > '9' then Text := True; end if;
--        if Grid(Grid'First + 3) < '0' or Grid(Grid'First + 3) > '9' then Text := True; end if;
--        if Text then goto Nine_Hundred; end if;

--        <<Forty>>
--        Grid2Deg(Grid & "mm", DLong, DLat);
--        Long := Integer(Float'Floor(DLong));
--        Lat := Integer(Float'Floor(DLat + 90.0));
--        NG := Unsigned_32(((Long + 180) / 2) * 180 + Lat); --This might need to be signed
--        goto Nine_Hundred;

--        <<Ninety>>
--        NG := NGBASE + 1;
--        goto Nine_Hundred;

--        <<Eight_Hundred>>
--        Text := True;
--
--        <<Nine_Hundred>>
--        return;

   end Pack_Grid;

   procedure Unpack_Grid(Ng : Integer; Grid : out String) is
      NGBASE : constant Integer := 180 * 180;
      Grid6 : String(1 .. 6);
      DLat, DLong : Float;
      N : Integer;

      procedure Ten is
      begin
         N := Ng - NGBASE - 1;

         if N >= 1 and N <= 30 then
            --Grid := Integer'Image(-N);
            Move(Integer'Image(-N), Grid, Right, Left, Space);
         elsif N >= 31 and N <= 60 then
            N := N - 30;
            --Grid := "R" & Integer'Image(-N);
            Move("R" & Integer'Image(-N), Grid, Right, Left, Space);
         elsif N = 61 then
            Grid := "RO  ";
         elsif N = 62 then
            Grid := "RRR ";
         elsif N = 63 then
            Grid := "73  ";
         end if;

      end Ten;


   begin
      Grid := "    ";

      if Ng >= 32400 then
         Ten;
         return;
      end if;

      DLat := Float((Ng mod 180) - 90);
      DLong := Float((Ng / 180) * 2 - 180 + 2);
      Deg2Grid(DLong, DLat, Grid6);
      Grid := Grid6(Grid6'First .. Grid6'First + 3);

      if Grid(Grid'First .. Grid'First + 1) = "KA" then
         N := Integer'Value(Grid(Grid'First + 2 .. Grid'First + 3));
         N := N - 50;
         Grid := Trim(Integer'Image(N), Both);
         if Grid(Grid'First) = ' ' then Grid(Grid'First) := '+'; end if;
      elsif Grid(Grid'First + 2 .. Grid'First + 3) = "LA" then
         N := Integer'Value(Grid(Grid'First + 2 .. Grid'First + 3));
         N := N - 50;
         Grid := "R" & Trim(Integer'Image(N), Both);
         if Grid(Grid'First + 1) = ' ' then Grid(Grid'First + 1) := '+'; end if;
      end if;

      return;

--        <<Ten>>
--        N := Ng - NGBASE - 1;
--
--        if N >= 1 and N <= 30 then
--           --Grid := Integer'Image(-N);
--           Move(Integer'Image(-N), Grid, Right, Left, Space);
--        elsif N >= 31 and N <= 60 then
--           N := N - 30;
--           --Grid := "R" & Integer'Image(-N);
--           Move("R" & Integer'Image(-N), Grid, Right, Left, Space);
--        elsif N = 61 then
--           Grid := "RO  ";
--        elsif N = 62 then
--           Grid := "RRR ";
--        elsif N = 63 then
--           Grid := "73  ";
--        end if;

   end Unpack_Grid;


   procedure Pack_Msg(Msg0 : String; Dat : out Integer_Array; IType : out Integer) is
      Msg : String(1 .. 22);
      --NBASE : constant Integer := 37*36*10*27*27*27;
      --NBASE2 : constant Integer := 262178562;
      C1,C2 : String(1..12);
      C3 : String(1..4);
      Grid6 : String(1..6);
      Text1, Text2, Text3 : Boolean := True;
      I_Start : Integer := 1;
      ia , ib, ic : Integer;
      Nv2a, Nv2b : Integer := 0;
      Nc1, Nc2, Ng : Unsigned_32 := 0;
      Skip_Ten : Boolean := False;

     procedure Three is
        K, K1, K2 : Integer;
      begin
         ic := I_Start;
         C3 := "    ";
         if ic >= ib + 1 then
            --C3 := Msg(ib+1 ..ic);
            Move(Msg(ib+1 ..ic), C3, Right, Left, Space);

         end if;

         if C3 = "OOO " then
            C3 := "    ";
         end if;

         Get_Pfx1(C1, K1, Nv2a);


         if Nv2a >= 4 then
            --Ten;
            return;
         end if;

         Pack_Call(C1, Nc1, Text1);


         if Text1 then
            --Ten;
            return;
         end if;

         Get_Pfx1(C2, K2, Nv2b);


         Pack_Call(C2, Nc2, text2);

         if Text2 then
            --Ten;
            return;
         end if;

         if Nv2a = 2 or Nv2a = 3 or Nv2b = 2 or Nv2b = 3 then
            if K1 < 0 or K2 < 0 or K1*K2 /= 0 then
               --Ten;
               return;
            end if;

            if K2 > 0 then
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

         if Nv2b = 4 then
            if C1(1 .. 3) = "CQ " and not Text3 then
               Nc1 := 262178563 + Unsigned_32(K2);
            end if;

            if C1(1 .. 4) = "QRZ " and not Text3 then
               Nc1 := 264002072 + Unsigned_32(K2);
            end if;

            if C1(1 ..3) = "DE " and not Text3 then
               Nc1 := 265825581 + Unsigned_32(K2);
            end if;

         elsif Nv2b = 5 then
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

      --Gets the second blank space
      procedure One is
      begin
         ia := I_Start;
         --C1 := Msg(1 .. ia-1);
         Move(Msg(1 .. ia-1), C1, Right, Left, Space);

         I_Start := ia + 1;
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
         ng := ng + 32768;
      end Ten;

   begin
      IType := 1;
      --Msg := Msg0;
      Move(Msg0, Msg, Right, Left, Space);

      --Fmtmsg(Msg); --This Needs to be fixed for now I am just using To_Upper
      Msg := To_Upper(Msg); --This was causing trouble


      if Msg(1..3) = "CQ " and Msg(4) >= '0' and Msg(4) <= '9' and Msg(5) = ' ' then
         Msg := "CQ 00" & Msg(4..Msg'Last - 2); --The Last character of the Msg is cut off, I dont think it matters but it might.
      end if;

      if Msg(1..6) = "CQ DX " then
         Msg(3) := '9';
      end if;

      if Msg(1..3) = "CQ " and msg(4) >= 'A' and Msg(4) <= 'Z' and Msg(5) >= 'A' and Msg(5) <= 'Z' and Msg(6) = ' ' then
         --Msg := "E9" & Msg(4..Msg'Last);
         Move("E9" & Msg(4..Msg'Last), Msg, Right, Left, Space);
      end if;

      --Check if it"s a CQ message
      if Msg(1..3) = "CQ " then
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
      Dat(1) := Integer(Shift_Right(Nc1, 22) and 63);
      Dat(2) := Integer(Shift_Right(Nc1, 16) and 63);
      Dat(3) := Integer(Shift_Right(Nc1, 10) and 63);
      Dat(4) := Integer(Shift_Right(Nc1, 4) and 63);
      Dat(5) := Integer(4 * (Nc1 and 15) + (Shift_Right(Nc2, 26) and 3));
      Dat(6) := Integer(Shift_Right(Nc2, 20) and 63);
      Dat(7) := Integer(Shift_Right(Nc2, 14) and 63);
      Dat(8) := Integer(Shift_Right(Nc2, 8) and 63);
      Dat(9) := Integer(Shift_Right(Nc2, 2) and 63);
      Dat(10) := Integer(16 * (Nc2 and 3) + (Shift_Right(Ng, 12) and 15));
      Dat(11) := Integer(Shift_Right(Ng, 6) and 63);
      Dat(12) := Integer(Ng and 63);

   end Pack_Msg;

   -- Need to fix null character check
   procedure Unpack_Msg(Dat : Integer_Array; Msg : out String) is
      NBASE : Integer := 37*36*10*27*27*27;
      NGBASE : Integer := 180*180;
      c1, c2 : String(1 .. 12);
      grid, psfx , junk2 : String(1..4);
      grid6 : String(1..6);
      cqnnn : Boolean := False;
      nc1, nc2, ng : Unsigned_32;
      iv2, NFreq, junk1, n1, n2, K, J : Integer;
      --Index_Val : Integer;

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

      procedure Twenty is
      begin
         if K = 0 then
            for I in 1 .. 4 loop
               if J <= 21 then J := J + 1; end if;
               Msg(J) := grid(I);
            end loop;

            if J <= 21 then J := J + 1; end if;
            Msg(J) := ' ';
         end if;
         One_Hundred;
      end Twenty;

      procedure Ten is
      begin
         for I in 1 .. 12 loop
            if J <= 21 then J := J + 1; end if;
            Msg(J) := c2(I);
            if c2(I) = ' ' then
               Twenty;
               return;
            end if;
         end loop;

         if J <= 21 then J := J + 1; end if;
         Msg(J) := ' ';
         Twenty;
      end Ten;

   begin
      nc1 := Shift_Left(Unsigned_32(Dat(1)), 22) + Shift_Left(Unsigned_32(Dat(2)), 16) + Shift_Left(Unsigned_32(Dat(3)), 10) +
        Shift_Left(Unsigned_32(Dat(4)), 4) + (Shift_Right(Unsigned_32(Dat(5)), 2) and 15);

      nc2 := Shift_Left((Unsigned_32(Dat(5)) and 3), 26) + Shift_Left(Unsigned_32(Dat(6)), 20) + Shift_Left(Unsigned_32(Dat(7)), 14) +
        Shift_Left(Unsigned_32(Dat(8)), 8) + Shift_Left(Unsigned_32(Dat(9)), 2) + (Shift_Right(Unsigned_32(Dat(10)), 4) and 3);

      ng := Shift_Left((Unsigned_32(Dat(10)) and 15), 12) + Shift_Left(Unsigned_32(Dat(11)), 6) + Unsigned_32(Dat(12));

      if ng >= 32768 then
         Unpack_Text(nc1, nc2, ng, Msg);
         One_Hundred;
         return;
      end if;

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

         if NFreq >= 0 and NFreq <= 999 then
            --write(c1,1002) nfreq
            --format('CQ ',i3.3)
            --c1 := "CQ " & Integer'Image(NFreq);
            Move("CQ " & Trim(Integer'Image(NFreq), Both), c1, Right, Left, Space);
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
            Msg := " ";
         end if;

         One_Hundred;
         return;
      end if;

      grid6 := grid & "ma";

      Grid2k(grid6, K);

      if K >= 1 and K <= 450 then
         Get_Pfx2(K, c1);
      end if;

      if K >= 451 and K <= 900 then
         Get_Pfx2(K, c2);
      end if;

--        Index_Val := Index(c1, " "); -- Should be looking for null characters not blank space
--
--        if Index_Val >= 3 then
--           --c1 := c1(1 .. Index_Val - 1) & "            ";
--           Move(c1(1 .. Index_Val - 1) & "            ", c1, Right, Left, Space);
--        end if;
--
--        Index_Val := Index(c2, " "); -- Should be looking for null characters not blank space
--
--        if Index_Val >= 3 then
--           --c2 := c2(1 .. Index_Val - 1) & "            ";
--           Move(c2(1 .. Index_Val - 1) & "            ", c2, Right, Left, Space);
--        end if;

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
--        <<Ten>>
--        for I in 1 .. 12 loop
--           if J <= 21 then J := J + 1; end if;
--           Msg(J) := c2(I);
--           if c2(I) = ' ' then goto Twenty; end if;
--        end loop;
--
--        if J <= 21 then J := J + 1; end if;
--        Msg(J) := ' ';

--        <<Twenty>>
--        if K = 0 then
--           for I in 1 .. 4 loop
--              if J <= 21 then J := J + 1; end if;
--              Msg(J) := grid(I);
--           end loop;
--
--           if J <= 21 then J := J + 1; end if;
--           Msg(J) := ' ';
--        end if;


--        <<One_Hundred>>
--        if Msg(Msg'First .. Msg'First + 5) = "CQ9DX " then
--           Msg(Msg'First + 2) := ' ';
--        end if;
--
--        if Msg(Msg'First .. Msg'First + 1) = "E9" and Msg(Msg'First + 2) >= 'A' and Msg(Msg'First + 2) <= 'Z' and Msg(Msg'First + 3) >= 'A' and Msg(Msg'First + 3) <= 'Z' and Msg(Msg'First + 4) = ' ' then
--
--           --Msg := "CQ " & Msg(Msg'First + 2 .. Msg'Last);
--           Move("CQ " & Msg(Msg'First + 2 .. Msg'Last), Msg, Right, Left, Space);
--        end if;
--
--        if Msg(Msg'First .. Msg'First + 4) = "CQ 00" and Msg(Msg'First + 5) >= '0' and Msg(Msg'First + 5) <= '9' then
--           --Msg := "CQ " & Msg(Msg'First + 5 .. Msg'Last);
--           Move("CQ " & Msg(Msg'First + 5 .. Msg'Last), Msg, Right, Left, Space);
--        end if;


   end Unpack_Msg;

   -- I did some limited testing with this and ended up getting the same result as its fortran equivalent
   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : out Unsigned_32) is
      C : String(1 .. 42);
      Skip_Step : Boolean;
      J_Count : Unsigned_32;
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
            Skip_Step := False;
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
            Skip_Step := False;
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
            Skip_Step := False;
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
      C : String := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ +-./?";
   begin
      Nc1 := Nc1a;
      Nc2 := Nc2a;
      Nc3 := Nc3a and 32767;

      if (Nc1 and 1) /= 0 then Nc3 := Nc3 + 32768; end if;

      Nc1 := Nc1 / 2;

      if (Nc2 and 1) /= 0 then Nc3 := Nc3 + 65536; end if;

      Nc2 := Nc2 / 2;

      for I in reverse 1 .. 5 loop
         J := (Integer(Nc1) mod 42) + 1;
         Msg(I) := C(J);
         Nc1 := Nc1 / 42;
      end loop;

      for I in reverse 6 .. 10 loop
         J := (Integer(Nc2) mod 42) + 1;
         Msg(I) := C(J);
         Nc2 := Nc2 / 42;
      end loop;

      for I in reverse 11 .. 13 loop
         J := (Integer(Nc3) mod 42) + 1;
         Msg(I) := C(J);
         Nc3 := Nc3 / 42;
      end loop;

      Msg(Msg'First + 13 .. Msg'First + 21) := "         ";

   end Unpack_Text;

   --Something is broken in this
   procedure Get_Pfx1(Callsign : in out String; K : out Integer; Nv2 : in out Integer) is
      Suffixes : sfx_array;
      Prefixes : pfx_array;
      Callsign0: String (1..12);
      C : String (1..8);
      iz, islash : Integer;

      procedure Ten is
         llof, lrof, I : Integer;
         t_pfx : String (1 .. 4);
         t_sfx : String (1 .. 3);
         lof, rof : String (1 .. 12);
         is_pfx, is_sfx, invalid : Boolean;
      begin
         if islash /= 0 and K = 0 then
            --lof := Callsign0(1 .. islash -1);
            Move(Callsign0(1 .. islash -1), lof, Right, Left, Space);
            --rof := Callsign0(islash +1 .. Callsign0'Last);
            Move(Callsign0(islash +1 .. Callsign0'Last), rof, Right, Left, Space);
            llof := Trim(lof, Right)'Length;
            lrof := Trim(rof, Right)'Length;
            is_pfx := llof > 0 and llof <= 4;

            is_sfx := lrof > 0 and lrof <= 3;

            invalid := not (is_pfx or is_sfx);

            if is_pfx and is_sfx then
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
               if is_pfx then
                  t_pfx := lof(1 .. 4);
                  K := NChar(t_pfx(1));
                  K := 37 * K + NChar(t_pfx(2));
                  K := 37 * K + NChar(t_pfx(3));
                  K := 37 * K + NChar(t_pfx(4));
                  Nv2 := 4;
                  I := Index(Callsign0, "/");

                  --Callsign := Callsign0(1 .. I-1);
                  Move(Callsign0(1 .. I-1), Callsign, Right, Left, Space);
                  --Callsign := Callsign0(I+1 .. Callsign0'Length);
                  Move(Callsign0(I+1 .. Callsign0'Length), Callsign, Right, Left, Space);
               end if;
               if is_sfx then
                  t_sfx := rof(1 .. 3);
                  K := NChar(t_sfx(1));
                  K := 37 * K + NChar(t_sfx(2));
                  K := 37 * K + NChar(t_sfx(3));
                  nv2 := 5;
                  I := Index(Callsign0, "/");
                  --Callsign := Callsign0(1 .. I-1);
                  Move(Callsign0(1 .. I-1), Callsign, Right, Left, Space);
               end if;

            end if;
         end if;
      end Ten;



      --Need to find a way to implement common/pfxcom/addpfx im not sure how it effects the behavior of the procedure
   begin
      Init_Pfx(Prefixes, Suffixes);

      Callsign0 := Callsign;
      Nv2 := 1;
      iz := Index(Callsign, " ") - 1;
      if iz < 0 then iz := 12; end if;
      islash := Index(Callsign(Callsign'First .. iz), "/");

      K := 0;

      --C := "   ";

      if islash > 0 and islash <= iz - 4 then
         --C := Callsign(1 .. islash - 1);
         Move(Callsign(Callsign'First .. islash - 1), C, Right, Left, Space);

         --Callsign := Callsign(islash + 1 .. iz);
         Move(Callsign(islash + 1 .. iz), Callsign, Right, Left, Space);

         for I in Prefixes'Range loop
            if Prefixes(I)(1 .. 4) = C(1 .. 4) then --Changed from original fortran code
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
         --Callsign := Callsign(1 .. islash - 1);
         Move(Callsign(Callsign'First .. islash - 1), Callsign, Right, Left, Space);
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
      K : Integer := K0;
      Iz : Integer;
      Suffix : sfx_array;
      Prefix : pfx_array;
   begin
      Init_Pfx(Prefix, Suffix);

      if K > 450 then K := K - 450; end if;

      if K >= 1 and K <= Prefix'Length then
         Iz := Index(Prefix(K), " ") - 1;
         --Callsign := Prefix(K)(1 .. Iz) & '/' & Callsign;
         Move(Prefix(K)(1 .. Iz) & '/' & Callsign, Callsign, Right, Left, Space);
      elsif K >= 401 and K <= (400 + Suffix'Length) then
         Iz := Index(Callsign, " ") - 1;
         --Callsign := Callsign(1 .. Iz) & '/' & Suffix(K - 400);
         Move(Callsign(Callsign'First .. Iz) & '/' & Suffix(K - 400), Callsign, Right, Left, Space);
      elsif K = 449 then
         Iz := Index(add_pfx, " ") - 1;
         if Iz < 1 then Iz := 8; end if;
         Callsign := add_pfx(1 .. Iz) & '/' & Callsign;
         Move(add_pfx(1 .. Iz) & '/' & Callsign, Callsign, Right, Left, Space);
      end if;

   end Get_Pfx2;


   procedure Grid2k(Grid : String; K : out Integer) is
      NLong, NLat : Integer;
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


   procedure Grid2N(Grid : String; N : Integer) is
   begin
      raise Not_Implemented with "Grid2N";
   end Grid2N;


   procedure N2Grid(N : Integer; Gride : String) is
   begin
      raise Not_Implemented with "N2Grid";
   end N2Grid;


   --Converts ascii number, letter, or space to 0-36
   function NChar(C : Character) return Integer is
      N : Integer;
   begin
      if C >= '0' and C <= '9' then
         N := Character'Pos(C) - Character'Pos('0');
      elsif C >= 'A' and C <= 'Z' then
         N := Character'Pos(C) - Character'Pos('A') + 10;
      elsif C >= 'a' and C <= 'z' then
         N := Character'Pos(C) - Character'Pos('a') + 10;
      elsif C >= ' ' then
         N := 36;
      else
         raise Invalid_Callsign with "Invalid character in callsign: " & C;
      end if;

      return N;

   end NChar;


   procedure Pack50(N1, N2 : Integer; Dat : Integer_Array) is
   begin
      raise Not_Implemented with "Pack50";
   end Pack50;


   procedure Pack_Pfx(Call1 : String; N1 : Integer; Ng : Integer; Nadd : Integer) is
   begin
      raise Not_Implemented with "Pack_Pfx";
   end Pack_Pfx;

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
      DLong : Float := DLong0;
      NLat, NLong, N1, N2, N3 : Integer;
   begin
      if DLong < -180.0 then DLong := DLong + 360.0; end if;
      if DLong > 180.0 then DLong := DLong - 360.0; end if;

      NLong := Integer(Float'Floor(60.0 * (180.0 - DLong) / 5.0));
      --Need to check how these are rounded
      N1 := NLong / 240;
      N2 := (NLong - 240 * N1) / 24;
      N3 := NLong - 240 * N1 - 24 * N2;
      Grid(Grid'First) := Character'Val(Character'Pos('A') + N1);
      Grid(Grid'First + 2) := Character'Val(Character'Pos('0') + N2);
      Grid(Grid'First + 4) := Character'Val(Character'Pos('a') + N3);

      NLat := Integer(Float'Floor(60.0 * (DLat + 90.0) / 2.5));
      N1 := NLat / 240;
      N2 := (NLat - 240 * N1) / 24;
      N3 := NLat - 240 * N1 - 24 * N2;
      Grid(Grid'First + 1) := Character'Val(Character'Pos('A') + N1);
      Grid(Grid'First + 3) := Character'Val(Character'Pos('0') + N2);
      Grid(Grid'First + 5) := Character'Val(Character'Pos('a') + N3);

   end Deg2Grid;


   --Something is Wrong here
   procedure Fmtmsg ( Msg : in out String ) is
   begin
      for I in Msg'Range loop
         if Msg(I) = ' ' then
            if I = Msg'Length then
               exit;
            else
               if Msg(I+1) = ' ' then
                  --Msg(I) := Character'Val (127);
                  null;
               end if;
            end if;
         else
            null;
         end if;
      end loop;
      Msg := To_Upper(Msg);
   end Fmtmsg;


end Pack_JT;

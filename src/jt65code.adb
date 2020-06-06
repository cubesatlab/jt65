pragma SPARK_Mode(ON);

with Pack_JT;
with General_JT; use General_JT;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Testmsg; use Testmsg;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Wrapkarn; use Wrapkarn;

procedure JT65Code
is

   Msg, Msg0, Msg1, Decoded, Expected : String (1 .. 22);
   MsgTmpString : String(1 .. 100);
   Msgtmp, Msgchktmp : Unbounded_String;
   Msgtmplength, Msgchktmplength: Integer;
   Cok: String (1 .. 3);
   Bad : String (1 .. 1);
   Msgtype : String (1 .. 12);
   --Sent, Tmp: Integer_Array (1 .. 63);
   Sent1, Sent_Channel, Tmp1: Integer_Array (0 .. 62);
   Dat, Recd_Tmp : Pack_JT.Integer_Array(1 .. 12);
   Dgen, Recd, Dgen_Packed : Integer_Array(0 .. 11);
   Recd_Convert : Integer_Array(1 .. 12);
   Era : Integer_Array (0 .. 50);
   Nspecial, Nmsg, Itype, Counter, Arg_Counter  : Integer;
   Testmsg : Testmsgarray;
   Testmsgchk : Testmsgarray;

   Testing : Boolean;
   IncorrectPress : Boolean;

begin

   Arg_Counter := 0;
    for I in 1 .. Argument_Count loop
      Msgtmp := To_Unbounded_String(Argument(1));
      Arg_Counter := Arg_Counter + Argument_Count;
   end loop;

   Init_Testmsg(Testmsg, Testmsgchk);

   Nmsg := Ntest + 5;

   Testmsg(Ntest + 1) := To_Unbounded_String("KA1ABC WB9XYZ EN34 OOO");
   Testmsg(Ntest + 2) :=  To_Unbounded_String("KA1ABC WB9XYZ OOO");
   Testmsg(Ntest + 3) := To_Unbounded_String("RO");
   Testmsg(Ntest + 4) := To_Unbounded_String("RRR");
   Testmsg(Ntest + 5) := To_Unbounded_String("73");
   Testmsgchk(Ntest + 1) := To_Unbounded_String("KA1ABC WB9XYZ EN34 OOO");
   Testmsgchk(Ntest + 1) := To_Unbounded_String("KA1ABC WB9XYZ OOO");
   Testmsgchk(Ntest + 1) := To_Unbounded_String("RO");
   Testmsgchk(Ntest + 1) := To_Unbounded_String("RRR");
   Testmsgchk(Ntest + 1) := To_Unbounded_String("73");


   --Start := 1;
   IncorrectPress := False;

   if Msgtmp /= "-t" then
      Nmsg := 1;
      --Start := 1;
      Testing := False;
   else
      Testing := True;
   end if;


   for Imsg in 1 .. Nmsg loop

      if Testing and IncorrectPress = False then

            Msgtmp := Testmsg(Imsg);
            Msgtmplength := Length(Msgtmp);
            if Msgtmplength > 22 then
            while Msgtmplength < 100 loop
               Append(Msgtmp, " ");
               Msgtmplength := Msgtmplength + 1;
            end loop;
            MsgTmpString := To_String(Msgtmp);
            Msg := MsgTmpString(1 .. 22);

            else
             while (Msgtmplength <= 21) loop
            Append(Msgtmp, " ");
            Msgtmplength := Msgtmplength + 1;
            end loop;
              Msg := To_String(Msgtmp);
            end if;

            Msgchktmp := Testmsgchk(Imsg);
            Msgchktmplength := Length(Msgchktmp);

            while (Msgchktmplength <= 21) loop
               Append(Msgchktmp, ' ');
               Msgchktmplength := Msgchktmplength + 1;
            end loop;


      elsif Arg_Counter /= 1 then
         Put_Line("Usage: jt65code 'message'");
         Put_Line("       jt65code -t");
         exit;
      else
         Msgtmplength := Length(Msgtmp);
         if Msgtmplength > 22 then
            while Msgtmplength < 100 loop
               Append(Msgtmp, " ");
               Msgtmplength := Msgtmplength + 1;
            end loop;
            MsgTmpString := To_String(Msgtmp);
            Msg := MsgTmpString(1 .. 22);
         else
            while (Msgtmplength <= 21) loop
               Append(Msgtmp, " ");
               Msgtmplength := Msgtmplength + 1;
            end loop;
            Msg := To_String(Msgtmp);
         end if;
         Msgchktmp := Testmsgchk(Imsg);
         Msgchktmplength := Length(Msgchktmp);
         while (Msgchktmplength <= 21) loop
            Append(Msgchktmp, ' ');
            Msgchktmplength := Msgchktmplength + 1;
         end loop;
      end if;

      for I in Era'Range loop
         Era(I) := 0;
      end loop;


      Pack_JT.Fmtmsg(Msg);
      Msg0 := Msg;
      Chkmsg(Msg, Cok, Nspecial);
      Msg1 := Msg;

      if ( Nspecial > 0 ) then

         if ( Nspecial = 2 ) then
            Decoded(1 .. 2) := "RO";
         end if;
         if ( Nspecial = 3 ) then
            Decoded(1 .. 3) := "RRR";
         end if;
         if ( Nspecial = 4 ) then
            Decoded(1 .. 2) := "73";
         end if;
         Itype := -1;
         Msgtype := "Shorthand   ";
         Pack_JT.Pack_Msg(Msg1, Dat, Itype);
         Dgen(0 .. 11) := Integer_Array(Dat(1 .. 12));
         Dgen_Packed(0 .. 11) := Dgen(0 .. 11);
      else

         Pack_JT.Pack_Msg(Msg1, Dat, Itype);
         Dgen(0 .. 11) := Integer_Array(Dat(1 .. 12));
         Dgen_Packed(0 .. 11) := Dgen(0 .. 11);

         if ( Itype = 1 ) then
            Msgtype := "1:Std Msg   ";
         end if;
         if ( Itype = 2 ) then
            Msgtype := "2:Type 1 pfx";
         end if;
         if ( Itype = 3 ) then
            Msgtype := "3:Type 1 sfx";
         end if;
         if ( Itype = 4 ) then
            Msgtype := "4:Type 2 pfx";
         end if;
         if ( Itype = 5 ) then
            Msgtype := "5:Type 2 sfx";
         end if;
         if ( Itype = 6 ) then
            Msgtype := "6:Free Text ";
         end if;
      end if;


         Rs_Encode(Dgen, Sent1);

         --Sent(1 .. 63) := Sent1(0 .. 62);

         -- DEBUG
         --New_Line; New_Line;
         --Put_Line("Rs_Encode: ");
         --for I in Sent1'Range loop
         --   Put(Integer'Image(Sent1(I)));
         --end loop;

         -- DEBUG
         --New_Line;New_Line;
         Interleave63(Sent1, 1);
         --Interleave63(Sent, 1);

         --Put_Line("Interleave: ");
         --for I in Sent'Range loop
         --   Put(Integer'Image(Sent(I)));
         --end loop;

         -- DEBUG
         --New_Line;New_Line;
         Graycode ( Sent1, 1);
         Sent_Channel(0 .. 62) := Sent1(0 .. 62);
         --Put_Line("GrayCode: ");
         --for I in Sent1'Range loop
         --   Put(Integer'Image(Sent1(I)));
         --end loop;


         New_Line; New_Line;


         --Tmp(1 .. 63) := Sent(1 .. 63);
         Tmp1(0 .. 62) := Sent1(0 .. 62);


         Graycode ( Tmp1, -1);
         -- DEBUG
         --Put_Line("Remove Graycode: ");
         --for I in Tmp'Range loop
         --   Put(Integer'Image(Tmp(I)));
         --end loop;
         --New_Line; New_Line;

         Interleave63 ( Tmp1, -1 );
         -- DEBUG
         --Put_Line("Remove-Interleave: ");
         --for I in Tmp'Range loop
         --   Put(Integer'Image(Tmp(I)));
         --end loop;

         --Sent1(0 .. 62) := Tmp(1 .. 63);
         Sent1(0 .. 62) := Tmp1(0 .. 62);
         Rs_Decode( Sent1, Era, 0, Recd);
         --New_Line;New_Line;

         -- DEBUG
         --Put_Line("Rs_Decode: ");
         Counter := 1;
         for I in Recd'Range loop
            --Put(Integer'Image(Integer(Recd(I))));
            Recd_Convert(Counter) := Recd(I);
            Counter := Counter + 1;
         end loop;
         --New_Line;New_Line;

         -- DEBUG
         --Recd_Convert(1 .. 12) := Recd(0 .. 11);
         --Put_Line("Recd_Convert: ");
         --for I in Recd_Convert'Range loop
         --   Put(Integer'Image(Integer(Recd(I))));
         --end loop;
         --New_Line;New_Line;

         Recd_Tmp(1 .. 12) := Pack_JT.Integer_Array(Recd_Convert(1 .. 12));
         -- DEBUG
         --Put_Line("Recd_Tmp: ");
         --for I in Recd'Range loop
         --   Put(Integer'Image(Integer(Recd(I))));
         --end loop;
         --New_Line;New_Line;

         Pack_JT.Unpack_Msg( Recd_Tmp, Decoded );

         Bad := " ";
         Expected := "EXACT                 ";
         if (Decoded /= Msg0) then
            Bad := "*";
            if (Decoded(1 .. 13) = Msg0(1 ..13) and Decoded(14 .. 22) = "         ")
            then Expected := "TRUNCATED             ";
            end if;
         end if;
         Put_Line("   Message                    Decoded             Err? Type             Expected");
         Put_Line("--------------------------------------------------------------------------------");
         Put(Imsg'Image);
         Put(". " & Msg0(1 .. 22));
         Put("   " & Decoded(1 .. 22));
         Put(" " & Bad);
         Put("  " & Msgtype);
         Put("    " & Expected);
         New_Line;New_Line;
         if ( Cok = "000" ) then Decoded ( 20 .. 22) := cok;
         end if;
         Put("Packed message, 6-bit symbols: ");
         for I in Dgen_Packed'Range loop
            Put(Integer'Image(Dgen_Packed(I)));
         end loop;
         New_Line;New_Line;
         Put_Line("Information-carrying channel symbols");
         for I in Sent_Channel'Range loop
            Put(Integer'Image(Sent_Channel(I)));
            if I = 20 or I = 41 then
               New_Line;
            end if;
         end loop;
         Pack_JT.Fmtmsg(Decoded);

      New_Line;New_Line;New_Line;
   end loop;
end JT65Code;






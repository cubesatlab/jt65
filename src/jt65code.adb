with Pack_JT;
with General_JT; use General_JT;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with General_JT; use General_JT;
with Testmsg; use Testmsg;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Wrapkarn; use Wrapkarn;
with Interfaces; use Interfaces;
with Init_Rs; use Init_Rs;
with Ada.Command_Line; use Ada.Command_Line;

procedure JT65Code
is

   Msg, Msg0, Msg1, Decoded, Expected : String (1 .. 22);
   MsgTmpString : String(1 .. 100);
   Msgtmp, Msgchktmp : Unbounded_String;
   Msgtmplength, Msgchktmplength: Integer;
   Cok: String (1 .. 3);
   Bad : String (1 .. 1);
   Msgtype : String (1 .. 10);
   Sent, Tmp, Holder : Integer_Array (1 .. 63);
   Sent1, Tmp1: Integer_Array (0 .. 62);
   Dat, Recd_Tmp : Pack_JT.Integer_Array(1 .. 12);
   Dgen, Recd : Integer_Array(0 .. 11);
   Recd_Convert : Integer_Array(1 .. 12);
   Era : Integer_Array (0 .. 50);
   --Args : Natural;
   Nspecial, Nmsg, Itype, Counter  : Integer;
   Nerr, Start : Integer := 0;
   Testmsg : Testmsgarray;
   Testmsgchk : Testmsgarray;

   Testing : Boolean;

begin

    for I in 1 .. Argument_Count loop
            Msgtmp := To_Unbounded_String(Argument(1));
    end loop;

   Init_Testmsg(Testmsg, Testmsgchk);

         Testmsg(Ntest + 1) := To_Unbounded_String("KA1ABC WB9XYZ EN34 OOO");
         Testmsg(Ntest + 2) :=  To_Unbounded_String("KA1ABC WB9XYZ OOO");
         Testmsg(Ntest + 3) := To_Unbounded_String("RO");
         Testmsg(Ntest + 3) := To_Unbounded_String("RRR");
         Testmsg(Ntest + 3) := To_Unbounded_String("73");

         Testmsgchk(Ntest + 1) := To_Unbounded_String("KA1ABC WB9XYZ EN34 OOO");
         Testmsgchk(Ntest + 1) := To_Unbounded_String("KA1ABC WB9XYZ OOO");
         Testmsgchk(Ntest + 1) := To_Unbounded_String("RO");
         Testmsgchk(Ntest + 1) := To_Unbounded_String("RRR");
         Testmsgchk(Ntest + 1) := To_Unbounded_String("73");

         Nmsg := Ntest + 5;

   Start := 1;
   if Msgtmp /= "-t" then
      Nmsg := 2;
      Start := 2;
      Testing := False;
   else
      Testing := True;
   end if;

   for Imsg in Start .. Nmsg loop

      if ( Testing ) then

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

      Pack_JT.Fmtmsg(Msg);
      Msg0 := Msg;
      Chkmsg(Msg, Cok, Nspecial);
      Msg1 := Msg;
      --Put_Line(Msg1);

      if ( Nspecial > 0 ) then
         if ( Nspecial = 2 ) then Decoded(1 .. 2) := "RO";
         end if;
         if ( Nspecial = 3 ) then Decoded(1 .. 3) := "RRR";
         end if;
         if ( Nspecial = 4 ) then Decoded(1 .. 2) := "73";
         end if;
         Itype := -1;
         Msgtype := "Shorthand ";
      else

         Pack_JT.Pack_Msg(Msg1, Dat, Itype);
         Dgen(0 .. 11) := Integer_Array(Dat(1 .. 12));

         if ( Itype = 1 ) then Msgtype := "Std Msg   ";
         end if;
         if ( Itype = 2 ) then Msgtype := "Type 1 pfx";
         end if;
         if ( Itype = 3 ) then Msgtype := "Type 1 sfx";
         end if;
         if ( Itype = 4 ) then Msgtype := "Type 2 pfx";
         end if;
         if ( Itype = 5 ) then Msgtype := "Type 2 sfx";
         end if;
         if ( Itype = 6 ) then Msgtype := "Free Text ";
         end if;


         Put_Line("   Message");
         Put_Line("-----------------------------------------------------------");


         Put("   " & Msg0(1 .. 22));

         New_Line;New_Line;

         Put_Line("Packed message, 6-bit symbols: ");
         for I in Dgen'Range loop
            Put(Integer'Image(Dgen(I)));
         end loop;



         Rs_Encode(Dgen, Sent1);

         --Sent(1 .. 63) := Sent1(0 .. 62);

         -- DEBUG
         --New_Line; New_Line;
         --Put_Line("Rs_Encode: ");
         --for I in Sent'Range loop
         --   Put(Integer'Image(Sent(I)));
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
         --Graycode ( Sent, 63);

         --Put_Line("GrayCode: ");
         --for I in Sent'Range loop
         --   Put(Integer'Image(Sent(I)));
         --end loop;


         New_Line; New_Line;
         Put_Line("Information-carrying channel symbols");
         for I in Sent1'Range loop
            Put(Integer'Image(Sent1(I)));
         end loop;

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
         Rs_Decode( Sent1, Era, 0, Recd, Nerr);
         New_Line;New_Line;

         -- DEBUG
         --Put_Line("Rs_Decode: ");
         Counter := 1;
         for I in Recd'Range loop
            --Put(Integer'Image(Integer(Recd(I))));
            Recd_Convert(Counter) := Recd(I);
            Counter := Counter + 1;
         end loop;
         --New_Line;New_Line;


         --Recd_Convert(1 .. 12) := Recd(0 .. 11);
         --Put_Line("Recd_Convert: ");
         --for I in Recd_Convert'Range loop
         --   Put(Integer'Image(Integer(Recd(I))));
         --end loop;
         --New_Line;New_Line;


         Recd_Tmp(1 .. 12) := Pack_JT.Integer_Array(Recd_Convert(1 .. 12));
         --Put_Line("Recd_Tmp: ");
         --for I in Recd'Range loop
         --   Put(Integer'Image(Integer(Recd(I))));
         --end loop;
         --New_Line;New_Line;

         Pack_JT.Unpack_Msg ( Recd_Tmp, Decoded );

         Bad := " ";
         Expected := "EXACT                 ";
         if (Decoded /= Msg0) then
            Bad := "*";
            if (Decoded(1 .. 13) = Msg0(1 ..13) and Decoded(14 .. 22) = "         ")
            then Expected := "TRUNCATED             ";
            end if;
         end if;
         Put_Line("   Decoded           Err?      Type               Expected        ");
         Put_Line("-----------------------------------------------------------");
         Put("   " & Decoded(1 .. 22));
         Put("  " & Bad);
         Put("   " & Msgtype);
         Put("         " & Expected); New_Line;

         --New_Line;New_Line;New_Line;

         if ( Cok = "000" ) then Decoded ( 20 .. 22) := cok;
         end if;
         Pack_JT.Fmtmsg(Decoded);
      end if;


      New_Line;New_Line;New_Line;
   end loop;
   Nmsg := 1;
   Nspecial := 0;
   if ( Nmsg = 1 and Nspecial = 0) then
      null;
   end if;

end JT65Code;





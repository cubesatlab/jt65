with Pack_JT; use Pack_JT;
with General_JT; use General_JT;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with General_JT; use General_JT;
with Testmsg; use Testmsg;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Wrapkarn; use Wrapkarn;
with Interfaces; use Interfaces;
with Init_Rs; use Init_Rs;

procedure JT65Code is

   Msg, Msg0, Msg1, Decoded, Expected : String (1 .. 22);
   Msgtmp, Msgchktmp : Unbounded_String;
   Msgtmplength, Msgchktmplength: Integer;
   Cok: String (1 .. 3);
   Bad : String (1 .. 1);
   Msgtype : String (1 .. 10);
   Sent, Tmp, Holder : Integer_Array (1 .. 63);
   Sent1: Integer_Array (0 .. 62);
   Dat : Integer_Array(1 .. 12);
   Dgen, Recd : Integer_Array(0 .. 11);
   Era : Integer_Array (0 .. 50);
   --Args : Natural;
   Nspecial, Nmsg, Itype  : Integer;
   Nerr : Integer := 0;
   Testmsg : Testmsgarray;
   Testmsgchk : Testmsgarray;

begin


   --Args := Argument_Count;
   --if ( Args /= 1 ) then
      --Put("Usage: jt65code 'message'"); New_Line;
      --Put("       jt65code -t"); New_Line;
   --end if;

      Nmsg := 1;
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

   for Imsg in 1 .. Nmsg loop
      -- for Imsg in 1 .. Nmsg loop -- Use this loop initializer to loop over
      -- every test message


      if ( Nmsg > 1 ) then
         --Msg := To_String(Testmsg(Imsg));
         --Msg := To_String(Testmsg(Imsg));
         --Msgchk := To_String(Testmsgchk(Imsg));
         Msgtmp := Testmsg(Imsg);
         Msgtmplength := Length(Msgtmp);

         while (Msgtmplength <= 21) loop
            Append(Msgtmp, " ");
            --Msgtmp :=  Msgtmp + To_Unbounded_String(" ");
            Msgtmplength := Msgtmplength + 1;
         end loop;

         Msg := To_String(Msgtmp);

         Msgchktmp := Testmsgchk(Imsg);
         Msgchktmplength := Length(Msgchktmp);
         while (Msgchktmplength <= 21) loop
            Append(Msgchktmp, ' ');
            Msgchktmplength := Msgchktmplength + 1;
         end loop;

         --Put_Line(Integer'Image(Msgtmplength));


      end if;

      Fmtmsg(Msg);
      Msg0 := Msg;
      Chkmsg(Msg, Cok, Nspecial);
      Msg1 := Msg;

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

         Pack_Msg(Msg1, Dat, Itype);
         Dgen(0 .. 11) := Dat(1 .. 12);

         Put_Line("Msg0 : " & Msg0(1 .. 22));New_Line;

         Put_Line("Packed message, 6-bit symbols: ");
         for I in Dgen'Range loop
            Put(Integer'Image(Dgen(I)));
         end loop;

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

         Rs_Encode(Dgen, Sent1);

         Sent(1 .. 63) := Sent1(0 .. 62);

         New_Line; New_Line;
         Put_Line("Rs_Encode: ");
         for I in Sent'Range loop
            Put(Integer'Image(Sent(I)));
         end loop;

         New_Line;New_Line;
         Interleave63(Sent, 1, Holder);
         Put_Line("Interleave: ");
         for I in Sent'Range loop
            Put(Integer'Image(Sent(I)));
         end loop;

         New_Line;New_Line;
         Graycode ( Sent, 63, 1);
         Put_Line("GrayCode: ");
         for I in Sent'Range loop
            Put(Integer'Image(Sent(I)));
         end loop;

         New_Line; New_Line;
         Tmp(1 .. 63) := Sent(1 .. 63);

         Graycode ( Tmp, 63, -1);
         Put_Line("Remove Graycode: ");
         for I in Tmp'Range loop
            Put(Integer'Image(Tmp(I)));
         end loop;
         New_Line; New_Line;

         Interleave63 ( Tmp, -1, Holder );
         Put_Line("Remove-Interleave: ");
         for I in Tmp'Range loop
            Put(Integer'Image(Tmp(I)));
         end loop;

         Sent1(0 .. 62) := Tmp(1 .. 63);
         Rs_Decode( Sent1, Era, 0, Recd, Nerr);
         New_Line;New_Line;

         Put_Line("Rs_Decode: ");
         for I in Recd'Range loop
            Put(Integer'Image(Integer(Recd(I))));
         end loop;
         New_Line;

         --Unpack_Msg ( Recd, Decoded );

         New_Line;New_Line;New_Line;

         if ( Cok = "000" ) then Decoded ( 20 .. 22) := cok;
         end if;
         Fmtmsg(Decoded);
      end if;
      Bad := " ";
      Expected := "EXACT                 ";
      if (Decoded /= Msg0) then
         Bad := "*";
         if (Decoded(1 .. 13) = Msg0(1 ..13) and Decoded(14 .. 22) = "         ")
         then Expected := "TRUNCATED             ";
         end if;
      end if;

   end loop;
   Nmsg := 1;
   Nspecial := 0;
   if ( Nmsg = 1 and Nspecial = 0) then
      null;
   end if;

end JT65Code;




--------------------------------------------------------------------------------
-- FILE   : jt65code.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces;              use Interfaces;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with General_JT;     use General_JT;
with Pack_JT;
with Testmsg;        use Testmsg;
with Unsigned_Array; use Unsigned_Array;
with Wrapkarn;       use Wrapkarn;


procedure JT65Code is
   Msg, Msg0, Msg1, Decoded, Expected : String (1 .. 22);
   MsgTmpString : String(1 .. 100);
   Msgtmp : Unbounded_String;
   Cok: String (1 .. 3);
   Bad : String (1 .. 1);
   Msgtype : String (1 .. 12);
   Sent, Sent_Channel : Unsigned_8_array(0 .. 62);
   Recd, Dat_Packed, Dat, Recd_Tmp : Unsigned_8_array(0 .. 11);
   Recd_Convert : Unsigned_8_array(1 .. 12);
   Era : Unsigned_8_array (0 .. 50);
   Nspecial, Nmsg, Itype, Counter, Arg_Counter  : Integer;
   Testmsg, Testmsgchk : Testmsgarray;

   Testing : Boolean;
   IncorrectPress : Boolean;
begin
   -- Initialization
   Arg_Counter := 0;
   IncorrectPress := False;

   for I in 1 .. Argument_Count loop
      Msgtmp := To_Unbounded_String(Argument(1));
      Arg_Counter :=  Argument_Count;
   end loop;

   if Msgtmp /= "-t" then
      Nmsg := 1;
      Testing := False;
   else
      Testing := True;
      Nmsg := Maxtest;
      Init_Testmsg(Testmsg, Testmsgchk);
   end if;

   for Imsg in 1 .. Nmsg loop

      if Testing and IncorrectPress = False then
         Msgtmp := Testmsg(Imsg);
      elsif Arg_Counter /= 1 then
         Put_Line("Usage: jt65code 'message'");
         Put_Line("       jt65code -t");
         exit;
      end if;

      while Length(Msgtmp) < 100 loop
         Append(Msgtmp, " ");
      end loop;
      if Length(Msgtmp) = 100 then
         MsgTmpString := To_String(Msgtmp);
         Msg := MsgTmpString(1 .. 22);
      else
         exit;
      end if;

      -- This will be dealt with. Satisfies SPARK warning for now.
      for I in Era'Range loop
         Era(I) := 0;
      end loop;

      --Pack_JT.Fmtmsg(Msg);
      Msg := To_Upper(Msg);
      Collapse_Blanks(Msg);
      Msg0 := Msg;
      Chkmsg(Msg, Cok, Nspecial);
      Msg1 := Msg;

      -- Packing Message
      Pack_JT.Pack_Msg(Msg1, Dat, Itype);
      Dat_Packed(0 .. 11) := Dat(0 .. 11); -- Used for printing 6-bit symbols and for packed check in decode

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
         Msgtype := "-1:Shorthand";
      else
         if ( Itype = 1 ) then
            Msgtype := "1:Std msg   ";
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
            Msgtype := "6:Free text ";
         end if;
      end if;

      Rs_Encode(Dat, Sent);
      Interleave63(Sent, 1);
      Graycode(Sent, 1);
      Sent_Channel(0 .. 62) := Sent(0 .. 62); -- Used for printing channel symbols
      Graycode ( Sent, -1);
      Interleave63 ( Sent, -1 );
      Rs_Decode( Sent, Era, 0, Recd, Dat_Packed);

      -- Converting to type Pack_JT.Integer_Array
      Counter := 1;
      for I in Recd'Range loop
         Recd_Convert(Counter) := Recd(I);
         Counter := Counter + 1;
      end loop;
      Recd_Tmp(0 .. 11) := Recd_Convert(1 .. 12);
      Pack_JT.Unpack_Msg( Recd_Tmp, Decoded ); -- 0T0 error in Unpack_Msg
      if ( Cok = "OOO" ) then
         Decoded ( 20 .. 22) := cok;
      end if;

      Decoded := To_Upper(Decoded);
      Collapse_Blanks(Decoded);

      Bad := " ";
      Expected := "EXACT                 ";
      if (Decoded /= Msg0) then
         Bad := "*";
         if (Decoded(1 .. 13) = Msg0(1 ..13) and Decoded(14 .. 22) = "         ")
         then Expected := "TRUNCATED             ";
         end if;
      end if;

      -- Printing to terminal
      Put_Line("    Message                Decoded              Err? Type          Expected");
      Put_Line("----------------------------------------------------------------------------");
      Put(Imsg'Image);
      Put(". " & Msg0(1 .. 22));
      Put(" " & Decoded(1 .. 22));
      Put(" " & Bad);
      Put("  " & Msgtype);
      Put("  " & Expected);
      New_Line(2);
      if Itype /= -1 then
         Put("Packed message, 6-bit symbols ");
         for I in Dat_Packed'Range loop
            if Dat_Packed(I) <= 9 then
               Put(" " &Unsigned_8'Image(Dat_Packed(I)));
            else
               Put(Unsigned_8'Image(Dat_Packed(I)));
            end if;
         end loop;
         New_Line(2);
         Put_Line("Information-carrying channel symbols");
         Put("  ");
         for I in Sent_Channel'Range loop
            if Sent_Channel(I) <= 9 then
               Put(" " &Unsigned_8'Image(Sent_Channel(I)));
            else
               Put(Unsigned_8'Image(Sent_Channel(I)));
            end if;
            if I = 20 or I = 41 then
               New_Line;
               Put("  ");
            end if;
         end loop;
      end if;
      New_Line;
   end loop;
end JT65Code;

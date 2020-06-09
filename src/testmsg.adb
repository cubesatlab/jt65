--------------------------------------------------------------------------------
-- FILE   : testmsg.adb
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
package body Testmsg is

   procedure Init_Testmsg(Msg : in out Testmsgarray; Msgchk : in out Testmsgarray) is

   begin

      Msg(1) := To_Unbounded_String("HELLO    ");
      Msg(2) := To_Unbounded_String("CUBESAT LABOR   ");
      Msg(3) := To_Unbounded_String("QRZ WB9XYZ EN34");
      Msg(4) := To_Unbounded_String("KA1ABC WB9XYZ EN34");
      Msg(5) := To_Unbounded_String("KA1ABC WB9XYZ RO");
      Msg(6) := To_Unbounded_String("KA1ABC WB9XYZ -21");
      Msg(7) := To_Unbounded_String("KA1ABC WB9XYZ R-19");
      Msg(8) := To_Unbounded_String("KA1ABC WB9XYZ RRR");
      Msg(9) := To_Unbounded_String("KA1ABC WB9XYZ 73");
      Msg(10) := To_Unbounded_String("KA1ABC WB9XYZ");
      Msg(11) := To_Unbounded_String("CQ 010 WB9XYZ EN34");
      Msg(12) := To_Unbounded_String("CQ 999 WB9XYZ EN34");
      Msg(13) := To_Unbounded_String("CQ EU WB9XYZ EN34");
      Msg(14) := To_Unbounded_String("CQ WY WB9XYZ EN34");
      Msg(15) := To_Unbounded_String("1A/KA1ABC WB9XYZ");
      Msg(16) := To_Unbounded_String("E5/KA1ABC WB9XYZ");
      Msg(17) := To_Unbounded_String("KA1ABC 1A/WB9XYZ");
      Msg(18) := To_Unbounded_String("KA1ABC E5/WB9XYZ");
      Msg(19) := To_Unbounded_String("KA1ABC/P WB9XYZ");
      Msg(20) := To_Unbounded_String("KA1ABC/A WB9XYZ");
      Msg(21) := To_Unbounded_String("KA1ABC WB9XYZ/P");
      Msg(22) := To_Unbounded_String("KA1ABC WB9XYZ/A");
      Msg(23) := To_Unbounded_String("CQ KA1ABC/P");
      Msg(24) := To_Unbounded_String("CQ WB9XYZ/A");
      Msg(25) := To_Unbounded_String("QRZ KA1ABC/P");
      Msg(26) := To_Unbounded_String("QRZ WB9XYZ/A");
      Msg(27) := To_Unbounded_String("DE KA1ABC/P");
      Msg(28) := To_Unbounded_String("DE WB9XYZ/A");
      Msg(29) := To_Unbounded_String("CQ 1A/KA1ABC");
      Msg(30) := To_Unbounded_String("CQ E5/KA1ABC");
      Msg(31) := To_Unbounded_String("DE 1A/KA1ABC");
      Msg(32) := To_Unbounded_String("DE E5/KA1ABC");
      Msg(33) := To_Unbounded_String("QRZ 1A/KA1ABC");
      Msg(34) := To_Unbounded_String("QRZ E5/KA1ABC");
      Msg(35) := To_Unbounded_String("CQ WB9XYZ/1A");
      Msg(36) := To_Unbounded_String("CQ WB9XYZ/E5");
      Msg(37) := To_Unbounded_String("QRZ WB9XYZ/1A");
      Msg(38) := To_Unbounded_String("QRZ WB9XYZ/E5");
      Msg(39) := To_Unbounded_String("DE WB9XYZ/1A");
      Msg(40) := To_Unbounded_String("DE WB9XYZ/E5");
      Msg(41) := To_Unbounded_String("CQ A000/KA1ABC FM07");
      Msg(42) := To_Unbounded_String("CQ ZZZZ/KA1ABC FM07");
      Msg(43) := To_Unbounded_String("QRZ W4/KA1ABC FM07");
      Msg(44) := To_Unbounded_String("DE W4/KA1ABC FM07");
      Msg(45) := To_Unbounded_String("CQ W4/KA1ABC -22");
      Msg(46) := To_Unbounded_String("DE W4/KA1ABC -22");
      Msg(47) := To_Unbounded_String("QRZ W4/KA1ABC -22");
      Msg(48) := To_Unbounded_String("CQ W4/KA1ABC R-22");
      Msg(49) := To_Unbounded_String("DE W4/KA1ABC R-22");
      Msg(50) := To_Unbounded_String("QRZ W4/KA1ABC R-22");
      Msg(51) := To_Unbounded_String("DE W4/KA1ABC 73");
      Msg(52) := To_Unbounded_String("CQ KA1ABC FM07");
      Msg(53) := To_Unbounded_String("QRZ KA1ABC FM07");
      Msg(54) := To_Unbounded_String("DE KA1ABC/VE6 FM07");
      Msg(55) := To_Unbounded_String("CQ KA1ABC/VE6 -22");
      Msg(56) := To_Unbounded_String("DE KA1ABC/VE6 -22");
      Msg(57) := To_Unbounded_String("QRZ KA1ABC/VE6 -22");
      Msg(58) := To_Unbounded_String("CQ KA1ABC/VE6 R-22");
      Msg(59) := To_Unbounded_String("DE KA1ABC/VE6 R-22");
      Msg(60) := To_Unbounded_String("QRZ KA1ABC/VE6 R-22");
      Msg(61) := To_Unbounded_String("DE KA1ABC 73");
      Msg(62) := To_Unbounded_String("HELLO WORLD");
      Msg(63) := To_Unbounded_String("ZL4/KA1ABC 73");
      Msg(64) := To_Unbounded_String("KA1ABC XL/WB9XYZ");
      Msg(65) := To_Unbounded_String("KA1ABC WB9XYZ/W4");
      Msg(66) := To_Unbounded_String("DE KA1ABC/QRP 2W");
      Msg(67) := To_Unbounded_String("KA1ABC/1 WB9XYZ/1");
      Msg(68) := To_Unbounded_String("123456789ABCDEFGH");
      Msg(69) := To_Unbounded_String("KA1ABC WB9XYZ EN34 OOO");
      Msg(70) := To_Unbounded_String("KA1ABC WB9XYZ OOO");
      Msg(71) := To_Unbounded_String("RO");
      Msg(72) := To_Unbounded_String("RRR");
      Msg(73) := To_Unbounded_String("73");


      Msgchk(1) := To_Unbounded_String("CQ WB9XYZ EN34");
      Msgchk(2) := To_Unbounded_String("CQ DX WB9XYZ EN34");
      Msgchk(3) := To_Unbounded_String("QRZ WB9XYZ EN34");
      Msgchk(4) := To_Unbounded_String("KA1ABC WB9XYZ EN34");
      Msgchk(5) := To_Unbounded_String("KA1ABC WB9XYZ RO");
      Msgchk(6) := To_Unbounded_String("KA1ABC WB9XYZ -21");
      Msgchk(7) := To_Unbounded_String("KA1ABC WB9XYZ R-19");
      Msgchk(8) := To_Unbounded_String("KA1ABC WB9XYZ RRR");
      Msgchk(9) := To_Unbounded_String("KA1ABC WB9XYZ 73");
      Msgchk(10) := To_Unbounded_String("KA1ABC WB9XYZ");
      Msgchk(11) := To_Unbounded_String("CQ 000 WB9XYZ EN34");
      Msgchk(12) := To_Unbounded_String("CQ 999 WB9XYZ EN34");
      Msgchk(13) := To_Unbounded_String("CQ EU WB9XYZ EN34");
      Msgchk(14) := To_Unbounded_String("CQ WY WB9XYZ EN34");
      Msgchk(15) := To_Unbounded_String("1A/KA1ABC WB9XYZ");
      Msgchk(16) := To_Unbounded_String("E5/KA1ABC WB9XYZ");
      Msgchk(17) := To_Unbounded_String("KA1ABC 1A/WB9XYZ");
      Msgchk(18) := To_Unbounded_String("KA1ABC E5/WB9XYZ");
      Msgchk(19) := To_Unbounded_String("KA1ABC/P WB9XYZ");
      Msgchk(20) := To_Unbounded_String("KA1ABC/A WB9XYZ");
      Msgchk(21) := To_Unbounded_String("KA1ABC WB9XYZ/P");
      Msgchk(22) := To_Unbounded_String("KA1ABC WB9XYZ/A");
      Msgchk(23) := To_Unbounded_String("CQ KA1ABC/P");
      Msgchk(24) := To_Unbounded_String("CQ WB9XYZ/A");
      Msgchk(25) := To_Unbounded_String("QRZ KA1ABC/P");
      Msgchk(26) := To_Unbounded_String("QRZ WB9XYZ/A");
      Msgchk(27) := To_Unbounded_String("DE KA1ABC/P");
      Msgchk(28) := To_Unbounded_String("DE WB9XYZ/A");
      Msgchk(29) := To_Unbounded_String("CQ 1A/KA1ABC");
      Msgchk(30) := To_Unbounded_String("CQ E5/KA1ABC");
      Msgchk(31) := To_Unbounded_String("DE 1A/KA1ABC");
      Msgchk(32) := To_Unbounded_String("DE E5/KA1ABC");
      Msgchk(33) := To_Unbounded_String("QRZ 1A/KA1ABC");
      Msgchk(34) := To_Unbounded_String("QRZ E5/KA1ABC");
      Msgchk(35) := To_Unbounded_String("CQ WB9XYZ/1A");
      Msgchk(36) := To_Unbounded_String("CQ WB9XYZ/E5");
      Msgchk(37) := To_Unbounded_String("QRZ WB9XYZ/1A");
      Msgchk(38) := To_Unbounded_String("QRZ WB9XYZ/E5");
      Msgchk(39) := To_Unbounded_String("DE WB9XYZ/1A");
      Msgchk(40) := To_Unbounded_String("DE WB9XYZ/E5");
      Msgchk(41) := To_Unbounded_String("CQ A000/KA1ABC FM07");
      Msgchk(42) := To_Unbounded_String("CQ ZZZZ/KA1ABC FM07");
      Msgchk(43) := To_Unbounded_String("QRZ W4/KA1ABC FM07");
      Msgchk(44) := To_Unbounded_String("DE W4/KA1ABC FM07");
      Msgchk(45) := To_Unbounded_String("CQ W4/KA1ABC -22");
      Msgchk(46) := To_Unbounded_String("DE W4/KA1ABC -22");
      Msgchk(47) := To_Unbounded_String("QRZ W4/KA1ABC -22");
      Msgchk(48) := To_Unbounded_String("CQ W4/KA1ABC R-22");
      Msgchk(49) := To_Unbounded_String("DE W4/KA1ABC R-22");
      Msgchk(50) := To_Unbounded_String("QRZ W4/KA1ABC R-22");
      Msgchk(51) := To_Unbounded_String("DE W4/KA1ABC 73");
      Msgchk(52) := To_Unbounded_String("CQ KA1ABC FM07");
      Msgchk(53) := To_Unbounded_String("QRZ KA1ABC FM07");
      Msgchk(54) := To_Unbounded_String("DE KA1ABC/VE6 FM07");
      Msgchk(55) := To_Unbounded_String("CQ KA1ABC/VE6 -22");
      Msgchk(56) := To_Unbounded_String("DE KA1ABC/VE6 -22");
      Msgchk(57) := To_Unbounded_String("QRZ KA1ABC/VE6 -22");
      Msgchk(58) := To_Unbounded_String("CQ KA1ABC/VE6 R-22");
      Msgchk(59) := To_Unbounded_String("DE KA1ABC/VE6 R-22");
      Msgchk(60) := To_Unbounded_String("QRZ KA1ABC/VE6 R-22");
      Msgchk(61) := To_Unbounded_String("DE KA1ABC 73");
      Msgchk(62) := To_Unbounded_String("HELLO WORLD");
      Msgchk(63) := To_Unbounded_String("ZL4/KA1ABC 73");
      Msgchk(64) := To_Unbounded_String("KA1ABC XL/WB9");
      Msgchk(65) := To_Unbounded_String("KA1ABC WB9XYZ");
      Msgchk(66) := To_Unbounded_String("DE KA1ABC/QRP");
      Msgchk(67) := To_Unbounded_String("KA1ABC/1 WB9X");
      Msgchk(68) := To_Unbounded_String("123456789ABCD");
      Msgchk(69) := To_Unbounded_String("KA1ABC WB9XYZ EN34 OOO");
      Msgchk(70) := To_Unbounded_String("KA1ABC WB9XYZ OOO");
      Msgchk(71) := To_Unbounded_String("RO");
      Msgchk(72) := To_Unbounded_String("RRR");
      Msgchk(73) := To_Unbounded_String("73");

   end Init_Testmsg;


end Testmsg;

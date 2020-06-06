--------------------------------------------------------------------------------
-- FILE   : testmsg.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Testmsg is

   Maxtest : constant Integer := 73;

   type Testmsgarray is array (1 .. Maxtest) of Unbounded_String;

   -- Initializes the two message arrays. Used for testing in jt65code.adb
   procedure Init_Testmsg (Msg : out Testmsgarray; Msgchk : out Testmsgarray);

end Testmsg;

--------------------------------------------------------------------------------
-- FILE   : testmsg.ads
-- SUBJECT: Specification of a package for [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Testmsg is

   Maxtest : constant Integer := 73;

   type Testmsgarray is array (1 .. Maxtest) of Unbounded_String;

   -- Initializes the two message arrays. Used for testing in jt65code.adb
   procedure Init_Testmsg(Msg : in out Testmsgarray; Msgchk : in out Testmsgarray)
     with
       Pre => ((Msg'First = 1 and Msg'Last = 73) and (Msgchk'First = 1 and Msgchk'Last = 73));

end Testmsg;


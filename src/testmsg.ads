--------------------------------------------------------------------------------
-- FILE   : testmsg.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Testmsg is

   Maxtest : constant Integer := 73;
   Array_Out_Of_Bounds : exception;

   type Testmsgarray is array (1 .. Maxtest) of Unbounded_String;

   -- Initializes the two message arrays. Used for testing in jt65code.adb
   procedure Init_Testmsg (Msg : in out Testmsgarray; Msgchk : in out Testmsgarray)
     with
       Pre => ((Msg'Length = 73 and Msg'First = 1 and Msg'Last = 73)
     and (Msgchk'Length = 73 and Msgchk'First = 1 and Msgchk'Last = 73))
     or else raise Array_Out_Of_Bounds;

end Testmsg;

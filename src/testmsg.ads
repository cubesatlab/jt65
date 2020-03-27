with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Testmsg is

   Maxtest : constant Integer := 75;
   Ntest : constant Integer := 68;

   type Testmsgarray is array (1 .. Maxtest) of Unbounded_String;

   -- Initializes the two message arrays. Used for testing in jt65code.adb
   procedure Init_Testmsg (Msg : in out Testmsgarray; Msgchk : in out Testmsgarray);

   function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

end Testmsg;

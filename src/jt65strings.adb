--------------------------------------------------------------------------------
-- FILE   : jt65string.ads
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body JT65Strings is

   -- This function is only a partial validation of callsigns. In particular it allows invalid
   -- callsigns such as "123XYZ", "AA1X Y", and "AA1 XY".
   -- TODO: Strengthen this function to make it more precise and eliminate *all* bad callsigns.
   function Is_Valid_Callsign(Callsign : in Callsign_Type) return Boolean is
   begin
      return
        (Is_Upper(Callsign(1)) or Is_Digit(Callsign(1)) or Callsign(1) = ' ') and
        (Is_Upper(Callsign(2)) or Is_Digit(Callsign(2)))                      and
        (                         Is_Digit(Callsign(3)))                      and
        (Is_Upper(Callsign(4))                          or Callsign(4) = ' ') and
        (Is_Upper(Callsign(5))                          or Callsign(5) = ' ') and
        (Is_Upper(Callsign(6))                          or Callsign(6) = ' ');
   end Is_Valid_Callsign;

end JT65Strings;

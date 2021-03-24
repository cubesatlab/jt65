--------------------------------------------------------------------------------
-- FILE   : jt65string.ads
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

-- @summary
-- This package contains type definitions (and supporting subprograms) that apply useful
-- constraints to ordinary strings as appropriate for JT65 protocol data.
--
package JT65Strings is

   subtype JT65_Character is Character
     with Static_Predicate =>
       (JT65_Character in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | ' ' | '+' | '-' | '.' | '/' | '?');

   subtype JT65_String is String
     with Dynamic_Predicate =>
       (for all I in JT65_String'Range =>
            JT65_String(I) in JT65_Character) and
           (JT65_String'First = 1 and JT65_String'Last >= 2 and JT65_String'Last <= 22);

   subtype JT65_String_Unconstrained is String
     with Dynamic_Predicate =>
       (for all I in JT65_String_Unconstrained'Range =>
          JT65_String_Unconstrained(I) in JT65_Character);

   subtype Callsign_Character is Character
     with Static_Predicate =>
       (Callsign_Character in '0' .. '9' | 'A' .. 'Z' | ' ');
   -- A subtype for representing only allowed characters in callsigns.
   -- TODO: Do we need to allow lower case letters also?

   subtype Callsign_Type is String(1 .. 6)
     with Dynamic_Predicate =>
       (for all I in Callsign_Type'Range => Callsign_Type(I) in Callsign_Character);
   -- A subtype for strings of Callsign_Character of a specific size. Actual callsigns require
   -- additional constraints as expressed by function Is_Valid_Callsign.

   function Is_Valid_Callsign(Callsign : in Callsign_Type) return Boolean;
   -- Returns True if the Callsign follows the rules of valid callsigns; False otherwise.
   -- Callsign_Type does not assert this in a dynamic predicate because there are cases where
   -- temporarily invalid callsigns are stored in Callsign_Type objects.
   --
   -- Valid callsigns have the form 'ppNsss' where 'pp' is one or two letters or digits, N is a
   -- digit, and 'sss' is one to three letters. Only a single digit is allowed in 'pp'. Spaces,
   -- if present, must be leading (one 'p') or trailing (one or two 's').

end JT65Strings;

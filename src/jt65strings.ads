--------------------------------------------------------------------------------
-- FILE   : jt65string.ads
-- SUBJECT: [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

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

   -- A subtype for representing only allowed characters in callsigns.
   -- TODO: Do we really need to allow lower case letters also?
   subtype Callsign_Character is Character
     with Static_Predicate =>
       (Callsign_Character in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | ' ');

   -- Callsigns are at most six characters, but may contain trailing spaces.
   -- TODO: Should we create a dynamic predicate that more precisely constrains this type?
   subtype Callsign_Type is String(1 .. 6);
     --with Dynamic_Predicate =>
     --  (for all I in Callsign_Type'Range => Callsign_Type(I) in Callsign_Character);

end JT65Strings;


--------------------------------------------------------------------------------
-- FILE   : jt65string.ads
-- SUBJECT: pecification
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

package JT65String is

   subtype JT65_Character is Character
     with Static_Predicate =>
       (JT65_Character in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | ' ' | '+' | '-' | '.' | '/' | '?');

   subtype JT65_String is String
     with Dynamic_Predicate =>
       (for all I in JT65_String'Range =>
            JT65_String(I) in JT65_Character) and then
       (JT65_String'First = 1 and JT65_String'Last >= 2 and JT65_String'Last <= 22);

   subtype JT65_String_Unconstrained is String
     with Dynamic_Predicate =>
       (for all I in JT65_String_Unconstrained'Range =>
          JT65_String_Unconstrained(I) in JT65_Character);

end JT65String;

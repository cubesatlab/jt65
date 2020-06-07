--------------------------------------------------------------------------------
-- FILE   : jt65code.ads
-- SUBJECT: Specification of a package for ...
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(ON);

with Wrapkarn; use Wrapkarn;
with Text_IO;

procedure JT65Code
  with Global =>  (In_Out => (Text_IO.File_System, Reed_S, First));

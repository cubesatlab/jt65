--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into JT65Strings.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body JT65Strings.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Is_Valid_Callsign (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Callsign_57d08f (Gnattest_T : in out Test) renames Test_Is_Valid_Callsign;
--  id:2.2/57d08fdb5ec5475e/Is_Valid_Callsign/1/0/
   procedure Test_Is_Valid_Callsign (Gnattest_T : in out Test) is
   --  jt65strings.ads:40:4:Is_Valid_Callsign
--  end read only

      pragma Unreferenced (Gnattest_T);

      type Test_Case is
         record
            Callsign        : Callsign_Type;
            Expected_Result : Boolean;
         end record;

      -- From the JT65 paper, "The JT65 Communications Protocol" by Joe Taylor.
      --
      -- "An amateur callsign consists of a one- or two-character prefix, at least one of which must be a
      -- letter, followed by a digit and a suffix of one to three letters."
      --
      -- Note that Is_Valid_Callsign expects the digit between the prefix and the suffix to always be at
      -- position #3 in the string.

      Test_Cases : constant array (1 .. 11) of Test_Case :=
         -- Start with valid callsigns.
        ( 1 => (Callsign => " K1JT ", Expected_Result => True),
          2 => (Callsign => "AA1A  ", Expected_Result => True),
          3 => (Callsign => "AA1AA ", Expected_Result => True),
          4 => (Callsign => "AA1AAA", Expected_Result => True),
          5 => (Callsign => "A11A  ", Expected_Result => True),
          6 => (Callsign => "1A1A  ", Expected_Result => True),

          -- Now check some invalid callsigns.
          7 => (Callsign => "AAXAAA", Expected_Result => False),   -- Non-digit at position 3.
          8 => (Callsign => "111AAA", Expected_Result => False),   -- No letters in prefix.
          9 => (Callsign => "AA11  ", Expected_Result => False),   -- Digits in suffix.
         10 => (Callsign => "K 1JT ", Expected_Result => False),   -- Space in prefix.
         11 => (Callsign => " K1J T", Expected_Result => False));  -- Space in suffix.

   begin

      for I in Test_Cases'Range loop
         AUnit.Assertions.Assert
           (Is_Valid_Callsign(Test_Cases(I).Callsign) = Test_Cases(I).Expected_Result,
            "Test case #" & Integer'Image(I) & " failed.");
      end loop;

--  begin read only
   end Test_Is_Valid_Callsign;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end JT65Strings.Test_Data.Tests;

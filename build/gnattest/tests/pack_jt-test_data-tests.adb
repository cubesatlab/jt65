--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pack_JT.Test_Data.

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
package body Pack_JT.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Pack_Bits (Gnattest_T : in out Test);
   procedure Test_Pack_Bits_8c6c18 (Gnattest_T : in out Test) renames Test_Pack_Bits;
--  id:2.2/8c6c18e13d37d22c/Pack_Bits/1/0/
   procedure Test_Pack_Bits (Gnattest_T : in out Test) is
   --  pack_jt.ads:25:4:Pack_Bits
--  end read only

      pragma Unreferenced (Gnattest_T);

      type Test_Case is
         record
            Input_Bits          : Unsigned_8_Array(1 .. 64);
            Word_Count          : Positive;
            Bits_Per_Word       : Bit_Count_Type;
            Expected_Word_Array : Unsigned_32_Array(1 .. 3);
         end record;

      Test_Cases : constant array (1 .. 4) of Test_Case :=
        (1 => (Input_Bits        => (1, others => 0),
               Word_Count        => 1,
               Bits_Per_Word     => 1,
               Expected_Word_Array => (16#0000_0001#, 16#0000_0000#, 16#0000_0000#)),
         2 => (Input_Bits        => (1, 0, 1, 0, 1, 0, others => 0),
               Word_Count        => 2,
               Bits_Per_Word     => 3,
               Expected_Word_Array => (16#0000_0005#, 16#0000_0002#, 16#0000_0000#)),
         3 => (Input_Bits        => (1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, others => 0),
               Word_Count        => 3,
               Bits_Per_Word     => 6,
               Expected_Word_Array => (16#0000_002A#, 16#0000_003F#, 16#0000_0021#)),
         4 => (Input_Bits        => (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
               Word_Count        => 2,
               Bits_Per_Word     => 32,
               Expected_Word_Array => (16#FFFF_FFFF#, 16#8000_0001#, 16#0000_0000#)));

      Result : Unsigned_32_Array(1 .. 3);
   begin
      for I in Test_Cases'Range loop
         Result := (others => 0);
         Pack_Bits
           (Test_Cases(I).Input_Bits,
            Test_Cases(I).Word_Count,
            Test_Cases(I).Bits_Per_Word,
            Result);
         AUnit.Assertions.Assert
           (Result = Test_Cases(I).Expected_Word_Array,
            "Test case #" & Integer'Image(I) & " failed.");
      end loop;

--  begin read only
   end Test_Pack_Bits;
--  end read only


--  begin read only
   procedure Test_Unpack_Bits (Gnattest_T : in out Test);
   procedure Test_Unpack_Bits_c7c976 (Gnattest_T : in out Test) renames Test_Unpack_Bits;
--  id:2.2/c7c976ee6da80048/Unpack_Bits/1/0/
   procedure Test_Unpack_Bits (Gnattest_T : in out Test) is
   --  pack_jt.ads:46:4:Unpack_Bits
--  end read only

      pragma Unreferenced (Gnattest_T);

      type Test_Case is
         record
            Word_Array         : Unsigned_32_Array(1 .. 3);
            Word_Count         : Positive;
            Bits_Per_Word      : Bit_Count_Type;
            Expected_Bit_Array : Unsigned_8_Array(1 .. 64);
         end record;

      Test_Cases : constant array (1 .. 4) of Test_Case :=
        (1 => (Word_Array         => (16#0000_0001#, 16#0000_0000#, 16#0000_0000#),
               Word_Count         => 1,
               Bits_Per_Word      => 1,
               Expected_Bit_Array => (1, others => 0)),
         2 => (Word_Array         => (16#0000_0005#, 16#0000_0002#, 16#0000_0000#),
               Word_Count         => 2,
               Bits_Per_Word      => 3,
               Expected_Bit_Array => (1, 0, 1, 0, 1, 0, others => 0)),
         3 => (Word_Array         => (16#0000_002A#, 16#0000_003F#, 16#0000_0021#),
               Word_Count         => 3,
               Bits_Per_Word      => 6,
               Expected_Bit_Array => (1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, others => 0)),
         4 => (Word_Array         => (16#FFFF_FFFF#, 16#8000_0001#, 16#0000_0000#),
               Word_Count         => 2,
               Bits_Per_Word      => 32,
               Expected_Bit_Array => (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)));

      Result : Unsigned_8_Array(1 .. 64);
   begin
      for I in Test_Cases'Range loop
         Result := (others => 0);
         Unpack_Bits
           (Test_Cases(I).Word_Array,
            Test_Cases(I).Word_Count,
            Test_Cases(I).Bits_Per_Word,
            Result);
         AUnit.Assertions.Assert
           (Result = Test_Cases(I).Expected_Bit_Array,
            "Test case #" & Integer'Image(I) & " failed.");
      end loop;

--  begin read only
   end Test_Unpack_Bits;
--  end read only


--  begin read only
   procedure Test_Pack_Callsign (Gnattest_T : in out Test);
   procedure Test_Pack_Callsign_034fe7 (Gnattest_T : in out Test) renames Test_Pack_Callsign;
--  id:2.2/034fe7c794c60d47/Pack_Callsign/1/0/
   procedure Test_Pack_Callsign (Gnattest_T : in out Test) is
   --  pack_jt.ads:71:4:Pack_Callsign
--  end read only

      pragma Unreferenced (Gnattest_T);

      type Test_Case is
         record
            Callsign          : Callsign_Type;  -- This gets updated by Pack_Callsign.
            Expected_Callsign : Callsign_Type;
            Expected_Encoded_Callsign : Unsigned_32;
            Expected_Text     : Boolean;
         end record;

      Test_Cases : array (1 .. 9) of Test_Case :=
        (1 => (Callsign          => "K1JT  ",
               Expected_Callsign => "K1JT  ",
               Expected_Encoded_Callsign => 259_055_063,
               Expected_Text     => False),
         2 => (Callsign          => "3DA0XY",   -- The Swaziland work-around.
               Expected_Callsign => "3D0XY ",
               Expected_Encoded_Callsign =>  23_833_871,
               Expected_Text     => False),
         3 => (Callsign          => "3XA1XY",   -- The Guinea prefix work-around.
               Expected_Callsign => "QA1XY ",
               Expected_Encoded_Callsign => 186_238_304,
               Expected_Text     => False),
         4 => (Callsign          => "CQ 123",   -- Special case #1
               Expected_Callsign => "CQ 123",
               Expected_Encoded_Callsign => 262_177_686,
               Expected_Text     => False),
         5 => (Callsign          => "QRZ   ",   -- Special case #2
               Expected_Callsign => "QRZ   ",
               Expected_Encoded_Callsign => 262_177_562,
               Expected_Text     => False),
         6 => (Callsign          => "DE    ",   -- Special case #3
               Expected_Callsign => "DE    ",
               Expected_Encoded_Callsign => 267_796_945,
               Expected_Text     => False),
         7 => (Callsign          => "K1XYZZ",  -- Not a valid callsign (too many suffix characters).
               Expected_Callsign => "K1XYZZ",
               Expected_Encoded_Callsign => 0,
               Expected_Text     => True),
         8 => (Callsign          => "ABCDEF",  -- Not a valid callsign (no digit between prefix and suffix).
               Expected_Callsign => "ABCDEF",
               Expected_Encoded_Callsign => 0,
               Expected_Text     => True),
         9 => (Callsign          => "K1JT1 ",  -- Not a valid callsign (fails Is_Valid_Callsign).
               Expected_Callsign => "K1JT1 ",
               Expected_Encoded_Callsign => 0,
               Expected_Text     => True));

      Result_Encoded_Callsign : Unsigned_32;
      Result_Text : Boolean;

   begin
      for I in Test_Cases'Range loop
         Pack_Callsign(Test_Cases(I).Callsign, Result_Encoded_Callsign, Result_Text);
         AUnit.Assertions.Assert
           (Test_Cases(I).Callsign = Test_Cases(I).Expected_Callsign and
                Result_Encoded_Callsign = Test_Cases(I).Expected_Encoded_Callsign and
                Result_Text = Test_Cases(I).Expected_Text,
            "Test case #" & Integer'Image(I) & " failed.");
      end loop;

--  begin read only
   end Test_Pack_Callsign;
--  end read only


--  begin read only
   procedure Test_Unpack_Call (Gnattest_T : in out Test);
   procedure Test_Unpack_Call_0c391b (Gnattest_T : in out Test) renames Test_Unpack_Call;
--  id:2.2/0c391b142d8f0c7a/Unpack_Call/1/0/
   procedure Test_Unpack_Call (Gnattest_T : in out Test) is
   --  pack_jt.ads:87:4:Unpack_Call
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Unpack_Call;
--  end read only


--  begin read only
   procedure Test_Pack_Grid (Gnattest_T : in out Test);
   procedure Test_Pack_Grid_0d328e (Gnattest_T : in out Test) renames Test_Pack_Grid;
--  id:2.2/0d328e5d38c95920/Pack_Grid/1/0/
   procedure Test_Pack_Grid (Gnattest_T : in out Test) is
   --  pack_jt.ads:99:4:Pack_Grid
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Pack_Grid;
--  end read only


--  begin read only
   procedure Test_Unpack_Grid (Gnattest_T : in out Test);
   procedure Test_Unpack_Grid_de94a3 (Gnattest_T : in out Test) renames Test_Unpack_Grid;
--  id:2.2/de94a345f01720cd/Unpack_Grid/1/0/
   procedure Test_Unpack_Grid (Gnattest_T : in out Test) is
   --  pack_jt.ads:104:4:Unpack_Grid
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Unpack_Grid;
--  end read only


--  begin read only
   procedure Test_Pack_Msg (Gnattest_T : in out Test);
   procedure Test_Pack_Msg_ccaba2 (Gnattest_T : in out Test) renames Test_Pack_Msg;
--  id:2.2/ccaba24bae0a0ca0/Pack_Msg/1/0/
   procedure Test_Pack_Msg (Gnattest_T : in out Test) is
   --  pack_jt.ads:120:4:Pack_Msg
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Pack_Msg;
--  end read only


--  begin read only
   procedure Test_Unpack_Msg (Gnattest_T : in out Test);
   procedure Test_Unpack_Msg_5cac61 (Gnattest_T : in out Test) renames Test_Unpack_Msg;
--  id:2.2/5cac61507b8160a7/Unpack_Msg/1/0/
   procedure Test_Unpack_Msg (Gnattest_T : in out Test) is
   --  pack_jt.ads:125:4:Unpack_Msg
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Unpack_Msg;
--  end read only


--  begin read only
   procedure Test_Pack_Text (Gnattest_T : in out Test);
   procedure Test_Pack_Text_fb6fee (Gnattest_T : in out Test) renames Test_Pack_Text;
--  id:2.2/fb6feeb2e3d6fab1/Pack_Text/1/0/
   procedure Test_Pack_Text (Gnattest_T : in out Test) is
   --  pack_jt.ads:130:4:Pack_Text
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Pack_Text;
--  end read only


--  begin read only
   procedure Test_Unpack_Text (Gnattest_T : in out Test);
   procedure Test_Unpack_Text_8de74f (Gnattest_T : in out Test) renames Test_Unpack_Text;
--  id:2.2/8de74fc5e0073739/Unpack_Text/1/0/
   procedure Test_Unpack_Text (Gnattest_T : in out Test) is
   --  pack_jt.ads:135:4:Unpack_Text
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Unpack_Text;
--  end read only


--  begin read only
   procedure Test_Get_Pfx1 (Gnattest_T : in out Test);
   procedure Test_Get_Pfx1_1d2d30 (Gnattest_T : in out Test) renames Test_Get_Pfx1;
--  id:2.2/1d2d3015bf13b86a/Get_Pfx1/1/0/
   procedure Test_Get_Pfx1 (Gnattest_T : in out Test) is
   --  pack_jt.ads:148:4:Get_Pfx1
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Pfx1;
--  end read only


--  begin read only
   procedure Test_Get_Pfx2 (Gnattest_T : in out Test);
   procedure Test_Get_Pfx2_011817 (Gnattest_T : in out Test) renames Test_Get_Pfx2;
--  id:2.2/0118171201c51ec4/Get_Pfx2/1/0/
   procedure Test_Get_Pfx2 (Gnattest_T : in out Test) is
   --  pack_jt.ads:154:4:Get_Pfx2
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Pfx2;
--  end read only


--  begin read only
   procedure Test_Grid2K (Gnattest_T : in out Test);
   procedure Test_Grid2K_b47633 (Gnattest_T : in out Test) renames Test_Grid2K;
--  id:2.2/b47633f6dcc44ed7/Grid2K/1/0/
   procedure Test_Grid2K (Gnattest_T : in out Test) is
   --  pack_jt.ads:159:4:Grid2K
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Grid2K;
--  end read only


--  begin read only
   procedure Test_K2Grid (Gnattest_T : in out Test);
   procedure Test_K2Grid_c56dd1 (Gnattest_T : in out Test) renames Test_K2Grid;
--  id:2.2/c56dd11153a3d44f/K2Grid/1/0/
   procedure Test_K2Grid (Gnattest_T : in out Test) is
   --  pack_jt.ads:164:4:K2Grid
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_K2Grid;
--  end read only


--  begin read only
   procedure Test_NChar (Gnattest_T : in out Test);
   procedure Test_NChar_5b447d (Gnattest_T : in out Test) renames Test_NChar;
--  id:2.2/5b447dce3da763d0/NChar/1/0/
   procedure Test_NChar (Gnattest_T : in out Test) is
   --  pack_jt.ads:184:4:NChar
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_NChar;
--  end read only


--  begin read only
   procedure Test_Grid2Deg (Gnattest_T : in out Test);
   procedure Test_Grid2Deg_c355c5 (Gnattest_T : in out Test) renames Test_Grid2Deg;
--  id:2.2/c355c590c29a9e3d/Grid2Deg/1/0/
   procedure Test_Grid2Deg (Gnattest_T : in out Test) is
   --  pack_jt.ads:204:4:Grid2Deg
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Grid2Deg;
--  end read only


--  begin read only
   procedure Test_Deg2Grid (Gnattest_T : in out Test);
   procedure Test_Deg2Grid_4680f7 (Gnattest_T : in out Test) renames Test_Deg2Grid;
--  id:2.2/4680f7dcf5174384/Deg2Grid/1/0/
   procedure Test_Deg2Grid (Gnattest_T : in out Test) is
   --  pack_jt.ads:211:4:Deg2Grid
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Deg2Grid;
--  end read only


--  begin read only
   procedure Test_Collapse_Blanks_12 (Gnattest_T : in out Test);
   procedure Test_Collapse_Blanks_12_47d1b9 (Gnattest_T : in out Test) renames Test_Collapse_Blanks_12;
--  id:2.2/47d1b9df3a124cb0/Collapse_Blanks_12/1/0/
   procedure Test_Collapse_Blanks_12 (Gnattest_T : in out Test) is
   --  pack_jt.ads:218:4:Collapse_Blanks_12
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Collapse_Blanks_12;
--  end read only


--  begin read only
   procedure Test_Get_Index (Gnattest_T : in out Test);
   procedure Test_Get_Index_952be7 (Gnattest_T : in out Test) renames Test_Get_Index;
--  id:2.2/952be72850f93d94/Get_Index/1/0/
   procedure Test_Get_Index (Gnattest_T : in out Test) is
   --  pack_jt.ads:223:4:Get_Index
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Index;
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
end Pack_JT.Test_Data.Tests;

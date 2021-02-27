--------------------------------------------------------------------------------
-- FILE   : pack_jt.ads
-- SUBJECT: Specification of a package for [DESCRIBE ME!]
-- AUTHOR : (C) Copyright 2021 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);

with Interfaces;     use Interfaces;
with Unsigned_Array; use Unsigned_Array;

package Pack_JT is

   type Octet is new Unsigned_8;
   type Octet_Array is array(Positive range <>) of Octet;

   add_pfx : String(1 .. 8);

   -- Pack 0s and 1s from DBits into Sym with M0 bits per word.
   -- NB: NSymd is the number of packed output words.
   -- No use of Pack_Bits, unknown expected output.
   procedure Pack_Bits
     (DBits : in     Unsigned_8_Array;
      NSymd : in     Integer;
      M0    : in     Integer;
      Sym   :    out Unsigned_8_Array)
     with
       Global => null,
       Pre =>
         NSymd >= 1 and NSymd <= 255 and
         M0    >= 1 and M0    <= 255 and DBits'Length >= 1 and Sym'Length >= 1;

   -- Unpack bits from Sym into DBits, one bit per byte.
   -- NB: NSymd is the number of input words, and M0 their length.
   -- there will be M0*NSymd output bytes, each 0 or 1.
   -- No use of Unpack_Bits, unknown expected output.
   procedure Unpack_Bits
     (Sym   : in     Unsigned_8_Array;
      NSymd : in     Integer;
      M0    : in     Integer;
      DBits :    out Unsigned_8_Array)
     with
       Global => null,
       Pre =>
         NSymd >= 1 and NSymd <= 255 and
         M0    >= 1 and M0    <= 255 and Sym'Length >= 1 and DBits'Length >= 1;

   -- Pack a valid callsign into a 28-bit integer.
   procedure Pack_Call
     (Call  : in out String;
      NCall : in out Unsigned_32;
      Text  :    out Boolean)
     with
       Global => null,
       Pre  => (Call'First = 1 and Call'Last = 6);

   procedure Unpack_Call
     (NCall : in     Unsigned_32;
      Word  :    out String;
      Iv2   :    out Integer;
      Psfx  :    out String)
     with
       Global => null,
       Pre =>
         NCall <= 2147483647 and
         Word'First = 1 and Word'Last = 12 and
         Psfx'First = 1 and Psfx'Last =  4;

   procedure Pack_Grid(Grid : in out String; NG : in out Unsigned_32; Text : out Boolean)
     with
       Global => null,
       Pre => (Grid'First = 1 and Grid'Last = 4);

   procedure Unpack_Grid(Ng : Integer; Grid : out String)
     with
       Global => null,
       Pre => (Grid'First = 1 and Grid'Last = 4);

   -- Packs a JT4/JT9/JT65 message into twelve 6-bit symbols
   --
   -- itype Message Type
   ----------------------
   --   1   Standardd message
   --   2   Type 1 prefix
   --   3   Type 1 suffix
   --   4   Type 2 prefix
   --   5   Type 2 suffix
   --   6   Free text
   --  -1   Does not decode correctly
   procedure Pack_Msg(Msg0 : String; Dat : out Unsigned_8_Array; IType : out Integer)
     with
       Global => add_pfx,
       Pre => Dat'First = 0 and Dat'Last = 11;

   procedure Unpack_Msg(Dat0 : Unsigned_8_Array; Msg : out String)
     with
       Global => add_pfx,
       Pre => (Dat0'First = 0 and Dat0'Last = 11 and Msg'First = 1 and Msg'Last = 22);

   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : out Unsigned_32)
     with
       Global => null,
       Pre => Msg'First = 1 and Msg'Last = 22;

   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Unsigned_32; Msg : in out String)
     with
       Global => null,
       Pre => Msg'First = 1 and Msg'Last = 22;

   -- Formats a message by converting all letters to upper case
   -- Fmtmsg can be removed
   --  procedure Fmtmsg(Msg : in out String)
   --    with
   --      Pre => Msg'First >= 1 and Msg'Last <= 22;
   --
private

   subtype Numeric_Callsign_Type is Natural range 0 .. 36;
   subtype Callsign_Type is Character
     with Static_Predicate => (Callsign_Type in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | ' ');

   type Callsign_String is array (Integer range <>) of Callsign_Type;

   procedure Get_Pfx1(Callsign : in out String; K : out Integer; Nv2 : out Integer)
     with
       Global => (Input => add_pfx),
       Pre => Callsign'First = 1 and Callsign'Last = 12,
       Post => Nv2 >= 0 and Nv2 <= 10 and K >= -1;

   procedure Get_Pfx2(K0 : Integer; Callsign : in out String)
     with
       Global => (Input => add_pfx),
       Pre => Callsign'First = 1 and Callsign'last = 12;

   procedure Grid2K(Grid : String; K : out Integer)
     with
       Global => null,
       Pre => Grid'First = 1 and Grid'Last = 6;

   procedure K2Grid(K : Integer; Grid : out String)
     with
       Global => null,
       Pre => Grid'First = 1 and Grid'Last = 6 and K >= -1;

   --  Grid2N - unused
   --  procedure Grid2N(Grid : String; N : out Integer)
   --    with
   --      Global => null,
   --      Pre => Grid'Length = 4;

   --  N2Grid is currently not used
   --  procedure N2Grid(N : Integer; Grid : in out String)
   --    with
   --      Global => null,
   --      Pre => Grid'Length = 4 and N in -70 .. -31;

   -- Convert ASCII number, letter, or space to 0-36 for callsign packing.
   function NChar(C : Character) return Numeric_Callsign_Type
     with Global => null;

   --  Pack50 - unused
   --  procedure Pack50(N1, N2 : Unsigned_32; Dat : out Unsigned_32_Array)
   --    with
   --      Global => null,
   --      Pre => Dat'Length = 11;

   --  Pack_Pfx is currently not used
   --  procedure Pack_Pfx(Call1 : String; N1 : in out Unsigned_32; Ng : in out Integer; Nadd : out Integer)
   --    with
   --      Global => null,
   --      Pre => Call1'Length = 12;

   -- Converts Maidenhead grid locator to degrees of West longitude and North latitude
   procedure Grid2Deg(Grid0 : String; DLong : out Float; DLat : out Float)
     with
       Global => null,
       Pre => Grid0'First = 1 and Grid0'Last = 6,
       Post =>
         DLong >= -2000.0 and DLong <= 2000.0 and DLat >= -1000.0 and DLat <= 1000.0;

   procedure Deg2Grid(DLong0 : Float; DLat : Float; Grid : out String)
     with
       Global => null,
       Pre =>
         Grid'First = 1 and Grid'Last = 6 and
         DLong0 <= 2000.0 and DLat >= -1000.0 and DLat <= 1000.0;

   procedure Collapse_Blanks_12(Word : in out String)
     with
       Pre => Word'First = 1 and Word'Last = 12;

   subtype Valid_Index is Integer range 0 .. 12;
   function Get_Index(JT65_String : in String; Pattern : in String) return Valid_Index
     with
       Global => null,
       Pre => Pattern'Length = 1;

end Pack_JT;


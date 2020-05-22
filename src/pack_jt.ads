with Interfaces; use Interfaces;
--with General_JT; use General_JT;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with pfx; use pfx;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package Pack_JT is

   Not_Implemented : exception;
   Invalid_Callsign : exception;

   type Octet is new Unsigned_8;
   type Octet_Array is array(Positive range <>) of Octet;
   type Integer_Array is array(Positive range <>) of Integer;

   -- Pack 0s and 1s from DBits into Sym with M0 bits per word.
   -- NB: NSymd is the number of packed output words.
   procedure Pack_Bits(DBits : in out Octet_Array; NSymd : in out Integer; M0 : in out Integer; Sym : in out Integer_Array);

   -- Unpack bits from Sym into DBits, one bit per byte.
   -- NB: NSymd is the number of input words, and M0 their length.
   -- there will be M0*NSymd output bytes, each 0 or 1.
   procedure Unpack_Bits(Sym : Integer_Array; NSymd : Integer; M0 : Integer; DBits : Octet_Array);

   -- Pack a valid callsign into a 28-bit integer.
   procedure Pack_Call(Call : in out String; NCall : in out  Unsigned_32; Text : out Boolean);

   procedure Unpack_Call(NCall : Integer; Word : String; Iv2 : Integer; Psfx : String);

   procedure Pack_Grid(Grid : in out String; NG : in out Unsigned_32; Text : in out Boolean);

   procedure Unpack_Grid(Ng : Integer; Grid : String);

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
   procedure Pack_Msg(Msg0 : String; Dat : out Integer_Array; IType : out Integer);

   procedure Unpack_Msg(Dat : Integer_Array; Msg : String);

   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : out Unsigned_32);

   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Integer; Msg : String);

   procedure Get_Pfx1(Callsign : in out String; K : out Integer; Nv2 : in out Integer);

   procedure Get_Pfx2(K0 : Integer; Callsign : String);

   procedure Grid2k(Grid : String; K : Integer);

   procedure K2Grid(K : Integer; Grid : out String);

   procedure Grid2N(Grid : String; N : Integer);

   procedure N2Grid(N : Integer; Gride : String);

   -- Convert ASCII number, letter, or space to 0-36 for callsign packing.
   function NChar(C : Character) return Integer;

   procedure Pack50(N1, N2 : Integer; Dat : Integer_Array);

   procedure Pack_Pfx(Call1 : String; N1 : Integer; Ng : Integer; Nadd : Integer);

   -- Converts Maidenhead grid locator to degrees of West longitude and North latitude
   procedure Grid2Deg(Grid0 : String; DLong : out Float; DLat : out Float);

   -- Formats a message by converting all letters to upper case and
   -- collapsing multiple blanks into one
   procedure Fmtmsg( Msg : in out String )
     with
       Pre => Msg'Length >= 1 and Msg'Length <= 22;

end Pack_JT;

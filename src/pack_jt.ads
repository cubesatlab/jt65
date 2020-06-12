
pragma SPARK_Mode(On);

with Interfaces; use Interfaces;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with pfx; use pfx;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Unsigned_Array; use Unsigned_Array;

package Pack_JT is

   Not_Implemented : exception;
   Invalid_Callsign : exception;
   N2Grid_Error : exception;
   Array_Out_Of_Bounds : exception;

   type Octet is new Unsigned_8;
   type Octet_Array is array(Positive range <>) of Octet;


   -- Pack 0s and 1s from DBits into Sym with M0 bits per word.
   -- NB: NSymd is the number of packed output words.
   procedure Pack_Bits(DBits : Octet_Array; NSymd : Integer; M0 : Integer; Sym : out Unsigned_32_Array);

   -- Unpack bits from Sym into DBits, one bit per byte.
   -- NB: NSymd is the number of input words, and M0 their length.
   -- there will be M0*NSymd output bytes, each 0 or 1.
   procedure Unpack_Bits(Sym : Unsigned_32_Array; NSymd : Integer; M0 : Integer; DBits : out Octet_Array);

   -- Pack a valid callsign into a 28-bit integer.
   procedure Pack_Call(Call : in out String; NCall : in out  Unsigned_32; Text : out Boolean)
     with Pre => ((Call'Length = 6 and Call'First = 1 and Call'Last = 6) or else raise Array_Out_Of_Bounds);

   procedure Unpack_Call(NCall : Unsigned_32; Word : out String; Iv2 : out Integer; Psfx : out String)
     with Pre => ((Word'Length = 12) and then (Psfx'Length = 4)) or else raise Array_Out_Of_Bounds;

   procedure Pack_Grid(Grid : in out String; NG : in out Unsigned_32; Text : out Boolean)
     with Pre => (Grid'Length = 4 and Grid'First = 1 and Grid'Last = 4) or else raise Array_Out_Of_Bounds;

   procedure Unpack_Grid(Ng : Integer; Grid : out String)
     with Pre => (Grid'Length = 4 and Grid'First = 1 and Grid'Last = 4) or else raise Array_Out_Of_Bounds;

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
   procedure Pack_Msg(Msg0 : String; Dat : out Unsigned_32_Array; IType : out Integer);

   procedure Unpack_Msg(Dat : Unsigned_32_Array; Msg : out String)
     with Pre => Msg'Length = 22 or else raise Array_Out_Of_Bounds;

   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : out Unsigned_32);

   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Unsigned_32; Msg : in out String);

   procedure Get_Pfx1(Callsign : in out String; K : out Integer; Nv2 : in out Integer);

   procedure Get_Pfx2(K0 : Integer; Callsign : in out String);

   procedure Grid2k(Grid : String; K : out Integer);

   procedure K2Grid(K : Integer; Grid : out String);

   procedure Grid2N(Grid : String; N : out Integer);

   procedure N2Grid(N : Integer; Grid : in out String);

   -- Convert ASCII number, letter, or space to 0-36 for callsign packing.
   function NChar(C : Character) return Integer;

   procedure Pack50(N1, N2 : Unsigned_32; Dat : out Unsigned_32_Array);

   procedure Pack_Pfx(Call1 : String; N1 : in out Unsigned_32; Ng : in out Integer; Nadd : out Integer)
     with Pre => Call1'Length = 12 or else raise Array_Out_Of_Bounds;

   -- Converts Maidenhead grid locator to degrees of West longitude and North latitude
   procedure Grid2Deg(Grid0 : String; DLong : out Float; DLat : out Float);

   procedure Deg2Grid(DLong0 : Float; DLat : Float; Grid : out String);

   -- Formats a message by converting all letters to upper case and
   -- collapsing multiple blanks into one
   procedure Fmtmsg( Msg : in out String )
     with
       Pre => Msg'Length >= 1 and Msg'Length <= 22;

end Pack_JT;

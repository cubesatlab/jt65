
package Pack_JT is

   Not_Implemented : exception;

   type Octet is mod 8;
   type Octet_Array is array(Positive range <>) of Octet;
   type Integer_Array is array(Positive range <>) of Integer;

   -- Pack 0s and 1s from DBits into Sym with M0 bits per word.
   -- NB: NSymd is the number of packed output words.
   procedure Pack_Bits(DBits : Octet_Array; NSymd : Integer; M0 : Integer; Sym : Integer_Array);

   -- Unpack bits from Sym into DBits, one bit per byte.
   -- NB: NSymd is the number of input words, and M0 their length.
   -- there will be M0*NSymd output bytes, each 0 or 1.
   procedure Unpack_Bits(Sym : Integer_Array; NSymd : Integer; M0 : Integer; DBits : Octet_Array);

   -- Pack a valid callsign into a 28-bit integer.
   procedure Pack_Call(Call : String; NCall : Integer; Text : Boolean);

   procedure Unpack_Call(NCall : Integer; Word : String; Iv2 : Integer; Psfx : String);

   procedure Pack_Grid(Grid : String; C1 : Character; Text : Boolean);

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
   procedure Pack_Msg(Msg0 : String; Dat : Integer_Array; IType : Integer);

   procedure Unpack_Msg(Dat : Integer_Array; Msg : String);

   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : Integer);

   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Integer; Msg : String);

   procedure Get_Pfx1(Callsign : String; K : Integer; Nv2 : Integer);

   procedure Get_Pfx2(K0 : Integer; Callsign : String);

   procedure Grid2k(Grid : String; K : Integer);

   procedure K2Grid(K : Integer; Grid : String);

   procedure Grid2N(Grid : String; N : Integer);

   procedure N2Grid(N : Integer; Gride : String);

   -- Convert ASCII number, letter, or space to 0-36 for callsign packing.
   function NChar(C : Character) return Integer;

   procedure Pack50(N1, N2 : Integer; Dat : Integer_Array);

   procedure Pack_Pfx(Call1 : String; N1 : Integer; Ng : Integer; Nadd : Integer);

end Pack_JT;


pragma SPARK_Mode(On);

with Interfaces; use Interfaces;
with Unsigned_Array; use Unsigned_Array;

package Pack_JT is

   type Octet is new Unsigned_8;
   type Octet_Array is array(Positive range <>) of Octet;

     add_pfx : String(1 .. 8);

   -- Pack 0s and 1s from DBits into Sym with M0 bits per word.
   -- NB: NSymd is the number of packed output words.
   procedure Pack_Bits
     (DBits : Unsigned_8_Array; NSymd : Integer; M0 : Integer; Sym : out Unsigned_8_Array)
     with
       Global => null,
       Pre => NSymd >= 1 and NSymd <= 255 and M0 >= 1 and M0 <= 255 and DBits'Length >= 1 and Sym'Length >= 1;



   -- Unpack bits from Sym into DBits, one bit per byte.
   -- NB: NSymd is the number of input words, and M0 their length.
   -- there will be M0*NSymd output bytes, each 0 or 1.
   procedure Unpack_Bits
     (Sym : Unsigned_8_Array; NSymd : Integer; M0 : Integer; DBits : out Unsigned_8_Array)
     with
       Global => null,
       Pre => NSymd >= 1 and NSymd <= 255 and M0 >= 1 and M0 <= 255 and Sym'Length >= 1 and DBits'Length >= 1;

   -- Pack a valid callsign into a 28-bit integer.
   procedure Pack_Call
     (Call : in out String; NCall : in out  Unsigned_32; Text : out Boolean)
     with
       Global => null,
       Pre => Call'Length = 6 and Call'First = 1 and Call'Last = 6;

   procedure Unpack_Call
     (NCall : Unsigned_32; Word : out String; Iv2 : out Integer; Psfx : out String)
     with
       Global => null,
       Pre => NCall <= 2147483647 and then Word'Length = 12 and then Psfx'Length = 4;

   procedure Pack_Grid(Grid : in out String; NG : in out Unsigned_32; Text : out Boolean)
     with
       Global => null,
       Pre => (Grid'Length = 4 and Grid'First = 1 and Grid'Last = 4);

   procedure Unpack_Grid(Ng : Integer; Grid : out String)
     with
       Global => null,
       Pre => (Grid'Length = 4 and Grid'First = 1 and Grid'Last = 4);


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
       Pre => (Dat'Length = 12 and Dat'First = 0 and Dat'Last = 11);

   procedure Unpack_Msg(Dat0 : Unsigned_8_Array; Msg : out String)
     with
       Global => add_pfx,
       Pre => Msg'Length = 22;

   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : out Unsigned_32)
     with
       Global => null,
       Pre => Msg'Length = 22;

   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Unsigned_32; Msg : in out String)
     with
       Global => null,
       Pre => Msg'Length = 22;

   -- Formats a message by converting all letters to upper case
   -- Fmtmsg can be removed
   procedure Fmtmsg( Msg : in out String )
     with
       Pre => Msg'First >= 1 and Msg'Last <= 22 and Msg'Length = 22;

   procedure Collapse_Blanks_12( Word : in out String)
     with
       Pre => Word'First = 1 and Word'Last = 12 and Word'Length = 12;

end Pack_JT;

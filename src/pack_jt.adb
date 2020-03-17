
package body Pack_JT is

   procedure Pack_Bits(DBits : Octet_Array; NSymd : Integer; M0 : Integer; Sym : Integer_Array) is
   begin
      raise Not_Implemented with "Pack_Bits";
   end Pack_Bits;


   procedure Unpack_Bits(Sym : Integer_Array; NSymd : Integer; M0 : Integer; DBits : Octet_Array) is
   begin
      raise Not_Implemented with "Unpack_Bits";
   end Unpack_Bits;


   procedure Pack_Call(Call : String; NCall : Integer; Text : Boolean) is
   begin
      raise Not_Implemented with "Pack_Call";
   end Pack_Call;


   procedure Unpack_Call(NCall : Integer; Word : String; Iv2 : Integer; Psfx : String) is
   begin
      raise Not_Implemented with "Unpack_Call";
   end Unpack_Call;


   procedure Pack_Grid(Grid : String; C1 : Character; Text : Boolean) is
   begin
      raise Not_Implemented with "Pack_Grid";
   end Pack_Grid;


   procedure Unpack_Grid(Ng : Integer; Grid : String) is
   begin
      raise Not_Implemented with "Unpack_Grid";
   end Unpack_Grid;


   procedure Pack_Msg(Msg0 : String; Dat : Integer_Array; IType : Integer) is
   begin
      raise Not_Implemented with "Pack_Msg";
   end Pack_Msg;


   procedure Unpack_Msg(Dat : Integer_Array; Msg : String) is
   begin
      raise Not_Implemented with "Unpack_Msg";
   end Unpack_Msg;


   procedure Pack_Text(Msg : String; Nc1, Nc2, Nc3 : Integer) is
   begin
      raise Not_Implemented with "Pack_Text";
   end Pack_Text;


   procedure Unpack_Text(Nc1a, Nc2a, Nc3a : Integer; Msg : String) is
   begin
      raise Not_Implemented with "Unpack_Text";
   end Unpack_Text;


   procedure Get_Pfx1(Callsign : String; K : Integer; Nv2 : Integer) is
   begin
      raise Not_Implemented with "Get_Pfx1";
   end Get_Pfx1;


   procedure Get_Pfx2(K0 : Integer; Callsign : String) is
   begin
      raise Not_Implemented with "Get_Pfx2";
   end Get_Pfx2;


   procedure Grid2k(Grid : String; K : Integer) is
   begin
      raise Not_Implemented with "Grid2k";
   end Grid2k;


   procedure K2Grid(K : Integer; Grid : String) is
   begin
      raise Not_Implemented with "K2Grid";
   end K2Grid;


   procedure Grid2N(Grid : String; N : Integer) is
   begin
      raise Not_Implemented with "Grid2N";
   end Grid2N;


   procedure N2Grid(N : Integer; Gride : String) is
   begin
      raise Not_Implemented with "N2Grid";
   end N2Grid;


   function NChar(C : Character) return Integer is
   begin
      raise Not_Implemented with "NChar";
      return 0;
   end NChar;


   procedure Pack50(N1, N2 : Integer; Dat : Integer_Array) is
   begin
      raise Not_Implemented with "Pack50";
   end Pack50;


   procedure Pack_Pfx(Call1 : String; N1 : Integer; Ng : Integer; Nadd : Integer) is
   begin
      raise Not_Implemented with "Pack_Pfx";
   end Pack_Pfx;


end Pack_JT;

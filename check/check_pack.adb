
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;
with Unsigned_Array; use Unsigned_Array;

with Pack_JT;     use Pack_JT;

procedure Check_Pack is
   IType : Integer;
   Dat   : Unsigned_8_Array(0 .. 11);
   Test  : constant String := "DE KA1ABC/VE6 -22";
   Decoded_Message : String(1 .. 22);
begin
   Pack_Msg(Test, Dat, IType);
   Put_Line("Message: " & Test);

   for I in Dat'Range loop
      Put(Unsigned_8'Image(Dat(I)) & " ");
   end loop;

   Put_Line(" Type: " & Integer'Image(IType));

   Unpack_Msg(Dat, Decoded_Message);
   Put_Line("Decoded Message: " & Decoded_Message);
end Check_Pack;

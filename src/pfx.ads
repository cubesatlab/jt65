package pfx is

   Maxpfx : constant Integer := 339;
   Maxsfx : constant Integer := 12;
   
   type pfx_array is array (1 .. Maxpfx) of String (1..5);
   type sfx_array is array (1 .. Maxsfx) of Character;
   
   procedure Init_Pfx (Prefixes : out pfx_array; Suffixes : out sfx_array);
end pfx;
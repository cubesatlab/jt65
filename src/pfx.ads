pragma SPARK_Mode(On);

package Pfx is

   Maxpfx : constant Integer := 339;
   Maxsfx : constant Integer := 12;

   type Pfx_Array is array (1 .. Maxpfx) of String (1 .. 5);
   type Sfx_Array is array (1 .. Maxsfx) of Character;

   procedure Init_Pfx(Prefixes : out Pfx_Array; Suffixes : out Sfx_Array);

end pfx;

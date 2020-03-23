with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

package body General_JT is

   function Fmtmsg (Msg : in String) return String is

      Formatted_Msg : String := Msg;
   begin

      for I in Formatted_Msg'Range loop

         if Formatted_Msg(I) = ' ' then
            if I = Formatted_Msg'Length then
               exit;
            else
               if Formatted_Msg(I+1) = ' ' then
                  Formatted_Msg(I) := Character'Val (127);
                  Formatted_Msg := Formatted_Msg;
               end if;
            end if;

         else
            null;
         end if;

      end loop;

      Formatted_Msg := To_Upper(Formatted_Msg);

      return Formatted_Msg;

   end Fmtmsg;

end General_JT;

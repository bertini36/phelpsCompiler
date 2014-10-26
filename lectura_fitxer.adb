with Ada.Text_IO; use Ada.Text_IO;
package body lectura_fitxer is
   procedure llegeixNom(nom: in string) is
      longitud: natural;
   begin
      longitud := nom'length;
      if (longitud <= 4) or (longitud >= 5 and then (nom(longitud-3..longitud) /= ".phe")) then
         raise nom_incorrecte;
      end if;
   end llegeixNom;
end lectura_fitxer;
